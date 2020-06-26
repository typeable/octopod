module DMS (runDMS) where


import           Control.Applicative
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.STM
import           Control.Exception (throwIO, try, Exception)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson (Value (..), encode, toJSON)
import           Data.Coerce
import           Data.Conduit (ConduitT, yield)
import           Data.Int (Int64)
import           Data.Maybe
import           Data.Pool
import           Data.Text (lines, pack, unpack, unwords)
import           Data.Traversable
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Options.Generic
import           PostgreSQL.ErrorCodes (unique_violation)
import           Prelude hiding (lines, unlines, unwords, log)
import           Servant
import           System.Environment (lookupEnv)
import           System.Exit
import           System.Log.FastLogger
import           System.Process.Typed


import           API
import           Common.API
import           DMS.Args
import           DMS.AWS
import           Orphans ()
import           TLS (createTLSOpts)
import           Types


type PgPool = Pool Connection
type AppM   = ReaderT AppState Handler

data AppState = AppState
  { pool             :: PgPool
  , logger           :: TimedFastLogger
  , eventSink        :: TChan WSEvent
  , projectName      :: ProjectName
  , baseDomain       :: Domain
  , namespace        :: Namespace
  , archiveRetention :: ArchiveRetention
  , creationCommand  :: Command
  , updateCommand    :: Command
  , deletionCommand  :: Command
  , checkingCommand  :: Command
  , cleanupCommand   :: Command }

data DeploymentException
  = DeploymentFailed Int
  | ThereIsAcriveDeployment
  deriving (Show)

instance Exception DeploymentException

data DeploymentListType
  = AllDeployments
  | ArchivedOnlyDeployments
  deriving (Show)

runDMS :: IO ()
runDMS = do
  timeCache <- newTimeCache "%Y-%m-%d %T%z"
  (logger', _) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
  logInfo logger' "started"
  opts <- parseArgs
  let
    a ?! e = a >>= maybe (die e) pure
    getEnvOrDie eName = lookupEnv eName ?! (eName <> " is not set")
  projName <- coerce . pack <$> getEnvOrDie "PROJECT_NAME"
  domain <- coerce . pack <$> getEnvOrDie "BASE_DOMAIN"
  ns <- coerce . pack <$> getEnvOrDie "NAMESPACE"
  archRetention <- coerce . read @Int <$> getEnvOrDie "ARCHIVE_RETENTION"
  creationCmd <- coerce . pack <$> getEnvOrDie "CREATION_COMMAND"
  updateCmd <- coerce . pack <$> getEnvOrDie "UPDATE_COMMAND"
  deletionCmd <- coerce . pack <$> getEnvOrDie "DELETION_COMMAND"
  checkingCmd <- coerce . pack <$> getEnvOrDie "CHECKING_COMMAND"
  cleanupCmd <- coerce . pack <$> getEnvOrDie "CLEANUP_COMMAND"
  pgPool <- initConnectionPool
    (unDBConnectionString $ dmsDB opts) (unDBPoolSize $ dmsDBPoolSize opts)
  channel <- liftIO . atomically $ newBroadcastTChan
  let
    app'         =
      app $ AppState
        pgPool
        logger'
        channel
        projName
        domain
        ns
        archRetention
        creationCmd
        updateCmd
        deletionCmd
        checkingCmd
        cleanupCmd
    wsApp'       = wsApp channel
    serverPort   = dmsPort opts
    uiServerPort = unServerPort $ dmsUIPort opts
    wsServerPort = unServerPort $ dmsWSPort opts
    tlsOpts      =
      createTLSOpts (dmsTLSCertPath opts) (dmsTLSKeyPath opts)
        (dmsTLSStorePath opts) serverPort
    warpOpts     = setPort (unServerPort serverPort) defaultSettings
    in
      (run uiServerPort app')
      `race_` (runTLS tlsOpts warpOpts app')
      `race_` (run wsServerPort wsApp')

initConnectionPool :: ByteString -> Int -> IO PgPool
initConnectionPool dbConnStr =
  createPool (connectPostgreSQL dbConnStr) close 1 30

nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s

app :: AppState -> Application
app s = serve api $ hoistServer api (nt s) server
  where
    api = Proxy @API

wsApp :: TChan WSEvent -> Application
wsApp channel = serve api $ wsServer channel
  where
    api = Proxy @WebSocketAPI

wsServer :: TChan WSEvent -> Server WebSocketAPI
wsServer = eventS

eventS :: MonadIO m => TChan WSEvent -> ConduitT () Value m ()
eventS channel = do
  dupChannel <- liftIO . atomically $ dupTChan channel
  forever $ do
    event <- liftIO . atomically . readTChan $ dupChannel
    yield . toJSON $ event

server :: ServerT API AppM
server =
  ( listH :<|> createH :<|> getH :<|> editH
    :<|> deleteH :<|> updateH :<|> infoH :<|> statusH
    :<|> cleanupH :<|> restoreH
  ) :<|> pingH :<|> cleanArchiveH :<|> projectNameH

listH :: AppM [DeploymentFullInfo]
listH = do
  AppState {pool = p, logger = l, baseDomain = d} <- ask

  retrieved <- liftIO $ withResource p $ \conn -> query_ conn q
  deployments <- liftIO $ for retrieved $ \(n, t, e, a, ct, ut) -> do
    es <- parseEnvs (lines e)
    pure $ DeploymentFullInfo (Deployment n t es) a ct ut (depUrls d n)

  liftIO . logInfo l $ "get deployments: " <> (pack . show $ deployments)
  return deployments
  where
    q                    =
      "SELECT name, tag, envs, archived, extract(epoch from created_at)::int, \
        \extract(epoch from updated_at)::int \
      \FROM deployments ORDER BY name"
    depUrls domain dName =
      [ ("app", appUrl domain dName)
      , ("kibana", "kibana." <> appUrl domain dName)
      , ("tasker", "tasker." <> appUrl domain dName)]
    appUrl domain dName  = coerce dName <> "." <> coerce domain

createH :: Deployment -> AppM CommandResponse
createH dep = do
  st <- ask
  let
    pgPool                                                 = pool st
    createDep :: PgPool -> Deployment -> IO Int64
    createDep p Deployment { name = n, tag = t, envs = e } =
      withResource p $ \conn -> execute
        conn
        "INSERT INTO deployments (name, tag, envs) VALUES (?, ?, ?)"
        -- FIXME: make EnvPairs a newtype and give it ToField instance
        (n, t, formatEnvPairs e)
  failIfImageNotFound $ tag dep
  res :: Either SqlError Int64 <- liftIO . try $ createDep pgPool dep
  case res of
    Right _                                                 -> pure ()
    Left (SqlError code _ _ _ _) | code == unique_violation ->
      throwError err400
        { errBody = validationError ["deployment already exists"] [] }
    Left (SqlError _ _ _ _ _)                               ->
      throwError err409 { errBody = appError "some database error" }
  ec <- createDeployment dep st
  void $ liftIO $ do
    createDeploymentLog pgPool dep "create" ec $ ArchivedFlag False
    handleExitCode ec
  sendReloadEvent
  pure Success

createDeployment :: Deployment -> AppState -> AppM ExitCode
createDeployment dep st = do
  let
    log :: Text -> IO ()
    log  = logInfo (logger st)
    args =
      [ "--project-name", coerce $ projectName st
      , "--base-domain", coerce $ baseDomain st
      , "--namespace", coerce $ namespace st
      , "--name", coerce $ name dep
      , "--tag", coerce $ tag dep
      ] ++ concat [["--env", concatPair e] | e <- envs dep]
    cmd  = coerce $ creationCommand st

  liftIO $ do
    log $ "call " <> unwords (cmd : args)
    ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
    log $ "deployment created, deployment: " <> (pack . show $ dep)
    pure ec

getH :: DeploymentName -> AppM Deployment
getH dName = do
  st <- ask
  dep <- selectDeployment (pool st) dName AllDeployments
  liftIO $ do
    logInfo (logger st) $ "get deployment: " <> (pack . show $ dep)
    return dep

selectDeploymentLogs :: PgPool -> DeploymentName -> IO [DeploymentLog]
selectDeploymentLogs p dName = do
  let
    q =
      "SELECT action::text, tag, envs, exit_code, \
        \extract(epoch from created_at)::int \
      \FROM deployment_logs \
      \WHERE deployment_id in (\
        \SELECT id FROM deployments WHERE name = ?\
      \) \
      \ORDER BY created_at DESC \
      \LIMIT 20"
  retrievedLogs <- withResource p (\conn -> query conn q (Only dName))
  -- FIXME: use FromRow instance instead
  for retrievedLogs $ \(a, t, e, ec, ts) -> do
    envPairs <- parseEnvs $ lines e
    pure $ DeploymentLog a t envPairs ec ts

selectDeployment
  :: PgPool
  -> DeploymentName
  -> DeploymentListType
  -> AppM Deployment
selectDeployment p dName lType = do
  let
    q AllDeployments          =
      "SELECT name, tag, envs FROM deployments \
      \WHERE name = ?"
    q ArchivedOnlyDeployments =
      "SELECT name, tag, envs FROM deployments \
      \WHERE name = ? AND archived = 't'"

  deps <- liftIO $ do
    retrieved <- withResource p $ \conn ->
      query conn (q lType) (Only dName)
    for retrieved $ \(n, t, e) -> Deployment n t <$> parseEnvs (lines e)
  case deps of
    [dep] -> pure dep
    []    -> throwError err404
      { errBody = validationError ["name not found"] [] }
    _     -> throwError err406
      { errBody = validationError ["more than one name found"] [] }

editH :: DeploymentName -> EnvPairs -> AppM CommandResponse
editH dName dEnvs = do
  st <- ask
  let
    log :: Text -> IO ()
    log          = logInfo (logger st)
    pgPool       = pool st
  liftIO $ getTag pgPool >>= \case
    dTag : _ -> do
      let
        args =
          [ "--project-name", coerce $ projectName st
          , "--base-domain", coerce $ baseDomain st
          , "--namespace", coerce $ namespace st
          , "--name", coerce dName
          , "--tag", coerce dTag
          ] ++ concat [["--env", concatPair p] | p <- dEnvs]
        cmd  = coerce $ updateCommand st
      liftIO $  do
        void $ updateEditDeployment pgPool
        log $ "call " <> unwords (cmd : args)
        ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
        log
          $ "deployment edited, name: " <> coerce dName
          <> ", envs: " <> formatEnvPairs dEnvs
        let dep = Deployment dName dTag dEnvs
        createDeploymentLog pgPool dep "edit" ec $ ArchivedFlag False
        handleExitCode ec
        return ()
    _     ->
      liftIO . logWarning (logger st) $ "tag not found, name: " <> coerce dName
  sendReloadEvent
  return Success
  where
    getTag                 :: PgPool -> IO [DeploymentTag]
    getTag p               = fmap (fmap fromOnly) $ withResource p $ \conn ->
      query conn "SELECT tag FROM deployments WHERE name = ?" (Only dName)
    updateEditDeployment   :: PgPool -> IO Int64
    updateEditDeployment p = withResource p $ \conn -> execute
      conn
      "UPDATE deployments SET envs = ?, updated_at = now() WHERE name = ?"
      -- FIXME: make EnvPairs a newtype and give it ToField instance
      (formatEnvPairs dEnvs, dName)

deleteH :: DeploymentName -> AppM CommandResponse
deleteH dName = do
  st <- ask
  let
    log     = logInfo (logger st)
    pgPool  = pool st
    args    =
      [ "--project-name", coerce $ projectName st
      , "--namespace", coerce $ namespace st
      , "--name", coerce dName
      ]
    cmd     = coerce $ deletionCommand st
  dep <- selectDeployment pgPool dName AllDeployments
  liftIO $ do
    log $ "call " <> unwords (cmd : args)
    ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
    void $ archiveDeployment pgPool dName
    void $ createDeploymentLog pgPool dep "delete" ec $ ArchivedFlag True
    log $ "deployment deleted, name: " <> coerce dName
    handleExitCode ec
  sendReloadEvent
  pure Success

archiveDeployment :: PgPool -> DeploymentName -> IO Int64
archiveDeployment p dName = withResource p $ \conn -> do
  let
    q =
      "UPDATE deployments \
      \SET archived = 't', archived_at = now() \
      \WHERE name = ?"
  execute conn q (Only dName)

updateH :: DeploymentName -> DeploymentUpdate -> AppM CommandResponse
updateH dName DeploymentUpdate { newTag = dTag, newEnvs = nEnvs } = do
  st <- ask
  let pgPool = pool st
  retrieved <- liftIO $ selectEnvPairs pgPool dName
  case retrieved of
    storedEnv : _ -> do
      envPairs <- liftIO $ case nEnvs of
        Nothing -> parseEnvs $ lines storedEnv
        Just dEnvs  -> pure dEnvs
      let
        log  = logInfo (logger st)
        args =
          [ "--project-name", coerce $ projectName st
          , "--base-domain", coerce $ baseDomain st
          , "--namespace", coerce $ namespace st
          , "--name", coerce $ dName
          , "--tag", coerce $ dTag
          ] ++ concat [["--env", concatPair e] | e <- envPairs]
        cmd  = coerce $ updateCommand st
      failIfImageNotFound dTag
      liftIO $ do
        void $ updateDeploymentNameAndTag pgPool dName dTag
        log $ "call " <> unwords (cmd : args)
        ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
        log $ "deployment updated, name: "
          <> coerce dName <> ", tag: " <> coerce dTag
        void $ do
          let
            dep = Deployment dName dTag envPairs
            arch = ArchivedFlag False
          createDeploymentLog pgPool dep "update" ec arch
        handleExitCode ec
        return ()
    _ ->
      liftIO . logWarning (logger st) $ "envs not found, name: " <> coerce dName
  sendReloadEvent
  return Success

selectEnvPairs :: PgPool -> DeploymentName -> IO [Text]
selectEnvPairs p dName = fmap (fmap fromOnly) $ withResource p $ \conn -> query
  conn
  "SELECT envs FROM deployments WHERE name = ?"
  (Only dName)

updateDeploymentNameAndTag
  :: PgPool
  -> DeploymentName
  -> DeploymentTag
  -> IO Int64
updateDeploymentNameAndTag p dName dTag = withResource p $ \conn -> execute
  conn
  "UPDATE deployments SET tag = ?, updated_at = now() WHERE name = ?"
  (dTag, dName)

infoH :: DeploymentName -> AppM [DeploymentInfo]
infoH dName = do
  st <- ask
  let pgPool = pool st
  dep <- selectDeployment pgPool dName AllDeployments
  liftIO $ do
    depLogs <- selectDeploymentLogs pgPool dName
    let depInfo = DeploymentInfo dep $ reverse depLogs
    logInfo (logger st) $ "get deployment info: " <> (pack . show $ depInfo)
    pure [depInfo]

pingH :: AppM NoContent
pingH = do
  pgPool <- pool <$> ask
  _ :: [Only Int] <- liftIO $ withResource pgPool $ \conn ->
    query_ conn "SELECT 1"
  pure NoContent

projectNameH :: AppM ProjectName
projectNameH = projectName <$> ask

statusH :: DeploymentName -> AppM DeploymentStatus
statusH dName = do
  st <- ask
  let pgPool = pool st
  void $ selectDeployment pgPool dName AllDeployments
  cmd <- checkingCommand <$> ask
  let args = [unpack . coerce $ dName]
  ec <- liftIO $ withProcessWait (proc (unpack $ coerce cmd) args) waitProcess
  pure . DeploymentStatus $
    case ec of
      ExitSuccess -> Ok
      _           -> Error

cleanupH :: DeploymentName -> AppM CommandResponse
cleanupH dName = do
  st <- ask
  cleanupDeployment dName st
  sendReloadEvent
  pure Success

cleanupDeployment :: DeploymentName -> AppState -> AppM ()
cleanupDeployment dName st = do
  let
    log     = logInfo (logger st)
    pgPool  = pool st
    args    =
      [ "--project-name", coerce $ projectName st
      , "--namespace", coerce $ namespace st
      , "--name", coerce dName
      ]
    cmd     = coerce $ cleanupCommand st
  liftIO $ do
    log $ "call " <> unwords (cmd : args)
    ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
    void $ deleteDeploymentLogs pgPool dName
    void $ deleteDeployment pgPool dName
    log $ "deployment destroyed, name: " <> coerce dName
    handleExitCode ec

deleteDeploymentLogs :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogs p n = withResource p $ \conn -> execute
  conn
  "DELETE FROM deployment_logs WHERE deployment_id in\
  \  (SELECT id FROM deployments where name = ?)"
  (Only n)

deleteDeployment :: PgPool -> DeploymentName -> IO Int64
deleteDeployment p n = withResource p $ \conn ->
  execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

cleanArchiveH :: AppM CommandResponse
cleanArchiveH = do
  st <- ask
  let
    pgPool  = pool st
    archRetention = unArchiveRetention . archiveRetention $ st
    q =
      "SELECT name FROM deployments \
      \WHERE archived = 't' AND archived_at + interval '?' second < now()"
  retrieved :: [Only DeploymentName] <- liftIO $
    withResource pgPool $ \conn -> query conn q (Only archRetention)
  void $ for retrieved $ \(Only dName) -> cleanupDeployment dName st
  pure Success

restoreH :: DeploymentName -> AppM CommandResponse
restoreH dName = do
  st <- ask
  let pgPool  = pool st
  dep <- selectDeployment pgPool dName ArchivedOnlyDeployments
  failIfImageNotFound $ tag dep
  ec <- createDeployment dep st
  void $ liftIO $ do
    let
      q =
        "UPDATE deployments SET archived = 'f', archived_at = null \
        \WHERE name = ?"
    void $ withResource pgPool $ \conn -> execute conn q (Only dName)
    createDeploymentLog pgPool dep "restore" ec $ ArchivedFlag False
    handleExitCode ec
  sendReloadEvent
  pure Success

waitProcess :: (Show o, Show e) => Process i o e -> IO ExitCode
waitProcess p = do
  print . getStdout $ p
  print . getStderr $ p
  waitExitCode p

handleExitCode :: ExitCode -> IO ()
handleExitCode ExitSuccess     = return ()
handleExitCode (ExitFailure c) = throwIO $ DeploymentFailed c

createDeploymentLog
  :: PgPool
  -> Deployment
  -> Action
  -> ExitCode
  -> ArchivedFlag
  -> IO ()
createDeploymentLog pgPool (Deployment dName dTag dEnvs) act ec arch = do
  let
    exitCode' = case ec of
      ExitSuccess     -> 0
      ExitFailure err -> err
    arch' = unArchivedFlag arch
    q =
      "INSERT INTO deployment_logs \
      \(deployment_id, action, tag, envs, exit_code, archived) (\
        \SELECT id, ?, ?, ?, ?, ? \
        \FROM deployments \
        \WHERE name = ? \
      \)"
  void $ withResource pgPool $ \conn ->
    execute conn q (act, dTag, formatEnvPairs dEnvs, exitCode', arch', dName)

failIfImageNotFound :: DeploymentTag -> AppM ()
failIfImageNotFound dTag = do
  foundTag <- liftIO . findImageTag $ dTag
  case foundTag of
    Just _  -> pure ()
    Nothing ->
      throwError err400 { errBody = validationError [] ["tag not found"] }

appError :: Text -> BSL.ByteString
appError = encode . AppError

validationError :: [Text] -> [Text] -> BSL.ByteString
validationError nameErrors tagErrors =
  encode $ ValidationError nameErrors tagErrors

sendReloadEvent :: AppM ()
sendReloadEvent = do
  channel <- eventSink <$> ask
  liftIO . atomically $ writeTChan channel FrontendPleaseUpdateEverything

logInfo :: TimedFastLogger -> Text -> IO ()
logInfo l = logWithSeverity l "INFO"

logWarning :: TimedFastLogger -> Text -> IO ()
logWarning l = logWithSeverity l "WARN"

logWithSeverity :: ToLogStr msg => TimedFastLogger -> ByteString -> msg -> IO ()
logWithSeverity l severity msg = l $ \ft -> metadata ft <> message
  where
    metadata :: ByteString -> LogStr
    metadata ft = foldMap toLogStr ["[", ft, " ", severity, "] "]
    message     = toLogStr msg <> toLogStr ("\n" :: ByteString)
