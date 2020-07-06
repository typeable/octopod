module DMS (runDMS) where


import           Chronos (Time, getTime, now)
import           Control.Applicative
import           Control.Concurrent (forkIO, threadDelay)
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
import           Data.Text.IO (hGetContents)
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
    appSt        =
      AppState
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
    app'         = app appSt
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
      `race_` (runStatusUpdater appSt)

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
  ) :<|> getActionInfoH :<|> pingH :<|> cleanArchiveH :<|> projectNameH

listH :: AppM [DeploymentFullInfo]
listH = do
  AppState {pool = p, logger = l, baseDomain = d} <- ask

  retrieved <- liftIO $ withResource p $ \conn -> query_ conn q
  deployments <- liftIO $ for retrieved $ \(n, t, e, a, ct, ut, st) -> do
    es <- parseEnvs (lines e)
    pure $
      DeploymentFullInfo (Deployment n t es) (read st) a ct ut (depUrls d n)

  liftIO . logInfo l $ "get deployments: " <> (pack . show $ deployments)
  return deployments
  where
    q                    =
      "SELECT name, tag, envs, archived, extract(epoch from created_at)::int, \
        \extract(epoch from updated_at)::int, status::text \
      \FROM deployments ORDER BY name"
    depUrls domain dName =
      [ ("app", appUrl domain dName)
      , ("kibana", "kibana." <> appUrl domain dName)
      , ("tasker", "tasker." <> appUrl domain dName)]
    appUrl domain dName  = coerce dName <> "." <> coerce domain

createH :: Deployment -> AppM CommandResponse
createH dep = do
  t1 <- liftIO $ now
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
  void . liftIO . forkIO $ do
    (ec, out, err) <- createDeployment dep st
    t2 <- now
    let
      arch   = ArchivedFlag False
      elTime = elapsedTime t2 t1
    createDeploymentLog pgPool dep "create" ec arch elTime out err
    sendReloadEvent st
    handleExitCode ec
  pure Success

createDeployment :: Deployment -> AppState -> IO (ExitCode, Stdout, Stderr)
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
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    log $ "deployment created, deployment: " <> (pack . show $ dep)
    pure (ec, out, err)


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
      "SELECT id, action::text, tag, envs, exit_code, \
        \duration, extract(epoch from created_at)::int \
      \FROM deployment_logs \
      \WHERE deployment_id in (\
        \SELECT id FROM deployments WHERE name = ?\
      \) \
      \ORDER BY created_at DESC \
      \LIMIT 20"
  retrievedLogs <- withResource p (\conn -> query conn q (Only dName))
  -- FIXME: use FromRow instance instead
  for retrievedLogs $ \(ai, a, t, e, ec, d, ts) -> do
    envPairs <- parseEnvs $ lines e
    pure $ DeploymentLog (ActionId ai) a t envPairs ec (Duration d) ts

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
  t1 <- liftIO $ now
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
      void . liftIO . forkIO $ do
        void $ updateEditDeployment pgPool
        log $ "call " <> unwords (cmd : args)
        (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
        log
          $ "deployment edited, name: " <> coerce dName
          <> ", envs: " <> formatEnvPairs dEnvs
        let dep = Deployment dName dTag dEnvs
        t2 <- now
        let
          arch   = ArchivedFlag False
          elTime = elapsedTime t2 t1
        createDeploymentLog pgPool dep "edit" ec arch elTime out err
        sendReloadEvent st
        handleExitCode ec
    _     ->
      liftIO . logWarning (logger st) $ "tag not found, name: " <> coerce dName
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
  t1 <- liftIO $ now
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
    arch    = ArchivedFlag True
  dep <- selectDeployment pgPool dName AllDeployments
  void . liftIO . forkIO $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    void $ archiveDeployment pgPool dName
    t2 <- now
    let elTime = elapsedTime t2 t1
    void $ createDeploymentLog pgPool dep "delete" ec arch elTime out err
    log $ "deployment deleted, name: " <> coerce dName
    sendReloadEvent st
    handleExitCode ec
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
  t1 <- liftIO $ now
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
      void . liftIO . forkIO $ do
        void $ updateDeployment pgPool dName dTag envPairs
        log $ "call " <> unwords (cmd : args)
        (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
        log $ "deployment updated, name: "
          <> coerce dName <> ", tag: " <> coerce dTag
        void $ do
          t2 <- now
          let
            dep    = Deployment dName dTag envPairs
            arch   = ArchivedFlag False
            elTime = elapsedTime t2 t1
          createDeploymentLog pgPool dep "update" ec arch elTime out err
        sendReloadEvent st
        handleExitCode ec
    _ ->
      liftIO . logWarning (logger st) $ "envs not found, name: " <> coerce dName
  return Success

selectEnvPairs :: PgPool -> DeploymentName -> IO [Text]
selectEnvPairs p dName = fmap (fmap fromOnly) $ withResource p $ \conn -> query
  conn
  "SELECT envs FROM deployments WHERE name = ?"
  (Only dName)

updateDeployment
  :: PgPool
  -> DeploymentName
  -> DeploymentTag
  -> EnvPairs
  -> IO Int64
updateDeployment p dName dTag dEnvs =
  withResource p $ \conn -> execute
  conn
  "UPDATE deployments SET tag = ?, envs = ?, updated_at = now() WHERE name = ?"
  (dTag, formatEnvPairs dEnvs, dName)

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

statusH :: DeploymentName -> AppM CurrentDeploymentStatus
statusH dName = do
  st <- ask
  let
    pgPool = pool st
    log    = logInfo (logger st)
    cmd    = checkingCommand st
    args   =
      [ "--namespace", coerce $ namespace st
      , "--name", coerce $ dName ]
  void $ selectDeployment pgPool dName AllDeployments
  liftIO $ log $ "call " <> unwords (coerce cmd : args)
  ec <- liftIO $ runCommandWithoutPipes (unpack $ coerce cmd) (unpack <$> args)
  pure . CurrentDeploymentStatus $
    case ec of
      ExitSuccess -> Ok
      _           -> Error

cleanupH :: DeploymentName -> AppM CommandResponse
cleanupH dName = do
  st <- ask
  cleanupDeployment dName st
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
  void . liftIO . forkIO $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    print out >> print err
    void $ deleteDeploymentLogs pgPool dName
    void $ deleteDeployment pgPool dName
    log $ "deployment destroyed, name: " <> coerce dName
    sendReloadEvent st
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
  t1 <- liftIO $ now
  st <- ask
  let pgPool  = pool st
  dep <- selectDeployment pgPool dName ArchivedOnlyDeployments
  failIfImageNotFound $ tag dep
  void . liftIO . forkIO $ do
    (ec, out, err) <- createDeployment dep st
    let
      q =
        "UPDATE deployments SET archived = 'f', archived_at = null \
        \WHERE name = ?"
    void $ withResource pgPool $ \conn -> execute conn q (Only dName)
    t2 <- now
    let
      arch   = ArchivedFlag False
      elTime = elapsedTime t2 t1
    createDeploymentLog pgPool dep "restore" ec arch elTime out err
    sendReloadEvent st
    handleExitCode ec
  pure Success

getActionInfoH :: ActionId -> AppM ActionInfo
getActionInfoH aId = do
  st <- ask
  let
    pgPool = pool st
    aId' = Only . unActionId $ aId
    q = "SELECT stdout, stderr FROM deployment_logs WHERE id = ?"
  rows :: [(Text, Text)] <- liftIO $
    withResource pgPool $ \conn -> query conn q aId'
  case rows of
    (out, err) : _ -> pure $ ActionInfo out err
    _              ->
      throwError err400 { errBody = appError "action not found" }

runCommand :: FilePath -> [String] -> IO (ExitCode, Stdout, Stderr)
runCommand cmd args = do
  let proc' c a = setStdout createPipe . setStderr createPipe $ proc c a
  withProcessWait (proc' cmd args) $ \p -> do
    ec <- waitExitCode p
    out <- hGetContents . getStdout $ p
    err <- hGetContents . getStderr $ p
    pure (ec, Stdout out, Stderr err)

runCommandWithoutPipes :: FilePath -> [String] -> IO ExitCode
runCommandWithoutPipes cmd args = do
  withProcessWait (proc cmd args) $ \p -> do
    ec <- waitExitCode p
    pure ec

handleExitCode :: ExitCode -> IO ()
handleExitCode ExitSuccess     = return ()
handleExitCode (ExitFailure c) = throwIO $ DeploymentFailed c

createDeploymentLog
  :: PgPool
  -> Deployment
  -> Action
  -> ExitCode
  -> ArchivedFlag
  -> Duration
  -> Stdout
  -> Stderr
  -> IO ()
createDeploymentLog pgPool dep act ec arch dur out err = do
  let
    (Deployment dName dTag dEnvs) = dep
    rawEnvs                       = formatEnvPairs dEnvs
    exitCode'                     =
      case ec of
        ExitSuccess         -> 0
        ExitFailure errCode -> errCode
    arch'                         = unArchivedFlag arch
    dur'                          = unDuration dur
    out'                          = unStdout out
    err'                          = unStderr err
    q                             =
      "INSERT INTO deployment_logs \
      \(deployment_id, action, tag, envs, exit_code, archived, \
      \duration, stdout, stderr) \
      \(\
        \SELECT id, ?, ?, ?, ?, ?, ?, ?, ? \
        \FROM deployments \
        \WHERE name = ? \
      \)"
  void $ withResource pgPool $ \conn ->
    execute conn q
      (act, dTag, rawEnvs, exitCode', arch', dur', out', err', dName)

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

sendReloadEvent :: AppState -> IO ()
sendReloadEvent state =
  atomically $ writeTChan (eventSink state) FrontendPleaseUpdateEverything

elapsedTime :: Time -> Time -> Duration
elapsedTime t1 t2 =
  Duration . fromIntegral . (`div` 1000000) . abs $ getTime t2 - getTime t1

runStatusUpdater :: AppState -> IO ()
runStatusUpdater state = do
  let
    pgPool      = pool state
    checkingCmd = checkingCommand state
    ns          = namespace state
    interval    = 30 :: Int
    select_deps =
      "SELECT name, status::text, \
      \extract(epoch from now())::int - \
        \extract(epoch from status_updated_at)::int \
      \FROM deployments \
      \WHERE checked_at < now() - interval '?' second"
    update_status  =
      "UPDATE deployments \
      \SET status = ?, status_updated_at = now(), checked_at = now() \
      \WHERE name = ?"
    update_checked_at  =
      "UPDATE deployments SET checked_at = now() WHERE name = ?"

  forever $ do
    rows :: [(DeploymentName, Text, Int)] <- liftIO $
      withResource pgPool $ \conn -> query conn select_deps (Only interval)
    let
      checkList :: [(DeploymentName, DeploymentStatus, Timestamp)] =
        (\(n, s, t) -> (n, read . unpack $ s, coerce t)) <$> rows
    checkResult <- for checkList $ \(dName, dStatus, ts) -> do
      let
        args = unpack <$> coerce ns : coerce dName : []
        cmd  = unpack . coerce $ checkingCmd
      ec <- runCommandWithoutPipes cmd args
      pure (dName, newStatus ec dStatus ts, ts)
    void $
      for (zip checkList checkResult) $ \((dName, oldSt, _), (_, newSt, _)) ->
        withResource pgPool $ \conn ->
          if oldSt == newSt
            then execute conn update_checked_at (Only dName)
            else execute conn update_status (show newSt, dName)
    if checkList == checkResult
      then pure ()
      else sendReloadEvent state
    threadDelay 5000000

updatePeriod :: Timestamp
updatePeriod = Timestamp 600

newStatus :: ExitCode -> DeploymentStatus -> Timestamp -> DeploymentStatus
newStatus ExitSuccess     _             _                      = Running
newStatus (ExitFailure _) Running       _                      = Failure
newStatus (ExitFailure _) CreatePending ts | ts > updatePeriod = Failure
newStatus (ExitFailure _) UpdatePending ts | ts > updatePeriod = Failure
newStatus (ExitFailure _) DeletePending _                      = Failure
newStatus (ExitFailure _) oldStatus     _                      = oldStatus

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
