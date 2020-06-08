module DMS (runDMS) where


import Control.Applicative
import Control.Concurrent.Async (race_)
import Control.Exception (throwIO, Exception)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Int (Int64)
import Data.Maybe
import Data.Pool
import Data.Text (lines, pack, unpack, unwords)
import Data.Traversable
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Options.Generic
import Prelude hiding (lines, unlines, unwords, log)
import Servant
import System.Environment (lookupEnv)
import System.Exit
import System.Log.FastLogger
import System.Process.Typed


import API
import DMS.Args
import TLS (createTLSOpts)
import Types


type PgPool = Pool Connection
type AppM   = ReaderT AppState Handler

data AppState = AppState
  { pool                :: PgPool
  , logger              :: TimedFastLogger
  , project_name        :: ProjectName
  , base_domain         :: Domain
  , namespace           :: Namespace
  , creation_command    :: Command
  , update_command      :: Command
  , deletion_command    :: Command
  , checking_command    :: Command }

data DeploymentException
  = DeploymentFailed Int
  | ThereIsAcriveDeployment
  deriving (Show)

instance Exception DeploymentException

runDMS :: IO ()
runDMS = do
  timeCache <- newTimeCache "%Y-%m-%d %T%z"
  (logger', _) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
  logInfo logger' "started"
  opts <- parseArgs
  let
    a ?! e = a >>= maybe (die e) pure
    getEnvOrDie eName = lookupEnv eName ?! (eName <> " is not set")
  proj_name <- coerce . pack <$> getEnvOrDie "PROJECT_NAME"
  domain <- coerce . pack <$> getEnvOrDie "BASE_DOMAIN"
  ns <- coerce . pack <$> getEnvOrDie "NAMESPACE"
  creation_cmd <- coerce . pack <$> getEnvOrDie "CREATION_COMMAND"
  update_cmd <- coerce . pack <$> getEnvOrDie "UPDATE_COMMAND"
  deletion_cmd <- coerce . pack <$> getEnvOrDie "DELETION_COMMAND"
  checking_cmd <- coerce . pack <$> getEnvOrDie "CHECKING_COMMAND"
  pgPool <- initConnectionPool (unDBConnectionString $ dmsDB opts) (unDBPoolSize $ dmsDBPoolSize opts)
  let app'         = app $ AppState pgPool
                                    logger'
                                    proj_name
                                    domain
                                    ns
                                    creation_cmd
                                    update_cmd
                                    deletion_cmd
                                    checking_cmd
      serverPort   = dmsPort opts
      uiServerPort = unServerPort $ dmsUIPort opts
      tlsOpts      = createTLSOpts (dmsTLSCertPath opts) (dmsTLSKeyPath opts) (dmsTLSStorePath opts) serverPort
      warpOpts     = setPort (unServerPort serverPort) defaultSettings
      in (run uiServerPort app') `race_` (runTLS tlsOpts warpOpts app')

initConnectionPool :: ByteString -> Int -> IO PgPool
initConnectionPool dbConnStr =
  createPool (connectPostgreSQL dbConnStr) close 1 30

nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s

app :: AppState -> Application
app s = serve api $ hoistServer api (nt s) server
  where
    api = Proxy @API

server :: ServerT API AppM
server = (listH :<|> createH :<|> getH :<|> editH
    :<|> destroyH :<|> updateH :<|> infoH :<|> statusH) :<|> pingH

listH :: AppM [DeploymentFullInfo]
listH = do
  AppState {pool = p, logger = l, base_domain = d} <- ask

  retrieved <- liftIO $ withResource p $ \conn -> query_ conn q
  deployments <- liftIO $ for retrieved $ \(n, t, e, ct, ut) -> do
    es <- parseEnvs (lines e)
    pure $ DeploymentFullInfo (Deployment n t es) ct ut (depUrls d n)

  liftIO . logInfo l $ "get deployments: " <> (pack . show $ deployments)
  return deployments
  where
    q                    =
      "SELECT name, tag, envs, extract(epoch from created_at)::int, extract(epoch from updated_at)::int \
      \FROM deployments ORDER BY name"
    depUrls domain dName = [ ("app", appUrl domain dName)
                           , ("kibana", "kibana." <> appUrl domain dName)
                           , ("tasker", "tasker." <> appUrl domain dName)
                           ]
    appUrl domain dName  = coerce dName <> "." <> coerce domain

createH :: Deployment -> AppM NoContent
createH dep = do
  st <- ask
  let
    log :: Text -> IO ()
    log                                                           = logInfo (logger st)
    pgPool                                                        = pool st
    args                                                          =
      [ "--project-name", coerce $ project_name st
      , "--base-domain", coerce $ base_domain st
      , "--namespace", coerce $ namespace st
      , "--name", coerce $ name dep
      , "--tag", coerce $ tag dep
      ] ++ concat [["--env", concatPair e] | e <- envs dep]
    cmd                                                           = coerce $ creation_command st
    createDeployment :: PgPool -> Deployment -> IO Int64
    createDeployment p Deployment { name = n, tag = t, envs = e } =
      withResource p $ \conn -> execute
        conn
        "INSERT INTO deployments (name, tag, envs) VALUES (?, ?, ?)"
        -- FIXME: make EnvPairs a newtype and give it ToField instance
        (n, t, formatEnvPairs e)

  liftIO $ do
    void $ createDeployment pgPool dep
    log $ "call " <> unwords (cmd : args)
    ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
    log $ "deployment created, deployment: " <> (pack . show $ dep)
    createDeploymentLog pgPool dep "create" ec
    handleExitCode ec
  pure NoContent

getH :: DeploymentName -> AppM Deployment
getH dName = do
  st <- ask
  liftIO $ do
    dep <- selectDeployment (pool st) dName
    logInfo (logger st) $ "get deployment: " <> (pack . show $ dep)
    return dep

selectDeploymentLogs :: PgPool -> DeploymentName -> IO [DeploymentLog]
selectDeploymentLogs p dName = do
  let
    q =
      "SELECT action::text, tag, envs, exit_code, extract(epoch from created_at)::int \
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

selectDeployment :: PgPool -> DeploymentName -> IO Deployment
selectDeployment p n = selectDeployments p n >>= \case
  [d] -> pure d
  _   -> error "more than one deployment found in the database"

selectDeployments :: PgPool -> DeploymentName -> IO [Deployment]
selectDeployments p dName = do
  retrieved <- withResource p $ \conn -> query
    conn
    "SELECT name, tag, envs FROM deployments WHERE name = ?"
    (Only dName)
  for retrieved $ \(n, t, e) -> Deployment n t <$> parseEnvs (lines e)

editH :: DeploymentName -> EnvPairs -> AppM NoContent
editH n e = do
  st <- ask
  let
    log :: Text -> IO ()
    log          = logInfo (logger st)
    pgPool       = pool st
  liftIO $ getTag pgPool >>= \case
    t : _ -> do
      let
        args = [ "--project-name", coerce $ project_name st
               , "--base-domain", coerce $ base_domain st
               , "--namespace", coerce $ namespace st
               , "--name", coerce n
               , "--tag", coerce t
               ] ++ concat [["--env", concatPair p] | p <- e]
        cmd  = coerce $ update_command st
      liftIO $  do
        void $ updateEditDeployment pgPool
        log $ "call " <> unwords (cmd : args)
        ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
        log
          $ "deployment edited, name: " <> coerce n
          <> ", envs: " <> formatEnvPairs e
        let dep = Deployment n t e
        createDeploymentLog pgPool dep "edit" ec
        handleExitCode ec
        return ()
    _     -> liftIO . logWarning (logger st) $ "tag not found, name: " <> coerce n
  return NoContent
  where
    getTag                 :: PgPool -> IO [DeploymentTag]
    getTag p               = fmap (fmap fromOnly) $ withResource p $ \conn ->
      query conn "SELECT tag FROM deployments WHERE name = ?" (Only n)
    updateEditDeployment   :: PgPool -> IO Int64
    updateEditDeployment p = withResource p $ \conn -> execute
      conn
      "UPDATE deployments SET envs = ?, updated_at = now() WHERE name = ?"
      -- FIXME: make EnvPairs a newtype and give it ToField instance
      (formatEnvPairs e, n)

destroyH :: DeploymentName -> AppM NoContent
destroyH dName = do
  st <- ask
  let
    log     = logInfo (logger st)
    pgPool  = pool st
    args    = [ "--project-name", coerce $ project_name st
              , "--namespace", coerce $ namespace st
              , "--name", coerce dName
              ]
    cmd     = coerce $ deletion_command st
  liftIO $ do
    log $ "call " <> unwords (cmd : args)
    ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
    void $ deleteDeploymentLogs pgPool dName
    void $ deleteDeployment pgPool dName
    log $ "deployment destroyed, name: " <> coerce dName
    handleExitCode ec
  pure NoContent

deleteDeploymentLogs :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogs p n = withResource p $ \conn -> execute
  conn
  "DELETE FROM deployment_logs WHERE deployment_id in\
  \  (SELECT id FROM deployments where name = ?)"
  (Only n)

deleteDeployment :: PgPool -> DeploymentName -> IO Int64
deleteDeployment p n = withResource p $ \conn ->
  execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

updateH :: DeploymentName -> DeploymentUpdate -> AppM NoContent
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
        args = [ "--project-name", coerce $ project_name st
               , "--base-domain", coerce $ base_domain st
               , "--namespace", coerce $ namespace st
               , "--name", coerce $ dName
               , "--tag", coerce $ dTag
               ] ++ concat [["--env", concatPair e] | e <- envPairs]
        cmd  = coerce $ update_command st
      liftIO $ do
        void $ updateDeploymentNameAndTag pgPool dName dTag
        log $ "call " <> unwords (cmd : args)
        ec <- withProcessWait (proc (unpack cmd) (unpack <$> args)) waitProcess
        log $ "deployment updated, name: "
          <> coerce dName <> ", tag: " <> coerce dTag
        createDeploymentLog pgPool (Deployment dName dTag envPairs) "update" ec
        handleExitCode ec
        return ()
    _ ->
      liftIO . logWarning (logger st) $ "envs not found, name: " <> coerce dName
  return NoContent

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
  liftIO $ do
    dep <- selectDeployment pgPool dName
    depLogs <- selectDeploymentLogs pgPool dName
    let depInfo = DeploymentInfo dep $ reverse depLogs
    logInfo (logger st) $ "get deployment info: " <> (pack . show $ depInfo)
    pure [depInfo]

pingH :: AppM NoContent
pingH = do
  pgPool <- pool <$> ask
  _ :: [Only Int] <- liftIO $ withResource pgPool $ \conn -> query_ conn "SELECT 1"
  pure NoContent

statusH :: DeploymentName -> AppM DeploymentStatus
statusH dName = do
  cmd <- checking_command <$> ask
  let args = [unpack . coerce $ dName]
  ec <- liftIO $ withProcessWait (proc (unpack $ coerce cmd) args) waitProcess
  pure . DeploymentStatus $
    case ec of
      ExitSuccess -> Ok
      _           -> Error

waitProcess :: (Show o, Show e) => Process i o e -> IO ExitCode
waitProcess p = do
  print . getStdout $ p
  print . getStderr $ p
  waitExitCode p

handleExitCode :: ExitCode -> IO ()
handleExitCode ExitSuccess     = return ()
handleExitCode (ExitFailure c) = throwIO $ DeploymentFailed c

createDeploymentLog :: PgPool -> Deployment -> Action -> ExitCode -> IO ()
createDeploymentLog pgPool (Deployment n t e) a ec = do
  let
    exitCode' = case ec of
      ExitSuccess     -> 0
      ExitFailure err -> err
    q =
      "INSERT INTO deployment_logs (deployment_id, action, tag, envs, exit_code) (\
        \SELECT id, ?, ?, ?, ? \
        \FROM deployments \
        \WHERE name = ? \
      \)"
  void $ withResource pgPool $ \conn ->
    execute conn q (a, t, formatEnvPairs e, exitCode', n)

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
