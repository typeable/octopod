module DMS (runDMS) where

import Prelude hiding (lines, unlines, unwords, log)

import Control.Exception (bracket, throwIO, Exception)
import Control.Monad (unless, void)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.IORef
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Pool
import Data.Text (lines, pack, unpack, unwords)
import Data.Traversable
import Database.PostgreSQL.Simple
import Network.Wai.Handler.Warp
import Options.Generic
import Servant
import System.Exit
import System.Log.FastLogger
import System.Process.Typed

import API
import DMS.Git
import DMS.Helm
import Types


data Args
  = Args { port :: Int, db :: ByteString, dbPoolSize :: Int }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

type PgPool = Pool Connection
type AppM = ReaderT AppState Handler

data AppState = AppState
  { pool                :: PgPool
  , logger              :: TimedFastLogger
  , helm                :: FilePath
  , b2bHelm             :: FilePath
  , git                 :: FilePath
  , gitUpgradeSemaphore :: IORef Int
  }

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
  args <- getRecord "DMS"
  helmBin <- helmPath
  b2bHelmBin <- b2bHelmPath
  gitBin <- gitPath
  semaphore <- newIORef 0
  case (helmBin, b2bHelmBin, gitBin) of
    (Just h, Just b, Just g) -> do
      pgPool <- initConnectionPool (db args) (dbPoolSize args)
      run (port args) (app $ AppState pgPool logger' h b g semaphore)
    (Nothing, _, _) -> die "helm not found"
    (_, Nothing, _) -> die "b2b-helm not found"
    (_, _, Nothing) -> die "git not found"

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
    :<|> destroyH :<|> updateH :<|> infoH) :<|> pingH

listH :: AppM [DeploymentName]
listH = do
  AppState{pool = p, logger = l} <- ask
  ds <- getDeployments p
  liftIO . logInfo l $ "get deployments: " <> (pack . show $ ds)
  return ds
  where
    getDeployments :: PgPool -> AppM [DeploymentName]
    getDeployments p = fmap (fmap fromOnly) . liftIO $
      withResource p $ \conn -> query_ conn "SELECT name FROM deployments"

createH :: Deployment -> AppM NoContent
createH dep = do
  st <- ask
  let
    log :: Text -> IO ()
    log          = logInfo (logger st)
    b2bHelmBin   = b2bHelm st
    pgPool       = pool st
    infraCmdArgs = createInfraArgs $ name dep
    appCmdArgs   = appArgs dep
  liftIO $ withSemaphore (gitUpgradeSemaphore st) $ do
    log "upgrading git repo"
    void $ upgradeRepo $ git st
    void $ createDeployment pgPool dep
    log $ "call " <> unwords (pack b2bHelmBin : infraCmdArgs)
    ec1 <- createInfra infraCmdArgs b2bHelmBin
    log $ "call " <> mconcat (pack b2bHelmBin : appCmdArgs)
    ec2 <- createApp appCmdArgs b2bHelmBin
    log $ "deployment created, deployment: " <> (pack . show $ dep)
    let ec = max ec1 ec2
    createDeploymentLog pgPool dep "create" ec
    handleExitCode ec
  pure NoContent

  where
    createDeployment :: PgPool -> Deployment -> IO Int64
    createDeployment p Deployment { name = n, tag = t, envs = e } =
      withResource p $ \conn -> execute
        conn
        "INSERT INTO deployments (name, tag, envs) VALUES (?, ?, ?)"
        -- FIXME: make EnvPairs a newtype and give it ToField instance
        (n, t, formatEnvPairs e)

appArgs :: Deployment -> [CommandArg]
appArgs (Deployment n t e) = composeAppArgs n t e

createInfra :: [CommandArg] -> FilePath -> IO ExitCode
createInfra args b2bHelmBin =
  withProcessWait (withRepoPath $ proc b2bHelmBin $ unpack <$> args) waitProcess

createApp :: [CommandArg] -> FilePath -> IO ExitCode
createApp args b2bHelmBin =
  withProcessWait (withRepoPath $ proc b2bHelmBin $ unpack <$> args) waitProcess

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
    b2bHelmBin  = b2bHelm st
    pgPool       = pool st
  liftIO $ getTag pgPool >>= \case
    t : _ -> do
      liftIO $ withSemaphore (gitUpgradeSemaphore st) $ do
        log "upgrading git repo..."
        void $ upgradeRepo $ git st
        void $ updateEditDeployment pgPool
        let dep = Deployment n t e
        log $ "call " <> unwords (pack b2bHelmBin : appArgs dep)
        ec <- editApp (appArgs dep) (b2bHelm st)
        log
          $ "deployment edited, name: " <> coerce n
          <> ", envs: " <> formatEnvPairs e
        createDeploymentLog pgPool dep "edit" ec
        handleExitCode ec
        return ()
    _ -> liftIO . logWarning (logger st) $ "tag not found, name: " <> coerce n
  return NoContent
  where
    getTag :: PgPool -> IO [DeploymentTag]
    getTag p = fmap (fmap fromOnly) $ withResource p $ \conn ->
      query conn "SELECT tag FROM deployments WHERE name = ?" (Only n)
    updateEditDeployment :: PgPool -> IO Int64
    updateEditDeployment p = withResource p $ \conn -> execute
      conn
      "UPDATE deployments SET envs = ?, updated_at = now() WHERE name = ?"
      -- FIXME: make EnvPairs a newtype and give it ToField instance
      (formatEnvPairs e, n)

editApp :: [CommandArg] -> FilePath -> IO ExitCode
editApp args b2bHelmBin = liftIO . withProcessWait
  (withRepoPath $ proc b2bHelmBin $ unpack <$> args) $ waitProcess

destroyH :: DeploymentName -> AppM NoContent
destroyH dName = do
  st <- ask
  let
    helmBin = helm st
    log     = logInfo (logger st)
    pgPool  = pool st
  liftIO $ do
    let appArgs' = destroyAppArgs dName
    log $ "call " <> unwords (pack helmBin : appArgs')
    ec1 <- destroyApp appArgs' helmBin
    let infraArgs = destroyInfraArgs dName
    log $ "call " <> unwords (pack helmBin : infraArgs)
    ec2 <- destroyInfra infraArgs helmBin
    void $ deleteDeploymentLogs pgPool dName
    void $ deleteDeployment pgPool dName
    log $ "deployment destroyed, name: " <> coerce dName
    handleExitCode $ max ec1 ec2
  pure NoContent

destroyApp :: [CommandArg] -> FilePath -> IO ExitCode
destroyApp args helmBin =
  withProcessWait (withRepoPath $ proc helmBin $ unpack <$> args) waitProcess

destroyInfra :: [CommandArg] -> FilePath -> IO ExitCode
destroyInfra args helmBin =
  withProcessWait (withRepoPath $ proc helmBin $ unpack <$> args) waitProcess

deleteDeploymentLogs :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogs p n = withResource p $ \conn -> execute
  conn
  "DELETE FROM deployment_logs WHERE deployment_id in\
  \  (SELECT id FROM deployments where name = ?)"
  (Only n)

deleteDeployment :: PgPool -> DeploymentName -> IO Int64
deleteDeployment p n = withResource p $ \conn ->
  execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

updateH :: DeploymentName -> DeploymentTag -> AppM NoContent
updateH dName dTag = do
  st <- ask
  let
    pgPool    = pool st
  retrieved <- liftIO $ selectEnvPairs pgPool dName
  case retrieved of
    storedEnv : _ -> do
      envPairs <- liftIO $ parseEnvs $ lines storedEnv
      let
        b2bHelmBin = b2bHelm st
        log        = logInfo (logger st)
        args    = composeAppArgs dName dTag envPairs
      liftIO $ withSemaphore (gitUpgradeSemaphore st) $ do
        log "upgrading git repo..."
        void $ upgradeRepo (git st)
        void $ updateDeploymentNameAndTag pgPool dName dTag
        log $ "call " <> unwords (pack b2bHelmBin : args)
        let
          updateApp :: [CommandArg] -> IO ExitCode
          updateApp args' = withProcessWait
            (withRepoPath $ proc b2bHelmBin $ unpack <$> args')
            waitProcess
        ec <- updateApp args
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
  let
    pgPool = pool st
  liftIO $ do
    dep <- selectDeployment pgPool dName
    depLogs <- selectDeploymentLogs pgPool dName
    let depInfo = DeploymentInfo dep $ reverse depLogs
    logInfo (logger st) $ "get deployment info: " <> (pack . show $ depInfo)
    pure [depInfo]

pingH :: AppM NoContent
pingH = do
  pgPool <- pool <$> ask
  _ :: [Only Int] <-
    liftIO $ withResource pgPool $ \conn -> query_ conn "SELECT 1"
  pure NoContent

waitProcess :: (Show o, Show e) => Process i o e -> IO ExitCode
waitProcess p = do
  print . getStdout $ p
  print . getStderr $ p
  waitExitCode p

handleExitCode :: ExitCode -> IO ()
handleExitCode ExitSuccess = return ()
handleExitCode (ExitFailure c) = throwIO $ DeploymentFailed c

withSemaphore :: IORef Int -> IO () -> IO ()
withSemaphore semaphore act = bracket
  (tryToAcquireSemaphore semaphore >>= \r ->
    unless r $ throwIO ThereIsAcriveDeployment)
  (\_ -> releaseSemaphore semaphore)
  (const act)

tryToAcquireSemaphore :: IORef Int -> IO Bool
tryToAcquireSemaphore s = atomicModifyIORef' s (\a -> (if a == 0 then 1 else a, a == 0))

releaseSemaphore :: IORef Int -> IO ()
releaseSemaphore s = modifyIORef' s pred

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
