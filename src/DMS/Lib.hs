{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module DMS.Lib
    ( runDMS
    ) where

import Prelude hiding (lines, unlines, unwords)

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Monoid ((<>))
import Data.Text (lines, pack, unlines, unpack, unwords)
import Data.Pool
import Database.PostgreSQL.Simple
import Options.Generic
import Network.Wai.Handler.Warp
import Servant
import System.Exit
import System.Log.FastLogger
import System.Process.Typed

import API.Lib (Deployment (Deployment), DeploymentAPI, name, tag, envs)
import DMS.Helm

data Args
  = Args { port :: Int, db :: ByteString, dbPoolSize :: Int }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

type PgPool = Pool Connection
type AppM = ReaderT State Handler

data State
  = State { pool :: PgPool, logger :: TimedFastLogger, helm :: FilePath, b2bHelm :: FilePath }

runDMS :: IO ()
runDMS = do
  timeCache <- newTimeCache "%Y-%m-%d %T%z"
  (logger, cleanUp) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
  logInfo logger "started"
  args <- getRecord "DMS"
  helm <- helmPath
  b2bHelm <- b2bHelmPath
  case (helm, b2bHelm) of
    (Just h, Just b) -> do
      pool <- initConnectionPool (db args) (dbPoolSize args)
      run (port args) (app $ State pool logger h b)
    (Nothing, _) -> die "helm not found"
    (_, _) -> die "b2b-helm not found"

initConnectionPool :: ByteString -> Int -> IO PgPool
initConnectionPool db = createPool (connectPostgreSQL db) close 1 30

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

app :: State -> Application
app s = serve deploymentAPI $ hoistServer deploymentAPI (nt s) server

deploymentAPI :: Proxy DeploymentAPI
deploymentAPI = Proxy

server :: ServerT DeploymentAPI AppM
server = list :<|> create :<|> get :<|> edit :<|> destroy :<|> update

list :: AppM [Text]
list = do
  State{pool = p, logger = l} <- ask
  ds <- getDeployments p
  liftIO . logInfo l $ "get deployments: " <> (pack . show $ ds)
  return ds

  where
    getDeployments :: PgPool -> AppM [Text]
    getDeployments p = fmap (fmap fromOnly) . liftIO $
      withResource p $ \conn -> query_ conn "SELECT name FROM deployments"

create :: Deployment -> AppM Text
create d = do
  State{pool = p, logger = l, b2bHelm = b} <- ask
  let ia = infraArgs d
      aa = appArgs d
  createDeployment p d
  liftIO . logInfo l $ "call " <> unwords (pack <$> b : ia)
  createInfra ia b
  liftIO . logInfo l $ "call " <> unwords (pack <$> b : aa)
  createApp aa b
  liftIO . logInfo l $ "deployment created, deployment: " <> (pack . show $ d)
  return ""

  where
    createDeployment :: PgPool -> Deployment -> AppM Int64
    createDeployment p Deployment { name = n, tag = t, envs = e } = liftIO $
      withResource p $ \conn ->
        execute conn "INSERT INTO deployments (name, tag, envs) VALUES (?, ?, ?)" (n, t, unlines e)

    infraArgs Deployment { name = n } = createInfraArgs . unpack $ n

    appArgs Deployment { name = n, tag = t, envs = e } = createAppArgs (unpack n) (unpack t) (fmap unpack e)

    createInfra :: [String] -> String -> AppM ()
    createInfra args b2bHelm = liftIO . withProcessWait_ (proc b2bHelm args) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

    createApp :: [String] -> String -> AppM ()
    createApp args b2bHelm = liftIO . withProcessWait_ (proc b2bHelm args) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

get :: Text -> AppM [Deployment]
get n = do
  State{pool = p, logger = l} <- ask
  d <- getDeployment p
  liftIO . logInfo l $ "get deployment: " <> (pack . show $ d)
  return d

  where
    getDeployment :: PgPool -> AppM [Deployment]
    getDeployment p = fmap (fmap (\(n, t, e) -> Deployment n t $ lines e)) . liftIO $
      withResource p $ \conn -> query conn "SELECT name, tag, envs FROM deployments WHERE name = ?" (Only n)

edit :: Text -> Deployment -> AppM Text
edit n d@Deployment { envs =  e } = do
  State{pool = p, logger = l, b2bHelm = b} <- ask
  t <- getTag p
  case t of
    t : _ -> do
      let aa = appArgs $ d { name = n, tag = t }
      editDeployment p
      liftIO . logInfo l $ "call " <> unwords (pack <$> b : aa)
      editApp aa b
      liftIO . logInfo l $ "deployment edited, name: " <> n <> ", envs: " <> unwords e
    _ -> liftIO . logWarning l $ "tag not found, name: " <> n
  return ""

  where
    getTag :: PgPool -> AppM [Text]
    getTag p = fmap (fmap fromOnly) . liftIO $
      withResource p $ \conn -> query conn "SELECT tag FROM deployments WHERE name = ?" (Only n)

    editDeployment :: PgPool -> AppM Int64
    editDeployment p = liftIO $
      withResource p $ \conn ->
        execute conn "UPDATE deployments SET envs = ?, updated_at = now() WHERE name = ?" (unlines e, n)

    editApp :: [String] -> String -> AppM ()
    editApp args b2bHelm = liftIO . withProcessWait_ (proc b2bHelm args) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

    appArgs Deployment { name = n, tag = t, envs =  e } = editAppArgs (unpack n) (unpack t) (fmap unpack e)

destroy :: Text -> AppM Text
destroy n = do
  State{pool = p, logger = l, helm = h} <- ask
  let ia = infraArgs n
      aa = appArgs n
  liftIO . logInfo l $ "call " <> unwords (pack <$> h : aa)
  destroyApp aa h
  liftIO . logInfo l $ "call " <> unwords (pack <$> h : ia)
  destroyInfra ia h
  deleteDeployment p
  liftIO . logInfo l $ "deployment destroyed, name: " <> n
  return ""

  where
    deleteDeployment :: PgPool -> AppM Int64
    deleteDeployment p = liftIO $
      withResource p $ \conn ->
        execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

    infraArgs = destroyInfraArgs . unpack

    appArgs = destroyAppArgs . unpack

    destroyApp :: [String] -> String -> AppM ()
    destroyApp args helm = liftIO . withProcessWait_ (proc helm args) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

    destroyInfra :: [String] -> String -> AppM ()
    destroyInfra args helm = liftIO . withProcessWait_ (proc helm args) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

update :: Text -> Deployment -> AppM Text
update n d@Deployment { tag = t } = do
  State{pool = p, logger = l, b2bHelm = b} <- ask
  e <- getEnvs p
  case e of
    e : _ -> do
      let aa = appArgs $ d { name = n, envs = lines e }
      updateDeployment p
      liftIO . logInfo l $ "call " <> unwords (pack <$> b : aa)
      updateApp aa b
      liftIO . logInfo l $ "deployment updated, name: " <> n <> ", tag: " <> t
    _ -> liftIO . logWarning l $ "envs not found, name: " <> n
  return ""

  where
    getEnvs :: PgPool -> AppM [Text]
    getEnvs p = fmap (fmap fromOnly) . liftIO $
      withResource p $ \conn -> query conn "SELECT envs FROM deployments WHERE name = ?" (Only n)

    updateDeployment :: PgPool -> AppM Int64
    updateDeployment p = liftIO $
      withResource p $ \conn ->
        execute conn "UPDATE deployments SET tag = ?, updated_at = now() WHERE name = ?" (t, n)

    updateApp :: [String] -> String -> AppM ()
    updateApp args b2bHelm = liftIO . withProcessWait_ (proc b2bHelm args) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

    appArgs Deployment { name = n, tag = t, envs = e } = updateAppArgs (unpack n) (unpack t) (fmap unpack e)

logInfo :: TimedFastLogger -> Text -> IO ()
logInfo logger = logWithSeverity logger "INFO"

logWarning :: TimedFastLogger -> Text -> IO ()
logWarning logger = logWithSeverity logger "WARN"

logWithSeverity :: ToLogStr msg => TimedFastLogger -> ByteString -> msg -> IO ()
logWithSeverity logger severity msg = logger $ \ft -> metadata ft <> message
  where metadata ft = foldMap toLogStr ["[" :: ByteString, ft, " " :: ByteString, severity, "] " :: ByteString]
        message = toLogStr msg <> toLogStr ("\n" :: ByteString)
