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
import System.Log.FastLogger
import System.Process.Typed

import API.Lib (Deployment(Deployment), DeploymentAPI)
import DMS.Helm

data Args
  = Args { port :: Int, db :: ByteString, dbPoolSize :: Int }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

type PgPool = Pool Connection
type AppM = ReaderT State Handler

data State
  = State { pool :: PgPool, logger :: TimedFastLogger }

runDMS :: IO ()
runDMS = do
  timeCache <- newTimeCache "%Y-%m-%d %T%z"
  (logger, cleanUp) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
  logInfo logger "started"
  args <- getRecord "DMS"
  pool <- initConnectionPool (db args) (dbPoolSize args)
  run (port args) (app $ State pool logger)

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
  State{pool = p, logger = l} <- ask
  b2bHelm <- liftIO b2bHelmPath
  case b2bHelm of
    Just b2bHelm -> do
      createDeployment p d
      createInfra d b2bHelm
      createApp d b2bHelm
      liftIO . logInfo l $ "deployment created, deployment: " <> (pack . show $ d)
    Nothing -> liftIO . logWarn l $ "b2b-helm not found. qed"
  return ""

  where
    createDeployment :: PgPool -> Deployment -> AppM Int64
    createDeployment p (Deployment n t e) = liftIO $
      withResource p $ \conn ->
        execute conn "INSERT INTO deployments (name, tag, envs) VALUES (?, ?, ?)" (n, t, unlines e)

    createInfra :: Deployment -> String -> AppM ()
    createInfra (Deployment n _ _) b2bHelm = liftIO . withProcessWait_ (proc b2bHelm $ createInfraArgs . unpack $ n) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

    createApp :: Deployment -> String -> AppM ()
    createApp (Deployment n _ _) b2bHelm = liftIO . withProcessWait_ (proc b2bHelm $ createAppArgs . unpack $ n) $ \pr -> do
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
edit n (Deployment _ _ e) = do
  State{pool = p, logger = l} <- ask
  updateDeployment p
  liftIO . logInfo l $ "deployment edited, name: " <> n <> ", envs: " <> unwords e
  return ""

  where
    updateDeployment :: PgPool -> AppM Int64
    updateDeployment p = liftIO $
      withResource p $ \conn ->
        execute conn "UPDATE deployments SET envs = ?, updated_at = now() WHERE name = ?" (unlines e, n)

destroy :: Text -> AppM Text
destroy n = do
  State{pool = p, logger = l} <- ask
  helm <- liftIO helmPath
  case helm of
    Just helm -> do
      destroyApp n helm
      destroyInfra n helm
      deleteDeployment p
      liftIO . logInfo l $ "deployment destroyed, name: " <> n
    Nothing -> liftIO . logWarn l $ "helm not found. qed"
  return ""

  where
    deleteDeployment :: PgPool -> AppM Int64
    deleteDeployment p = liftIO $
      withResource p $ \conn ->
        execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

    destroyApp :: Text -> String -> AppM ()
    destroyApp n helm = liftIO . withProcessWait_ (proc helm $ destroyAppArgs . unpack $ n) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

    destroyInfra :: Text -> String -> AppM ()
    destroyInfra n helm = liftIO . withProcessWait_ (proc helm $ destroyInfraArgs . unpack $ n) $ \pr -> do
        print . getStdout $ pr
        print . getStderr $ pr

update :: Text -> Deployment -> AppM Text
update n (Deployment _ t _) = do
  State{pool = p, logger = l} <- ask
  updateDeployment p
  liftIO . logInfo l $ "deployment updated, name: " <> n <> ", tag: " <> t
  return ""

  where
    updateDeployment :: PgPool -> AppM Int64
    updateDeployment p = liftIO $
      withResource p $ \conn ->
        execute conn "UPDATE deployments SET tag = ?, updated_at = now() WHERE name = ?" (t, n)

logInfo :: TimedFastLogger -> Text -> IO ()
logInfo logger = logWithSeverity logger "INFO"

logWarn :: TimedFastLogger -> Text -> IO ()
logWarn logger = logWithSeverity logger "WARN"

logWithSeverity :: ToLogStr msg => TimedFastLogger -> ByteString -> msg -> IO ()
logWithSeverity logger severity msg = logger $ \ft -> metadata ft <> message
  where metadata ft = foldMap toLogStr ["[" :: ByteString, ft, " " :: ByteString, severity, "] " :: ByteString]
        message = toLogStr msg <> toLogStr ("\n" :: ByteString)
