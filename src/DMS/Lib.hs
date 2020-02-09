{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module DMS.Lib
    ( runDMS
    ) where

import Prelude hiding (unwords, words)

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Text (unpack, unwords, words)
import Data.Pool
import Database.PostgreSQL.Simple
import Options.Generic
import Network.Wai.Handler.Warp
import Servant

import API.Lib (Deployment(Deployment), DeploymentAPI)

data Args
  = Args { port :: Int, db :: ByteString, dbPoolSize :: Int }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

type PgPool = Pool Connection

runDMS :: IO ()
runDMS = do
  args <- getRecord "DMS"
  print (args :: Args)
  pool <- initConnectionPool (db args) (dbPoolSize args)
  run (port args) (app pool)

initConnectionPool :: ByteString -> Int -> IO PgPool
initConnectionPool db = createPool (connectPostgreSQL db) close 1 30

app :: PgPool -> Application
app pool = serve deploymentAPI (server pool)

deploymentAPI :: Proxy DeploymentAPI
deploymentAPI = Proxy

server :: PgPool -> Server DeploymentAPI
server p = list p :<|> create p :<|> get p :<|> edit p :<|> destroy p :<|> update p

list :: PgPool -> Handler [Text]
list p = do
  ds <- getDeployments
  liftIO . putStrLn $ "get deployments: " ++ show ds
  return ds

  where
    getDeployments :: Handler [Text]
    getDeployments = fmap (fmap fromOnly) . liftIO $
      withResource p $ \conn -> query_ conn "SELECT name FROM deployments"

create :: PgPool -> Deployment -> Handler Text
create p d = do
  createDeployment d
  liftIO . putStrLn $ "deployment created, deployment: " ++ show d
  return ""

  where
    createDeployment :: Deployment -> Handler Int64
    createDeployment (Deployment n t e) = liftIO $
      withResource p $ \conn ->
        execute conn "INSERT INTO deployments (name, template, envs) VALUES (?, ?, ?)" (n, t, unwords e)

get :: PgPool -> Text -> Handler [Deployment]
get p n = do
  d <- getDeployment
  liftIO . putStrLn $ "get deployment: " ++ show d
  return d

  where
    getDeployment :: Handler [Deployment]
    getDeployment = fmap (fmap (\(n, t, e) -> Deployment n t $ words e)) . liftIO $
      withResource p $ \conn -> query conn "SELECT name, template, envs FROM deployments WHERE name = ?" (Only n)

edit :: PgPool -> Text -> Deployment -> Handler Text
edit p n (Deployment _ _ e) = do
  updateDeployment
  liftIO . putStrLn $ "deployment edited, name: " ++ unpack n ++ ", envs: " ++ (unpack . unwords $ e)
  return ""

  where
    updateDeployment :: Handler Int64
    updateDeployment = liftIO $
      withResource p $ \conn ->
        execute conn "UPDATE deployments SET envs = ?, updated_at = now() WHERE name = ?" (unwords e, n)

destroy :: PgPool -> Text -> Handler Text
destroy p n = do
  deleteDeployment
  liftIO . putStrLn $ "deployment destroyed, name: " ++ unpack n
  return ""

  where
    deleteDeployment :: Handler Int64
    deleteDeployment = liftIO $
      withResource p $ \conn ->
        execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

update :: PgPool -> Text -> Deployment -> Handler Text
update p n (Deployment _ t _) = do
  updateDeployment
  liftIO . putStrLn $ "deployment updated, name: " ++ unpack n ++ ", template: " ++ unpack t
  return ""

  where
    updateDeployment :: Handler Int64
    updateDeployment = liftIO $
      withResource p $ \conn ->
        execute conn "UPDATE deployments SET template = ?, updated_at = now() WHERE name = ?" (t, n)
