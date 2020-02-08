{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}

module DMS.Lib
    ( runDMS
    ) where

import Control.Monad.IO.Class
import Data.Text (unpack)
import Options.Generic
import Network.Wai.Handler.Warp
import Servant

import API.Lib (Deployment(Deployment), DeploymentAPI)

data Args
  = Args { port :: Int, db :: Text, db_pool_size :: Int }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers $ defaultModifiers { fieldNameModifier = fmap (\c -> if c == '_' then '-' else c) }

runDMS :: IO ()
runDMS = do
  args <- getRecord "DMS"
  print (args :: Args)
  run (port args) app

app :: Application
app = serve deploymentAPI server

deploymentAPI :: Proxy DeploymentAPI
deploymentAPI = Proxy

server :: Server DeploymentAPI
server = list :<|> create :<|> edit :<|> destroy :<|> update

list :: Handler [Text]
list = do
  liftIO . putStrLn $ "get deployments: " ++ show deployments
  return deployments

create :: Deployment -> Handler Text
create d = do
  liftIO . putStrLn $ "deployment created, deployment: " ++ show d
  return ""

edit :: Text -> Deployment -> Handler Text
edit n d = do
  liftIO . putStrLn $ "deployment edited, name: " ++ unpack n ++ ", deployment: " ++ show d
  return ""

destroy :: Text -> Handler Text
destroy n = do
  liftIO . putStrLn $ "deployment destroyed, name: " ++ unpack n
  return ""

update :: Text -> Deployment -> Handler Text
update n d = do
  liftIO . putStrLn $ "deployment updated, name: " ++ unpack n ++ ", deployment: " ++ show d
  return ""

deployments = ["foo", "bar", "baz"]
