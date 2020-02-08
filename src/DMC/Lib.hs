{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DMC.Lib
    ( runDMC
    ) where

import Data.Proxy
import Options.Generic
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import System.Environment (lookupEnv)

import API.Lib (Deployment(Deployment), DeploymentAPI)

data Args
  = Create { name :: Text, template :: Text, envs :: [Text] }
  | List
  | Edit { name :: Text }
  | Destroy { name :: Text }
  | Update { name :: Text, template :: Text }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers $ defaultModifiers { shortNameModifier = firstLetter }

runDMC :: IO ()
runDMC = do
  args <- getRecord "DMC"
  print (args :: Args)

  let port = 4000
  manager <- newManager defaultManagerSettings
  env <- getBaseUrl
  handleCommand (mkClientEnv manager env) args

getBaseUrl :: IO BaseUrl
getBaseUrl = do
  dmsURL <- lookupEnv "DMS_URL"
  return $ case dmsURL of
    Just _url -> defaultBaseUrl
    Nothing -> defaultBaseUrl

defaultBaseUrl = BaseUrl Http "localhost" 4000 ""

handleCommand clientEnv Create {name, template, envs} = do
  res <- flip runClientM clientEnv $ create $ Deployment name template envs
  print res

handleCommand clientEnv List = do
  res <- runClientM list clientEnv
  print res

handleCommand clientEnv Edit {name} = do
  res <- flip runClientM clientEnv $ edit name $ Deployment "n" "t" ["SOME_KEY=SOME_VAL"]
  print res

handleCommand clientEnv Destroy {name} = do
  res <- flip runClientM clientEnv $ destroy name
  print res

handleCommand clientEnv Update {name, template} = do
  res <- flip runClientM clientEnv $ update name $ Deployment name template ["SOME_KEY=SOME_VAL"]
  print res

deploymentAPI :: Proxy DeploymentAPI
deploymentAPI = Proxy

list :<|> create :<|> get :<|> edit :<|> destroy :<|> update = client deploymentAPI

list :: ClientM [Text]

create :: Deployment -> ClientM Text

get :: Text -> ClientM Deployment

edit :: Text -> Deployment -> ClientM Text

destroy :: Text -> ClientM Text

update :: Text -> Deployment -> ClientM Text
