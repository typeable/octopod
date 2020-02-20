{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DMC.Lib
    ( runDMC
    ) where

import Prelude hiding (unlines)

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (pack, unlines, unpack)
import Options.Generic
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutMicro)
import Network.URI
import Servant.API
import Servant.Client
import System.Environment (lookupEnv)
import System.IO
import System.IO.Temp
import System.Process.Typed

import API.Lib (Deployment(Deployment), DeploymentAPI)

data Args
  = Create { name :: Text, tag :: Text, envs :: [Text] }
  | List
  | Edit { name :: Text }
  | Destroy { name :: Text }
  | Update { name :: Text, tag :: Text }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers $ defaultModifiers { shortNameModifier = firstLetter }

runDMC :: IO ()
runDMC = do
  args <- getRecord "DMC"

  manager <- newManager defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro $ 20 * 60 * 10^6 }
  env <- getBaseUrl
  handleCommand (mkClientEnv manager env) args

getBaseUrl :: IO BaseUrl
getBaseUrl = do
  dmsURL <- lookupEnv "DMS_URL"
  return $ maybe defaultBaseUrl parseUrl dmsURL

  where
    defaultBaseUrl = BaseUrl Http "dm.kube.thebestagent.pro" 80 ""

    parseUrl = fromMaybe defaultBaseUrl . parseUrl'

    parseUrl' url = do
      uri <- parseURI url
      uriData <- uriAuthority uri
      let s = if uriScheme uri == "https:" then Https else Http
          h = uriRegName uriData
          p = read . tail . uriPort $ uriData
      return $ BaseUrl s h p ""

handleCommand clientEnv Create {name, tag, envs}
  = handleResponse =<< runClientM (create $ Deployment name tag envs) clientEnv

handleCommand clientEnv List = (\l -> handleResponse $ unlines <$> l) =<< runClientM list clientEnv

handleCommand clientEnv Edit {name} = do
  editor <- lookupEnv "EDITOR"
  case editor of
    Just ed -> do
      res <- flip runClientM clientEnv $ get name
      case res of
        Right ((Deployment _ t e):_) -> editEnvs ed e >>= sendUpdate t
        Right [] -> print $ "deployment " ++ unpack name ++ " not found"
        Left err -> print $ "request failed, reason: " ++ show err
    Nothing -> print "environment variable $EDITOR not found"

  where
    editEnvs ed e = withTempFile "" "dmc" $ \p h -> do
      mapM_ (hPutStrLn h) (fmap unpack e)
      hFlush h
      withProcessWait_ (proc ed [p]) $ \_ -> putStrLn "sending update..."
      fmap pack . lines <$> readFile p

    sendUpdate t e = handleResponse =<< runClientM (edit name $ Deployment name t e) clientEnv

handleCommand clientEnv Destroy {name} = handleResponse =<< runClientM (destroy name) clientEnv

handleCommand clientEnv Update {name, tag}
  = handleResponse =<< runClientM (update name $ Deployment name tag envs) clientEnv
  where envs = ["UNUSED_KEY=UNUSED_VALUE"]

deploymentAPI :: Proxy DeploymentAPI
deploymentAPI = Proxy

list :<|> create :<|> get :<|> edit :<|> destroy :<|> update = client deploymentAPI

list :: ClientM [Text]

create :: Deployment -> ClientM Text

get :: Text -> ClientM [Deployment]

edit :: Text -> Deployment -> ClientM Text

destroy :: Text -> ClientM Text

update :: Text -> Deployment -> ClientM Text

handleResponse (Right result) = putStrLn (unpack result) >> putStrLn "done"
handleResponse (Left err) = putStrLn $ "command failed, reason: " ++ show err
