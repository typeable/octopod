{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module DMC.Lib
    ( runDMC
    ) where

import Prelude hiding (unlines, unwords)

import Chronos
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (pack, unlines, unpack, unwords)
import Options.Generic
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutMicro)
import Network.URI
import Servant.API
import Servant.Client
import System.Environment (lookupEnv)
import System.IO
import System.IO.Temp
import System.Process.Typed

import API.Lib ( Deployment (Deployment)
               , DeploymentAPI
               , DeploymentInfo (DeploymentInfo)
               , deployment
               , logs
               , DeploymentLog (DeploymentLog)
               , action
               , deploymentTag
               , deploymentEnvs
               , exitCode
               , createdAt
               )

data Args
  = Create { name :: Text, tag :: Text, envs :: [Text] }
  | List
  | Edit { name :: Text }
  | Destroy { name :: Text }
  | Update { name :: Text, tag :: Text }
  | Info { name :: Text }
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
        Right (Deployment _ t e : _) -> editEnvs ed e >>= sendUpdate t
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

handleCommand clientEnv Info {name} = do
  res <- runClientM (info name) clientEnv
  case res of
    Right (i : _) -> printInfo i
    Right [] -> print $ "deployment " ++ unpack name ++ " not found"
    Left err -> print $ "request failed, reason: " ++ show err

deploymentAPI :: Proxy DeploymentAPI
deploymentAPI = Proxy

(list :<|> create :<|> get :<|> edit :<|> destroy :<|> update :<|> info) :<|> ping = client deploymentAPI

list :: ClientM [Text]

create :: Deployment -> ClientM Text

get :: Text -> ClientM [Deployment]

edit :: Text -> Deployment -> ClientM Text

destroy :: Text -> ClientM Text

update :: Text -> Deployment -> ClientM Text

info :: Text -> ClientM [DeploymentInfo]

ping :: ClientM Text

handleResponse (Right result) = putStrLn (unpack result) >> putStrLn "done"
handleResponse (Left err) = putStrLn $ "command failed, reason: " ++ show err

printInfo DeploymentInfo {deployment, logs} = do
  let Deployment n t e = deployment
  putStrLn "Current settings:"
  putStrLn $ "tag: " ++ unpack t
  putStrLn $ "envs: " ++ unpack (unwords e)
  putStrLn $ "URL: " ++ unpack n ++ ".kube.thebestagent.pro"
  putStrLn ""
  putStrLn "Last logs:"
  mapM_ ppDeploymentLog $ reverse logs

ppDeploymentLog DeploymentLog {action, deploymentTag, deploymentEnvs, exitCode, createdAt}
  = putStrLn . unpack . unwords $ [ encode_YmdHMS SubsecondPrecisionAuto w3c (timeToDatetime . Time . fromIntegral $ createdAt * 10^9)
                                  , action
                                  , deploymentTag
                                  , unwords deploymentEnvs
                                  , pack $ show exitCode
                                  ]
