module DMC (runDMC) where

import Chronos
import Control.Lens hiding (List)
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Generics.Product
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text as T
import Data.Text.IO as T
import Data.Text.Lens
import Network.HTTP.Client
  ( defaultManagerSettings, managerResponseTimeout, newManager
  , responseTimeoutMicro)
import Network.URI
import Options.Generic
import Prelude as P
import Servant.API
import Servant.Client
  ( ClientError, BaseUrl(..), ClientEnv, ClientM, runClientM, client
  , mkClientEnv, Scheme(..))
import System.Environment (lookupEnv)
import System.IO
import System.IO.Temp
import System.Process.Typed

import API
import Types


-- | CLI options
-- FIXME: add options documentation
data Args
  = Create { name :: Text, tag :: Text, envs :: [Text] }
  | List
  | Edit { name :: Text }
  | Destroy { name :: Text }
  | Update { name :: Text, tag :: Text }
  | Info { name :: Text }
  deriving stock (Show, Generic)

instance ParseRecord Args where
  -- FIXME: external CLI interface depends on the naming in the code, yuck
  parseRecord = parseRecordWithModifiers $ defaultModifiers
    { shortNameModifier = firstLetter }

runDMC :: IO ()
runDMC = do
  args <- getRecord "DMC"
  manager <- newManager defaultManagerSettings
    { managerResponseTimeout =
      responseTimeoutMicro $ 20 * 60 * 10 ^ (6 :: Int) }
  env <- getBaseUrl
  let clientEnv = mkClientEnv manager env
  case args of
    Create n t e      -> do
      envPairs <- parseEnvs e
      handleCreate clientEnv $ Deployment (coerce n) (coerce t) envPairs
    List              -> handleList clientEnv
    Edit tName        -> handleEdit clientEnv $ DeploymentName tName
    Destroy tName     -> handleDestroy clientEnv $ DeploymentName tName
    Update tName tTag -> handleUpdate clientEnv
      (DeploymentName tName)
      (DeploymentTag tTag)
    Info tName        -> handleInfo clientEnv $ DeploymentName tName

getBaseUrl :: IO BaseUrl
getBaseUrl = do
  -- FIXME: move to CLI options
  dmsURL <- lookupEnv "DMS_URL"
  let
    -- FIXME: unhardcode
    defaultBaseUrl :: BaseUrl
    defaultBaseUrl = BaseUrl Http "dm.kube.thebestagent.pro" 80 ""
    parseUrl :: String -> BaseUrl
    parseUrl = fromMaybe defaultBaseUrl . parseUrl'
     where
       parseUrl' url = do
         uri <- parseURI url
         uriData <- uriAuthority uri
         let
           s = if uriScheme uri == "https:" then Https else Http
           h = uriRegName uriData
           p = read . P.tail . uriPort $ uriData
         return $ BaseUrl s h p ""
  return $ maybe defaultBaseUrl parseUrl dmsURL

handleCreate :: ClientEnv -> Deployment -> IO ()
handleCreate clientEnv createCmd = do
  response <- runClientM (createH createCmd) clientEnv
  handleResponse (const $ pure ()) response

handleList :: ClientEnv -> IO ()
handleList clientEnv = do
  l <- runClientM listH clientEnv
  handleResponse T.putStr $ T.unlines . coerce <$> l

handleEdit :: ClientEnv -> DeploymentName -> IO ()
handleEdit clientEnv dName = do
  editor <- lookupEnv "EDITOR"
  case editor of
    Just ed -> do
      res <- flip runClientM clientEnv $ getH dName
      case res of
        Right (Deployment _ _ e) -> do
          envPairs <- editEnvs ed e
          response <- runClientM (editH dName envPairs) clientEnv
          handleResponse (const $ pure ()) response
        Left err ->
          P.print $ "request failed, reason: " <> show err
    Nothing -> error "environment variable $EDITOR not found"

editEnvs :: FilePath -> EnvPairs -> IO EnvPairs
editEnvs ed e = withTempFile "" "dmc" $ \p h -> do
  for_ e $ \(k,v) -> (T.hPutStrLn h $ k <> "=" <> v)
  hFlush h
  withProcessWait_ (proc ed [p]) $ \_ -> T.hPutStrLn h "processing update..."
  l <- T.lines <$> T.readFile p
  envPairs <- parseEnvs l
  T.putStrLn "sending update..."
  pure envPairs

handleDestroy :: ClientEnv -> DeploymentName -> IO ()
handleDestroy clientEnv dName = do
  handleResponse (const $ pure ()) =<< runClientM (destroyH dName) clientEnv

handleUpdate :: ClientEnv -> DeploymentName -> DeploymentTag -> IO ()
handleUpdate clientEnv dName dTag = do
  response <- runClientM (updateH dName dTag) clientEnv
  handleResponse (const $ pure ()) response

handleInfo :: ClientEnv -> DeploymentName -> IO ()
handleInfo clientEnv dName = do
  res <- runClientM (infoH dName) clientEnv
  case res of
    Right (i : _) -> printInfo i
    Right [] -> print $ "deployment " ++ unpack (coerce dName) ++ " not found"
    Left err -> print $ "request failed, reason: " ++ show err

listH :: ClientM [DeploymentName]

createH :: Deployment -> ClientM NoContent

getH :: DeploymentName -> ClientM Deployment

editH :: DeploymentName -> EnvPairs -> ClientM NoContent

destroyH :: DeploymentName -> ClientM NoContent

updateH :: DeploymentName -> DeploymentTag -> ClientM NoContent

infoH :: DeploymentName -> ClientM [DeploymentInfo]

(listH
 :<|> createH
 :<|> getH
 :<|> editH
 :<|> destroyH
 :<|> updateH
 :<|> infoH)
 :<|> _ = client (Proxy @API)

handleResponse :: (a -> IO ()) -> Either ClientError a -> IO ()
handleResponse f (Right result) = f result
handleResponse _ (Left err) =
  T.putStrLn $ "command failed, reason: " <> T.pack (show err)

printInfo :: DeploymentInfo -> IO ()
printInfo (DeploymentInfo (Deployment dName dTag envPairs) dLogs) = do
  T.putStrLn "Current settings:"
  T.putStrLn $ "tag: " <> coerce dTag
  T.putStrLn $ "envs: " <> formatEnvPairs envPairs
  T.putStrLn
    $ "URL: https://" <> coerce dName <> ".kube.thebestagent.pro"
  T.putStrLn ""
  T.putStrLn "Last logs:"
  mapM_ ppDeploymentLog dLogs

ppDeploymentLog :: DeploymentLog -> IO ()
ppDeploymentLog dLog = T.putStrLn . T.unwords $
  [ encode_YmdHMS SubsecondPrecisionAuto w3c
    (timeToDatetime . Time . fromIntegral
      $ dLog ^. field @"createdAt" * 10 ^ (9 :: Int))
  , dLog ^. field @"action" . coerced
  , dLog ^. field @"deploymentTag" . coerced
  , dLog ^. field @"deploymentEnvs" . to formatEnvPairs
  , dLog ^. field @"exitCode" . re _Show . packed
  ]
