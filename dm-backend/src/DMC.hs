module DMC (runDMC) where

import           Chronos
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Char8 as BSC
import           Data.Coerce
import           Data.Foldable
import           Data.Generics.Product
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Text as T
import           Data.Text.IO as T
import           Data.Text.Lens
import           Network.URI
import           Prelude as P
import           Servant.API
import           Servant.Client
  (BaseUrl(..), ClientError, ClientEnv, ClientM, Scheme(..)
  , runClientM, client, mkClientEnv)
import           Servant.Client.Core
  (ClientError(FailureResponse), ResponseF(..))
import           System.Environment (lookupEnv)
import           System.IO
import           System.IO.Temp
import           System.Process.Typed

import           Common.API
import           DMC.Args
import           TLS (makeClientManager)
import           Types

runDMC :: IO ()
runDMC = do
  args <- parseArgs
  let
    cert = maybe "cert.pem" (BSC.unpack . unTLSCertPath) $ dmcTLSCertPath args
    key = maybe "key.pem" (BSC.unpack . unTLSKeyPath) $ dmcTLSKeyPath args
  manager <- makeClientManager dmsHostName cert key
  env <- getBaseUrl
  let clientEnv = mkClientEnv manager env
  flip runReaderT clientEnv $
    case args of
      CreateC  tName tTag tEnvs _ _ -> do
        envPairs <- liftIO $ parseEnvs tEnvs
        handleCreate $ Deployment (coerce tName) (coerce tTag) envPairs
      ListC         _     _        -> handleList
      EditC         tName _    _   -> handleEdit . coerce $ tName
      DeleteC       tName _    _   -> handleDelete . coerce $ tName
      UpdateC       tName tTag _ _ -> handleUpdate (coerce tName) (coerce tTag)
      InfoC         tName _    _   -> handleInfo . coerce $ tName
      CleanupC      tName _    _   -> handleCleanup . coerce $ tName
      RestoreC      tName _    _   -> handleRestore . coerce $ tName
      CleanArchiveC _     _        -> handleCleanArchive

dmsHostName :: String
dmsHostName = "dm.stage.thebestagent.pro"

getBaseUrl :: IO BaseUrl
getBaseUrl = do
  -- FIXME: move to CLI options
  dmsURL <- lookupEnv "DMS_URL"
  let
    -- FIXME: unhardcode
    defaultBaseUrl :: BaseUrl
    defaultBaseUrl = BaseUrl Https dmsHostName 443 ""
    parseUrl :: String -> BaseUrl
    parseUrl = fromMaybe defaultBaseUrl . parseUrl'
      where
        parseUrl' url = do
          uri <- parseURI url
          uriData <- uriAuthority uri
          let
            schema = if uriScheme uri == "https:" then Https else Http
            host = uriRegName uriData
            port = read . P.tail . uriPort $ uriData
          return $ BaseUrl schema host port ""
  return $ maybe defaultBaseUrl parseUrl dmsURL

handleCreate :: Deployment -> ReaderT ClientEnv IO ()
handleCreate createCmd = do
  clientEnv <- ask
  liftIO $ do
    response <- runClientM (createH createCmd) clientEnv
    handleResponse (const $ pure ()) response

handleList :: ReaderT ClientEnv IO ()
handleList = do
  clientEnv <- ask
  liftIO $ do
    resp <- runClientM listH clientEnv
    let
      getName (DeploymentFullInfo (Deployment dName _ _) _ _ _ _) = dName
      names = T.unlines . coerce . (getName <$>) <$> resp
    handleResponse T.putStr names

handleEdit :: DeploymentName -> ReaderT ClientEnv IO ()
handleEdit dName = do
  clientEnv <- ask
  liftIO $ do
    editorPath <- lookupEnv "EDITOR"
    case editorPath of
      Just editor -> do
        res <- flip runClientM clientEnv $ getH dName
        case res of
          Right (Deployment _ _ dEnvs) -> do
            envPairs <- editEnvs editor dEnvs
            response <- runClientM (editH dName envPairs) clientEnv
            handleResponse (const $ pure ()) response
          Left err                     ->
            P.print $ "request failed, reason: " <> show err
      Nothing     -> error "environment variable $EDITOR not found"

editEnvs :: FilePath -> EnvPairs -> IO EnvPairs
editEnvs editor currentEnvPairs = withTempFile "" "dmc" $ \filePath handle -> do
  for_ currentEnvPairs $ \(k, v) -> (T.hPutStrLn handle $ k <> "=" <> v)
  hFlush handle
  withProcessWait_ (proc editor [filePath]) $ \_ ->
    T.hPutStrLn handle "processing update..."
  rawEnvs <- T.lines <$> T.readFile filePath
  envPairs <- parseEnvs rawEnvs
  T.putStrLn "sending update..."
  pure envPairs

handleDelete :: DeploymentName -> ReaderT ClientEnv IO ()
handleDelete dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (deleteH dName) clientEnv

handleUpdate :: DeploymentName -> DeploymentTag -> ReaderT ClientEnv IO ()
handleUpdate dName dTag = do
  clientEnv <- ask
  liftIO $ do
    let dUpdate = DeploymentUpdate { newTag = dTag, newEnvs = Nothing }
    response <- runClientM (updateH dName dUpdate) clientEnv
    handleResponse (const $ pure ()) response

handleInfo :: DeploymentName -> ReaderT ClientEnv IO ()
handleInfo dName = do
  clientEnv <- ask
  liftIO $ do
    res <- runClientM (infoH dName) clientEnv
    case res of
      Right (i : _) -> printInfo i
      Right []      -> print notFoundMsg
      Left err      -> print $ "request failed, reason: " ++ show err
    where
      notFoundMsg = "deployment " ++ unpack (coerce dName) ++ " not found"

handleCleanup :: DeploymentName -> ReaderT ClientEnv IO ()
handleCleanup dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (cleanupH dName) clientEnv

handleRestore :: DeploymentName -> ReaderT ClientEnv IO ()
handleRestore dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (restoreH dName) clientEnv

handleCleanArchive :: ReaderT ClientEnv IO ()
handleCleanArchive = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM cleanArchiveH clientEnv

listH :: ClientM [DeploymentFullInfo]

createH :: Deployment -> ClientM NoContent

getH :: DeploymentName -> ClientM Deployment

editH :: DeploymentName -> EnvPairs -> ClientM NoContent

deleteH :: DeploymentName -> ClientM NoContent

updateH :: DeploymentName -> DeploymentUpdate -> ClientM NoContent

infoH :: DeploymentName -> ClientM [DeploymentInfo]

_statusH :: DeploymentName -> ClientM DeploymentStatus

cleanupH :: DeploymentName -> ClientM NoContent

restoreH :: DeploymentName -> ClientM NoContent

_pingH :: ClientM NoContent

cleanArchiveH :: ClientM NoContent

( listH
  :<|> createH
  :<|> getH
  :<|> editH
  :<|> deleteH
  :<|> updateH
  :<|> infoH
  :<|> _statusH
  :<|> cleanupH
  :<|> restoreH)
    :<|> _pingH :<|> cleanArchiveH = client (Proxy @API)

handleResponse :: (a -> IO ()) -> Either ClientError a -> IO ()
handleResponse f (Right result)                    = f result
handleResponse _ (Left (FailureResponse _req res)) =
  T.putStrLn $ "error: " <> T.pack (show $ responseBody res)
handleResponse _ (Left err)                        =
  T.putStrLn $ "command failed due to unknown reason: " <> T.pack (show err)

printInfo :: DeploymentInfo -> IO ()
printInfo (DeploymentInfo (Deployment dName dTag envPairs) dLogs) = do
  T.putStrLn "Current settings:"
  T.putStrLn $ "tag: " <> coerce dTag
  T.putStrLn $ "envs: " <> formatEnvPairs envPairs
  T.putStrLn
    $ "URL: https://" <> coerce dName <> ".stage.thebestagent.pro"
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
