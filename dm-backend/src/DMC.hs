module DMC (runDMC) where

import           Chronos
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import qualified Data.ByteString.Char8 as BSC
import           Data.Coerce
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

import           API
import           Common.Utils (dfiName)
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
      CreateC tName tTag tSetAp tSetSt tSetPAp tSetPSt _ _               -> do
        setApp <- liftIO $ parseSetApplicationOverrides Public tSetAp
        setSt <- liftIO $ parseSetStagingOverrides Public tSetSt
        setPApp <- liftIO $ parseSetApplicationOverrides Private tSetPAp
        setPSt <- liftIO $ parseSetStagingOverrides Private tSetPSt
        let
          appOvs = setApp ++ setPApp
          stOvs  = setSt ++ setPSt
        handleCreate $ Deployment (coerce tName) (coerce tTag) appOvs stOvs
      ListC _ _                                                          ->
        handleList
      DeleteC tName _  _                                                 ->
        handleDelete . coerce $ tName
      UpdateC tName tTag tSetAp tUnsAp tSetSt tUnsSt tSetPAp tSetPSt _ _ -> do
        setApp <- liftIO $ parseSetApplicationOverrides Public tSetAp
        setSt <- liftIO $ parseSetStagingOverrides Public tSetSt
        unsetApp <- liftIO $ parseUnsetApplicationOverrides Public tUnsAp
        unsetSt <- liftIO $ parseUnsetStagingOverrides Public tUnsSt
        setPApp <- liftIO $ parseSetApplicationOverrides Private tSetPAp
        setPSt <- liftIO $ parseSetStagingOverrides Private tSetPSt
        let
          appOvs = setApp ++ setPApp
          stOvs  = setSt ++ setPSt
        handleUpdate (coerce tName) (coerce tTag) appOvs unsetApp stOvs unsetSt
      InfoC tName _ _                                                    ->
        handleInfo . coerce $ tName
      CleanupC tName _ _                                                 ->
        handleCleanup . coerce $ tName
      RestoreC tName _ _                                                 ->
        handleRestore . coerce $ tName
      CleanArchiveC _ _                                                  ->
        handleCleanArchive

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
      getName dep = dep ^.dfiName
      names = T.unlines . coerce . (getName <$>) <$> resp
    handleResponse T.putStr names

handleDelete :: DeploymentName -> ReaderT ClientEnv IO ()
handleDelete dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (deleteH dName) clientEnv

handleUpdate
  :: DeploymentName
  -> DeploymentTag
  -> ApplicationOverrides
  -> ApplicationOverrides
  -> StagingOverrides
  -> StagingOverrides
  -> ReaderT ClientEnv IO ()
handleUpdate dName dTag dNewAppOvs dOldAppOvs dNewStOvs dOldStOvs = do
  clientEnv <- ask
  liftIO $ do
    let
      dUpdate = DeploymentUpdate
        { newTag = dTag
        , newAppOverrides = dNewAppOvs
        , oldAppOverrides = dOldAppOvs
        , newStagingOverrides = dNewStOvs
        , oldStagingOverrides = dOldStOvs }
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

createH :: Deployment -> ClientM CommandResponse

deleteH :: DeploymentName -> ClientM CommandResponse

updateH :: DeploymentName -> DeploymentUpdate -> ClientM CommandResponse

infoH :: DeploymentName -> ClientM [DeploymentInfo]

_fullInfoH :: DeploymentName -> ClientM DeploymentFullInfo

_statusH :: DeploymentName -> ClientM CurrentDeploymentStatus

cleanupH :: DeploymentName -> ClientM CommandResponse

restoreH :: DeploymentName -> ClientM CommandResponse

_getActionInfoH :: ActionId -> ClientM ActionInfo

_pingH :: ClientM NoContent

cleanArchiveH :: ClientM CommandResponse

_projectName :: ClientM ProjectName

( listH
  :<|> createH
  :<|> deleteH
  :<|> updateH
  :<|> infoH
  :<|> _fullInfoH
  :<|> _statusH
  :<|> cleanupH
  :<|> restoreH)
    :<|> _getActionInfoH
    :<|> _pingH
    :<|> cleanArchiveH
    :<|> _projectName = client (Proxy @PowerAPI)

handleResponse :: (a -> IO ()) -> Either ClientError a -> IO ()
handleResponse f (Right result)                    = f result
handleResponse _ (Left (FailureResponse _req res)) =
  T.putStrLn $ "error: " <> T.pack (show $ responseBody res)
handleResponse _ (Left err)                        =
  T.putStrLn $ "command failed due to unknown reason: " <> T.pack (show err)

printInfo :: DeploymentInfo -> IO ()
printInfo (DeploymentInfo (Deployment dName dTag dAppOvs dStOvs) dLogs) = do
  T.putStrLn "Current settings:"
  T.putStrLn $ "tag: " <> coerce dTag
  T.putStrLn $ "application overrides: "
    <> (formatOverrides $ coerce <$> dAppOvs)
  T.putStrLn $ "staging overrides: "
    <> (formatOverrides $ coerce <$> dStOvs)
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
  , dLog ^. field @"deploymentAppOverrides"
    . to (formatOverrides . (coerce <$>))
  , dLog ^. field @"deploymentStagingOverrides"
    . to (formatOverrides . (coerce <$>))
  , dLog ^. field @"exitCode" . re _Show . packed
  ]
