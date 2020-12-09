module Octopod.CLI (runOcto) where

import           Chronos
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson (decode)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import           Data.Coerce
import           Data.Generics.Labels ()
import           Data.Generics.Product
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Text.Lens
import           GHC.IO.Encoding
import           Network.HTTP.Client (Manager)
import           Prelude as P
import           Servant.API
import           Servant.Client
  (BaseUrl(..), ClientEnv, ClientError, ClientM, client, mkClientEnv,
  runClientM)
import           Servant.Client.Core
  (ClientError(FailureResponse), ResponseF(..), parseBaseUrl)
import           System.Environment (lookupEnv)
import           System.Exit

import           Common.Types
import           Common.Utils (dfiName)
import           Octopod.CLI.Args
import           Octopod.CLI.TLS (makeClientManager)
import           Octopod.PowerAPI
import           Text.Layout.Table
import           Text.Layout.Table.Extras ()

-- | Runs the octo CLI.
runOcto :: IO ()
runOcto = do
  setLocaleEncoding utf8
  args <- parseArgs
  env <- getBaseUrl
  manager <- initClientManager env
  let clientEnv = mkClientEnv manager env
  flip runReaderT clientEnv $
    case args of
      Create tName tTag tSetAp tSetDep tSetPAp tSetPDep -> do
        setApp <- liftIO $ parseSetApplicationOverrides Public tSetAp
        setDep <- liftIO $ parseSetDeploymentOverrides Public tSetDep
        setPApp <- liftIO $ parseSetApplicationOverrides Private tSetPAp
        setPDep <- liftIO $ parseSetDeploymentOverrides Private tSetPDep
        let
          appOvs = setApp ++ setPApp
          depOvs = setDep ++ setPDep
        handleCreate $ Deployment (coerce tName) (coerce tTag) appOvs depOvs
      List ->
        handleList
      Archive tName ->
        handleArchive . coerce $ tName
      Update tName tTag tSetAp tUnsAp tSetD tUnsD tSetPAp tSetPD -> do
        setApp <- liftIO $ parseSetApplicationOverrides Public tSetAp
        setDep <- liftIO $ parseSetDeploymentOverrides Public tSetD
        unsetApp <- liftIO $ parseUnsetApplicationOverrides Public tUnsAp
        unsetDep <- liftIO $ parseUnsetDeploymentOverrides Public tUnsD
        setPApp <- liftIO $ parseSetApplicationOverrides Private tSetPAp
        setPDep <- liftIO $ parseSetDeploymentOverrides Private tSetPD
        let
          appOvs = setApp ++ setPApp
          depOvs = setDep ++ setPDep
          tName' = coerce tName
          tTag' = coerce tTag
        handleUpdate tName' tTag' appOvs unsetApp depOvs unsetDep
      Info tName ->
        handleInfo . coerce $ tName
      Cleanup tName ->
        handleCleanup . coerce $ tName
      Restore tName ->
        handleRestore . coerce $ tName
      CleanArchive ->
        handleCleanArchive
      GetActionLogs aId l -> handleGetActionInfo aId l

-- | Initializes the HTTP connection manager.
initClientManager :: BaseUrl -> IO Manager
initClientManager baseURL = do
  let
  cert <- lookupEnv "TLS_CERT_PATH"
    >>= maybe (pure $ TLSCertPath "cert.pem") (pure . TLSCertPath . BSC.pack)
  key <- lookupEnv "TLS_KEY_PATH"
    >>= maybe (pure $ TLSKeyPath "key.pem") (pure . TLSKeyPath . BSC.pack)
  makeClientManager (baseUrlHost baseURL) cert key

-- | Returns BaseUrl from 'OCTOPOD_URL' environment variable
-- or exit with exit_code=1.
getBaseUrl :: IO BaseUrl
getBaseUrl = do
  octopodURL <- lookupEnv "OCTOPOD_URL"
    >>= maybe (die "OCTOPOD_URL is not set") pure
  case parseBaseUrl octopodURL of
    Just url -> pure url
    Nothing -> die "could not parse OCTOPOD_URL"

-- | Handles the 'create' subcommand.
handleCreate :: Deployment -> ReaderT ClientEnv IO ()
handleCreate createCmd = do
  clientEnv <- ask
  liftIO $ do
    response <- runClientM (createH createCmd) clientEnv
    handleResponse (const $ pure ()) response

-- | Handles the 'list' subcommand.
handleList :: ReaderT ClientEnv IO ()
handleList = do
  clientEnv <- ask
  liftIO $ do
    resp <- runClientM listH clientEnv
    let
      getName dep = dep ^.dfiName
      names = T.unlines . coerce . (getName <$>) <$> resp
    handleResponse T.putStr names

-- | Handles the 'archive' subcommand.
handleArchive :: DeploymentName -> ReaderT ClientEnv IO ()
handleArchive dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (archiveH dName) clientEnv

-- | Handles the 'update' subcommand.
handleUpdate
  :: DeploymentName
  -> DeploymentTag
  -> ApplicationOverrides
  -> ApplicationOverrides
  -> DeploymentOverrides
  -> DeploymentOverrides
  -> ReaderT ClientEnv IO ()
handleUpdate dName dTag dNewAppOvs dOldAppOvs dNewDepOvs dOldDepOvs = do
  clientEnv <- ask
  liftIO $ do
    let
      dUpdate = DeploymentUpdate
        { newTag = dTag
        , newAppOverrides = dNewAppOvs
        , oldAppOverrides = dOldAppOvs
        , newDeploymentOverrides = dNewDepOvs
        , oldDeploymentOverrides = dOldDepOvs
        }
    response <- runClientM (updateH dName dUpdate) clientEnv
    handleResponse (const $ pure ()) response

-- | Handles the 'info' subcommand.
handleInfo :: DeploymentName -> ReaderT ClientEnv IO ()
handleInfo dName = do
  clientEnv <- ask
  liftIO $ do
    res <- runClientM (infoH dName) clientEnv
    case res of
      Right (i : _) -> printInfo i
      Right [] -> putStrLn notFoundMsg
      Left err -> putStrLn $ "request failed, reason: " ++ show err
    where
      notFoundMsg = "deployment " ++ T.unpack (coerce dName) ++ " not found"

-- | Handles the 'cleanup' subcommand.
handleCleanup :: DeploymentName -> ReaderT ClientEnv IO ()
handleCleanup dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (cleanupH dName) clientEnv

-- | Handles the 'restore' subcommand.
handleRestore :: DeploymentName -> ReaderT ClientEnv IO ()
handleRestore dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (restoreH dName) clientEnv

-- | Handles the 'clean-archive' subcommand.
handleCleanArchive :: ReaderT ClientEnv IO ()
handleCleanArchive = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM cleanArchiveH clientEnv

-- | Handles the 'logs' subcommand.
handleGetActionInfo :: ActionId -> LogOutput -> ReaderT ClientEnv IO ()
handleGetActionInfo aId l = do
  clientEnv <- ask
  liftIO $ do
    runClientM (getActionInfoH aId) clientEnv >>= \case
      Left err -> print err
      Right x -> case l of
        Out -> T.putStrLn $ x ^. #stdout
        Err -> T.putStrLn $ x ^. #stderr
        ErrOut -> do
          T.putStrLn "\t\tstdout:\n"
          T.putStrLn $ x ^. #stdout
          T.putStrLn "\t\tstderr:\n"
          T.putStrLn $ x ^. #stderr


listH :: ClientM [DeploymentFullInfo]

createH :: Deployment -> ClientM CommandResponse

archiveH :: DeploymentName -> ClientM CommandResponse

updateH :: DeploymentName -> DeploymentUpdate -> ClientM CommandResponse

infoH :: DeploymentName -> ClientM [DeploymentInfo]

_fullInfoH :: DeploymentName -> ClientM DeploymentFullInfo

_statusH :: DeploymentName -> ClientM CurrentDeploymentStatus

cleanupH :: DeploymentName -> ClientM CommandResponse

restoreH :: DeploymentName -> ClientM CommandResponse

getActionInfoH :: ActionId -> ClientM ActionInfo

cleanArchiveH :: ClientM CommandResponse

( listH
  :<|> createH
  :<|> archiveH
  :<|> updateH
  :<|> infoH
  :<|> _fullInfoH
  :<|> _statusH
  :<|> cleanupH
  :<|> restoreH)
    :<|> getActionInfoH
    :<|> cleanArchiveH = client (Proxy @PowerAPI)

-- | Handles response from Octopod Server.
handleResponse :: (a -> IO ()) -> Either ClientError a -> IO ()
handleResponse f (Right result) = f result
handleResponse _ (Left (FailureResponse _req res)) =
  T.putStrLn . decodeError $ responseBody res
handleResponse _ (Left err) =
  T.putStrLn $ "command failed due to unknown reason: " <> T.pack (show err)

decodeError :: LBSC.ByteString -> Text
decodeError body =
  case decode body of
    Just (ValidationError nameErrors tagErrors) ->
      T.concat ((<> "\n") <$> nameErrors) <> T.concat ((<> "\n") <$> tagErrors)
    Just (AppError errorMsg) -> errorMsg
    Just Success -> "ok"
    _ -> "error: " <> (T.pack . LBSC.unpack $ body)

-- | Pretty-prints the 'info' subcommand result.
printInfo :: DeploymentInfo -> IO ()
printInfo (DeploymentInfo (Deployment _ dTag dAppOvs dStOvs) dMeta dLogs) = do
  T.putStrLn "Current settings:"
  T.putStrLn $ "tag: " <> coerce dTag
  T.putStrLn $ "application overrides: "
    <> (formatOverrides $ coerce <$> dAppOvs)
  T.putStrLn $ "deployment overrides: "
    <> (formatOverrides $ coerce <$> dStOvs)
  T.putStrLn $ "metadata: "
  forM_ dMeta $ \m ->
    T.putStrLn $
      "  " <> deploymentMetadataKey m <> ": " <> deploymentMetadataValue m
  T.putStrLn ""
  T.putStrLn "Last logs:"
  ppDeploymentLogs dLogs

ppDeploymentLogs :: [DeploymentLog] -> IO ()
ppDeploymentLogs ds =
  putStrLn
    . tableString
      [ column expand right noAlign def
      , column expand center noAlign def
      , column expand center noAlign def
      , column expand center noAlign def
      , column expand left (charAlign '=') def
      , column expand left (charAlign '=') def
      , column expand center noAlign def
      ]
      unicodeBoldHeaderS
      (titlesH
        [ "Created at"
        , "Action id"
        , "Action"
        , "Tag"
        , "App overrides"
        , "Deployment overrides"
        , "Exit code"
        ])
    $ ppDeploymentLogRow <$> ds

-- | Pretty-prints the deployment log.
ppDeploymentLogRow :: DeploymentLog -> RowGroup Text
ppDeploymentLogRow dLog = colsAllG top
  [ [encode_YmdHMS SubsecondPrecisionAuto w3c
    (timeToDatetime . Time . fromIntegral
      $ dLog ^. field @"createdAt" * 10 ^ (9 :: Int))]
  , [dLog ^. field @"actionId" . to unActionId . re _Show . packed]
  , [dLog ^. field @"action" . coerced]
  , [dLog ^. field @"deploymentTag" . coerced]
  , dLog ^. field @"deploymentAppOverrides"
    . to (fmap $ formatOverride . coerce)
  , dLog ^. field @"deploymentDepOverrides"
    . to (fmap $ formatOverride . coerce)
  , [dLog ^. field @"exitCode" . re _Show . packed]
  ]
