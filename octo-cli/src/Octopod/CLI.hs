{-# LANGUAGE PartialTypeSignatures #-}

module Octopod.CLI (runOcto) where

import Chronos
import Common.Types
import Common.Utils (dfiName)
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Coerce
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lens
import GHC.IO.Encoding
import Network.HTTP.Client.TLS
import Octopod.CLI.Args
import Octopod.PowerAPI
import Octopod.PowerAPI.Auth.Client
import Servant.API
import Servant.Client
  ( BaseUrl (..),
    ClientEnv,
    ClientError,
    ClientM,
    client,
    mkClientEnv,
    runClientM,
  )
import Servant.Client.Core
  ( ClientError (FailureResponse),
    ResponseF (..),
    parseBaseUrl,
  )
import System.Environment (lookupEnv)
import System.Exit
import Text.Layout.Table
import Text.Layout.Table.Extras ()
import Prelude as P

-- | Runs the octo CLI.
runOcto :: IO ()
runOcto = do
  setLocaleEncoding utf8
  (AuthHeaderAuthCtx -> auth) <-
    lookupEnv "OCTO_AUTHORIZATION_HEADER"
      >>= maybe (die "OCTO_AUTHORIZATION_HEADER is not set") pure
  args <- parseArgs
  env <- getBaseUrl
  manager <- newTlsManager
  let clientEnv = mkClientEnv manager env
  flip runReaderT clientEnv $
    case args of
      Create tName tTag tSetAp tSetDep tSetPAp tSetPDep -> do
        setApp <- liftIO $ parseSetApplicationOverrides Public tSetAp
        setDep <- liftIO $ parseSetDeploymentOverrides Public tSetDep
        setPApp <- liftIO $ parseSetApplicationOverrides Private tSetPAp
        setPDep <- liftIO $ parseSetDeploymentOverrides Private tSetPDep
        let appOvs = setApp ++ setPApp
            depOvs = setDep ++ setPDep
        handleCreate auth $ Deployment (coerce tName) (coerce tTag) appOvs depOvs
      List -> handleList auth
      Archive tName -> handleArchive auth . coerce $ tName
      Update tName tTag tSetAp tUnsAp tSetD tUnsD tSetPAp tSetPD -> do
        setApp <- liftIO $ parseSetApplicationOverrides Public tSetAp
        setDep <- liftIO $ parseSetDeploymentOverrides Public tSetD
        unsetApp <- liftIO $ parseUnsetApplicationOverrides Public tUnsAp
        unsetDep <- liftIO $ parseUnsetDeploymentOverrides Public tUnsD
        setPApp <- liftIO $ parseSetApplicationOverrides Private tSetPAp
        setPDep <- liftIO $ parseSetDeploymentOverrides Private tSetPD
        let appOvs = setApp ++ setPApp
            depOvs = setDep ++ setPDep
            tName' = coerce tName
            tTag' = coerce tTag
        handleUpdate auth tName' tTag' appOvs unsetApp depOvs unsetDep
      Info tName ->
        handleInfo auth . coerce $ tName
      Cleanup tName ->
        handleCleanup auth . coerce $ tName
      Restore tName ->
        handleRestore auth . coerce $ tName
      CleanArchive ->
        handleCleanArchive auth
      GetActionLogs aId l -> handleGetActionInfo auth aId l

-- | Returns BaseUrl from 'OCTOPOD_URL' environment variable
-- or exit with exit_code=1.
getBaseUrl :: IO BaseUrl
getBaseUrl = do
  octopodURL <-
    lookupEnv "OCTOPOD_URL"
      >>= maybe (die "OCTOPOD_URL is not set") pure
  case parseBaseUrl octopodURL of
    Just url -> pure url
    Nothing -> die "could not parse OCTOPOD_URL"

-- | Handles the 'create' subcommand.
handleCreate :: AuthContext AuthHeaderAuth -> Deployment -> ReaderT ClientEnv IO ()
handleCreate auth createCmd = do
  clientEnv <- ask
  liftIO $ do
    response <- runClientM (createH auth createCmd) clientEnv
    handleResponse (const $ pure ()) response

-- | Handles the 'list' subcommand.
handleList :: AuthContext AuthHeaderAuth -> ReaderT ClientEnv IO ()
handleList auth = do
  clientEnv <- ask
  liftIO $ do
    resp <- runClientM (listH auth) clientEnv
    let getName dep = dep ^. dfiName
        names = T.unlines . coerce . (getName <$>) <$> resp
    handleResponse T.putStr names

-- | Handles the 'archive' subcommand.
handleArchive :: AuthContext AuthHeaderAuth -> DeploymentName -> ReaderT ClientEnv IO ()
handleArchive auth dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (archiveH auth dName) clientEnv

-- | Handles the 'update' subcommand.
handleUpdate ::
  AuthContext AuthHeaderAuth ->
  DeploymentName ->
  DeploymentTag ->
  ApplicationOverrides ->
  ApplicationOverrides ->
  DeploymentOverrides ->
  DeploymentOverrides ->
  ReaderT ClientEnv IO ()
handleUpdate auth dName dTag dNewAppOvs dOldAppOvs dNewDepOvs dOldDepOvs = do
  clientEnv <- ask
  liftIO $ do
    let dUpdate =
          DeploymentUpdate
            { newTag = dTag
            , newAppOverrides = dNewAppOvs
            , oldAppOverrides = dOldAppOvs
            , newDeploymentOverrides = dNewDepOvs
            , oldDeploymentOverrides = dOldDepOvs
            }
    response <- runClientM (updateH auth dName dUpdate) clientEnv
    handleResponse (const $ pure ()) response

-- | Handles the 'info' subcommand.
handleInfo :: AuthContext AuthHeaderAuth -> DeploymentName -> ReaderT ClientEnv IO ()
handleInfo auth dName = do
  clientEnv <- ask
  liftIO $ do
    res <- runClientM (infoH auth dName) clientEnv
    case res of
      Right (i : _) -> printInfo i
      Right [] -> putStrLn notFoundMsg
      Left err -> putStrLn $ "request failed, reason: " ++ show err
  where
    notFoundMsg = "deployment " ++ T.unpack (coerce dName) ++ " not found"

-- | Handles the 'cleanup' subcommand.
handleCleanup :: AuthContext AuthHeaderAuth -> DeploymentName -> ReaderT ClientEnv IO ()
handleCleanup auth dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (cleanupH auth dName) clientEnv

-- | Handles the 'restore' subcommand.
handleRestore :: AuthContext AuthHeaderAuth -> DeploymentName -> ReaderT ClientEnv IO ()
handleRestore auth dName = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (restoreH auth dName) clientEnv

-- | Handles the 'clean-archive' subcommand.
handleCleanArchive :: AuthContext AuthHeaderAuth -> ReaderT ClientEnv IO ()
handleCleanArchive auth = do
  clientEnv <- ask
  liftIO $
    handleResponse (const $ pure ()) =<< runClientM (cleanArchiveH auth) clientEnv

-- | Handles the 'logs' subcommand.
handleGetActionInfo :: AuthContext AuthHeaderAuth -> ActionId -> LogOutput -> ReaderT ClientEnv IO ()
handleGetActionInfo auth aId l = do
  clientEnv <- ask
  liftIO $ do
    runClientM (getActionInfoH auth aId) clientEnv >>= \case
      Left err -> print err
      Right x -> case l of
        Out -> T.putStrLn $ x ^. #stdout
        Err -> T.putStrLn $ x ^. #stderr
        ErrOut -> do
          T.putStrLn "\t\tstdout:\n"
          T.putStrLn $ x ^. #stdout
          T.putStrLn "\t\tstderr:\n"
          T.putStrLn $ x ^. #stderr

listH :: AuthContext AuthHeaderAuth -> ClientM [DeploymentFullInfo]
createH :: AuthContext AuthHeaderAuth -> Deployment -> ClientM CommandResponse
archiveH :: AuthContext AuthHeaderAuth -> DeploymentName -> ClientM CommandResponse
updateH :: AuthContext AuthHeaderAuth -> DeploymentName -> DeploymentUpdate -> ClientM CommandResponse
infoH :: AuthContext AuthHeaderAuth -> DeploymentName -> ClientM [DeploymentInfo]
_fullInfoH :: AuthContext AuthHeaderAuth -> DeploymentName -> ClientM DeploymentFullInfo
_statusH :: AuthContext AuthHeaderAuth -> DeploymentName -> ClientM CurrentDeploymentStatus
cleanupH :: AuthContext AuthHeaderAuth -> DeploymentName -> ClientM CommandResponse
restoreH :: AuthContext AuthHeaderAuth -> DeploymentName -> ClientM CommandResponse
getActionInfoH :: AuthContext AuthHeaderAuth -> ActionId -> ClientM ActionInfo
cleanArchiveH :: AuthContext AuthHeaderAuth -> ClientM CommandResponse
( listH
    :<|> createH
    :<|> archiveH
    :<|> updateH
    :<|> infoH
    :<|> _fullInfoH
    :<|> _statusH
    :<|> cleanupH
    :<|> restoreH
  )
  :<|> getActionInfoH
  :<|> cleanArchiveH = pushArrowIntoServantAlt $ client (Proxy @PowerAPI)

type PushArrowIntoServantAlt a b = PushArrowIntoServantAlt' a b (CanPushArrow a)

class CanPushArrow x ~ f => PushArrowIntoServantAlt' x y (f :: Bool) | x f -> y where
  pushArrowIntoServantAlt :: x -> y

type family CanPushArrow x :: Bool where
  CanPushArrow (_ -> (_ :<|> _)) = 'True
  CanPushArrow _ = 'False

instance (PushArrowIntoServantAlt (x -> a) a', PushArrowIntoServantAlt (x -> b) b') => PushArrowIntoServantAlt' (x -> (a :<|> b)) (a' :<|> b') 'True where
  pushArrowIntoServantAlt f =
    pushArrowIntoServantAlt (\x -> let (a :<|> _) = f x in a)
      :<|> pushArrowIntoServantAlt (\x -> let (_ :<|> b) = f x in b)

instance CanPushArrow (x -> y) ~ 'False => PushArrowIntoServantAlt' (x -> y) (x -> y) 'False where
  pushArrowIntoServantAlt x = x

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
  T.putStrLn $
    "application overrides: "
      <> (formatOverrides $ coerce <$> dAppOvs)
  T.putStrLn $
    "deployment overrides: "
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
      ( titlesH
          [ "Created at"
          , "Action id"
          , "Action"
          , "Tag"
          , "App overrides"
          , "Deployment overrides"
          , "Exit code"
          ]
      )
    $ ppDeploymentLogRow <$> ds

-- | Pretty-prints the deployment log.
ppDeploymentLogRow :: DeploymentLog -> RowGroup Text
ppDeploymentLogRow dLog =
  colsAllG
    top
    [
      [ encode_YmdHMS
          SubsecondPrecisionAuto
          w3c
          ( timeToDatetime . Time . fromIntegral $
              dLog ^. field @"createdAt" * 10 ^ (9 :: Int)
          )
      ]
    , [dLog ^. field @"actionId" . to unActionId . re _Show . packed]
    , [dLog ^. field @"action" . coerced]
    , [dLog ^. field @"deploymentTag" . coerced]
    , dLog
        ^. field @"deploymentAppOverrides"
          . to (fmap $ formatOverride . coerce)
    , dLog
        ^. field @"deploymentDepOverrides"
          . to (fmap $ formatOverride . coerce)
    , [dLog ^. field @"exitCode" . re _Show . packed]
    ]
