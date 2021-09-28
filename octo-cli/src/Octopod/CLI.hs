{-# LANGUAGE PartialTypeSignatures #-}

module Octopod.CLI (runOcto) where

import Common.Types
import Common.Utils (dfiName)
import Control.Exception
import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Coerce
import Data.Generics.Labels ()
import Data.Generics.Product
import qualified Data.Map.Ordered.Strict as OM
import Data.Maybe
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lens
import Data.Time
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

dieT :: MonadIO m => Text -> m a
dieT = liftIO . die . T.unpack

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
      Create tName tSetAp tSetDep -> do
        appOvs <- either dieT pure $ parseSetOverrides tSetAp
        depOvs <- either dieT pure $ parseSetOverrides tSetDep
        handleCreate auth $ Deployment (coerce tName) appOvs depOvs
      List -> handleList auth
      Archive tName -> handleArchive auth . coerce $ tName
      Update tName tSetAp unsetApp tSetD unsetDep -> do
        appOvs <- either dieT pure $ parseSetOverrides tSetAp
        depOvs <- either dieT pure $ parseSetOverrides tSetD
        let tName' = coerce tName
        handleUpdate auth tName' appOvs unsetApp depOvs unsetDep
      Info tName ->
        handleInfo auth . coerce $ tName
      Cleanup tName ->
        handleCleanup auth . coerce $ tName
      Restore tName ->
        handleRestore auth . coerce $ tName
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
  Overrides 'ApplicationLevel ->
  [Text] ->
  Overrides 'DeploymentLevel ->
  [Text] ->
  ReaderT ClientEnv IO ()
handleUpdate auth dName dNewAppOvs removedAppOvs dNewDepOvs removedDepOvs = do
  clientEnv <- ask
  dep <- runClientM' (_fullInfoH auth dName) clientEnv
  let removeAll :: Ord k => [k] -> OM.OMap k v -> Either k (OM.OMap k v)
      removeAll [] m = Right m
      removeAll (k : kk) m =
        if k `OM.member` m
          then removeAll kk $ OM.delete k m
          else Left k
      removeAllM :: MonadIO m => [Text] -> Overrides l -> m (Overrides l)
      removeAllM ks (Overrides m) =
        either
          (\k -> dieT $ "Override " <> k <> " not present in deployment.")
          (pure . Overrides)
          $ removeAll ks m
  appOverrides' <-
    fmap (<> dNewAppOvs) $
      removeAllM removedAppOvs $ dep ^. #deployment . #appOverrides
  deploymentOverrides' <-
    fmap (<> dNewDepOvs) $
      removeAllM removedDepOvs $ dep ^. #deployment . #deploymentOverrides
  liftIO $ do
    let dUpdate =
          DeploymentUpdate
            { appOverrides = appOverrides'
            , deploymentOverrides = deploymentOverrides'
            }
    response <- runClientM (updateH auth dName dUpdate) clientEnv
    handleResponse (const $ pure ()) response

runClientM' :: MonadIO m => ClientM a -> ClientEnv -> m a
runClientM' req env = liftIO $ runClientM req env >>= either (die . displayException) pure

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

-- | Handles the 'logs' subcommand.
handleGetActionInfo :: AuthContext AuthHeaderAuth -> ActionId -> LogOutput -> ReaderT ClientEnv IO ()
handleGetActionInfo auth aId l = do
  clientEnv <- ask
  liftIO $ do
    runClientM (getActionInfoH auth aId) clientEnv >>= \case
      Left err -> print err
      Right x -> case l of
        Out -> T.putStrLn . unStdout $ x ^. #stdout
        Err -> T.putStrLn . unStderr $ x ^. #stderr
        ErrOut -> do
          T.putStrLn "\t\tstdout:\n"
          T.putStrLn . unStdout $ x ^. #stdout
          T.putStrLn "\t\tstderr:\n"
          T.putStrLn . unStderr $ x ^. #stderr

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
  :<|> getActionInfoH = pushArrowIntoServantAlt $ client (Proxy @PowerAPI)

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
    Just (ValidationError nameErrors) ->
      T.concat ((<> "\n") <$> nameErrors)
    Just (AppError errorMsg) -> errorMsg
    Just Success -> "ok"
    _ -> "error: " <> (T.pack . LBSC.unpack $ body)

-- | Pretty-prints the 'info' subcommand result.
printInfo :: DeploymentInfo -> IO ()
printInfo (DeploymentInfo (Deployment _ dAppOvs dStOvs) (DeploymentMetadata dMeta) dLogs) = do
  T.putStrLn "Current settings:"
  T.putStrLn $
    "application overrides: "
      <> formatOverrides dAppOvs
  T.putStrLn $
    "deployment overrides: "
      <> formatOverrides dStOvs
  T.putStrLn $ "metadata: "
  forM_ dMeta $ \m ->
    T.putStrLn $
      "  " <> m ^. #name <> ": " <> m ^. #link
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
      [ T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) $
          dLog ^. field @"createdAt"
      ]
    , [dLog ^. field @"actionId" . to unActionId . re _Show . packed]
    , [dLog ^. field @"action" . to actionToText]
    , dLog ^. field @"deploymentAppOverrides" . to formatOverrides'
    , dLog ^. field @"deploymentDepOverrides" . to formatOverrides'
    , [dLog ^. field @"exitCode" . re _Show . packed]
    ]
