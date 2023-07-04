module Octopod.Server (runOctopodServer) where

import Common.Validation
import Control.Applicative
import Control.CacheMap (CacheMap)
import qualified Control.CacheMap as CM
import Control.Concurrent.Async (race)
import Control.Concurrent.Lifted hiding (yield)
import Control.Concurrent.STM
import Control.Exception.Lifted hiding (Handler)
import Control.Lens hiding (Context, each, pre, (<.))
import Control.Lens.Extras
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Octopod.DeploymentLock
import Crypto.JOSE hiding (Context)
import Data.Aeson (Value (..), encode, toJSON)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Coerce
import Data.Conduit (ConduitT, yield)
import qualified Data.ConfigTree as CT
import qualified Data.Csv as C
import Data.Fixed
import Data.Foldable
import Data.Functor
import Data.Functor.Contravariant
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.IORef.Lifted
import Data.Int
import Data.Maybe
import Data.Pool
import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Traversable
import qualified Data.Vector as V
import GHC.Exception
import GHC.Stack
import Hasql.Connection
import qualified Hasql.Session as HasQL
import Hasql.Statement
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Katip hiding (Namespace)
import Network.Wai.Handler.Warp
import Octopod.API
import Octopod.PowerAPI
import Octopod.PowerAPI.Auth.Server
import Octopod.Schema
import Octopod.Server.Args
import Octopod.Server.ControlScriptUtils
import Octopod.Server.Logging
import Octopod.Server.Posix
import Options.Generic
import Orphans ()
import PostgreSQL.ErrorCodes
import Rel8 hiding (encode)
import Rel8.Expr.Time
import Servant
import Servant.Auth.Server
import System.Environment (lookupEnv)
import System.Exit
import System.Posix.Signals (sigTERM)
import Text.Read (readMaybe)
import Types
import Prelude hiding (lines, log, unlines, unwords)

type AppM = ReaderT AppState (KatipContextT Handler)

class (KatipContext m, MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) => AppMConstraints m
instance (KatipContext m, MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) => AppMConstraints m

-- | Octopod Server state definition.
data AppState = AppState
  { -- | postgres pool
    dbPool :: !(Pool Connection)
  , -- | channel for WS events for the frontend
    eventSink :: !(TChan WSEvent)
  , -- | background workers counter
    bgWorkersCounter :: !(IORef Int)
  , -- | flag of activating graceful shutdown
    gracefulShutdownActivated :: !(IORef Bool)
  , -- | semaphore for graceful shutdown
    shutdownSem :: !(MVar ())
  , -- | project name
    projectName :: !ProjectName
  , -- | base domain
    baseDomain :: !Domain
  , -- | namespace
    namespace :: !Namespace
  , -- | status update timeout
    statusUpdateTimeout :: !Timeout
  , -- | creation command path
    creationCommand :: !Command
  , -- | update command path
    updateCommand :: !Command
  , -- | deletion command path
    archiveCommand :: !Command
  , -- | checking command path
    checkingCommand :: !Command
  , -- | cleanup command path
    cleanupCommand :: !Command
  , -- | archive checking command path
    archiveCheckingCommand :: !Command
  , -- | tag checking command path
    configCheckingCommand :: !Command
  , infoCommand :: !Command
  , notificationCommand :: !(Maybe Command)
  , deploymentOverridesCommand :: !Command
  , deploymentOverrideKeysCommand :: !Command
  , applicationOverridesCommand :: !Command
  , applicationOverrideKeysCommand :: !Command
  , unarchiveCommand :: !Command
  , -- | Deployments currently being processed which has not yet been
    -- recorded in the database.
    lockedDeployments :: !LockedDeployments
  , depOverridesCache :: !(CacheMap ServerError AppMConstraints () (DefaultConfig 'DeploymentLevel))
  , depOverrideKeysCache :: !(CacheMap ServerError AppMConstraints () [Text])
  , appOverridesCache :: !(CacheMap ServerError AppMConstraints (Config 'DeploymentLevel) (DefaultConfig 'ApplicationLevel))
  , appOverrideKeysCache :: !(CacheMap ServerError AppMConstraints (Config 'DeploymentLevel) [Text])
  , gitSha :: !Text
  , controlScriptTimeout :: !ControlScriptTimeout
  }
  deriving stock (Generic)

-- | Deployment exception definition.
data DeploymentException
  = DeploymentFailed Int
  | ThereIsActiveDeployment
  deriving stock (Show)

instance Exception DeploymentException

-- | Deployment list type definition.
data DeploymentListType
  = AllDeployments
  | ArchivedOnlyDeployments
  | ActiveOnlyDeployments
  deriving stock (Show)

-- | Full info list type definition.
data FullInfoListType
  = FullInfoForAll
  | FullInfoOnlyForOne DeploymentName
  deriving stock (Show)

runOctopodServer ::
  -- | The git SHA
  Text ->
  IO ()
runOctopodServer sha = do
  bgWorkersC <- newIORef 0
  gracefulShutdownAct <- newIORef False
  shutdownS <- newEmptyMVar
  opts <- parseArgs
  let a ?! e = a >>= maybe (die e) pure
      getEnvOrDie eName = lookupEnv eName ?! (eName <> " is not set")
      getEnvOrDieWith eName f = fmap (>>= f) (lookupEnv eName) ?! (eName <> " is not set")
  projName <- coerce . pack <$> getEnvOrDie "PROJECT_NAME"
  domain <- coerce . pack <$> getEnvOrDie "BASE_DOMAIN"
  ns <- coerce . pack <$> getEnvOrDie "NAMESPACE"
  archRetention <- fromInteger <$> getEnvOrDieWith "ARCHIVE_RETENTION" readMaybe
  stUpdateTimeout <- Timeout . CalendarDiffTime 0 . fromInteger <$> getEnvOrDieWith "STATUS_UPDATE_TIMEOUT" readMaybe
  scriptTimeout <- ControlScriptTimeout . fromInteger . maybe (60 * 30) read <$> lookupEnv "CONTROL_SCRIPT_TIMEOUT"
  creationCmd <- Command . pack <$> getEnvOrDie "CREATION_COMMAND"
  updateCmd <- Command . pack <$> getEnvOrDie "UPDATE_COMMAND"
  archiveCmd <- Command . pack <$> getEnvOrDie "ARCHIVE_COMMAND"
  checkingCmd <- Command . pack <$> getEnvOrDie "CHECKING_COMMAND"
  cleanupCmd <- Command . pack <$> getEnvOrDie "CLEANUP_COMMAND"
  archiveCheckingCmd <- Command . pack <$> getEnvOrDie "ARCHIVE_CHECKING_COMMAND"
  tagCheckingCmd <- Command . pack <$> getEnvOrDie "CONFIG_CHECKING_COMMAND"
  infoCmd <- Command . pack <$> getEnvOrDie "INFO_COMMAND"
  dOverridesCmd <- Command . pack <$> getEnvOrDie "DEPLOYMENT_CONFIG_COMMAND"
  dKeysCmd <- Command . pack <$> getEnvOrDie "DEPLOYMENT_KEYS_COMMAND"
  aOverridesCmd <- Command . pack <$> getEnvOrDie "APPLICATION_CONFIG_COMMAND"
  aKeysCmd <- Command . pack <$> getEnvOrDie "APPLICATION_KEYS_COMMAND"
  unarchiveCmd <- Command . pack <$> getEnvOrDie "UNARCHIVE_COMMAND"
  powerAuthorizationHeader <- AuthHeader . BSC.pack <$> getEnvOrDie "POWER_AUTHORIZATION_HEADER"
  invalidationTime <- fromInteger . maybe (60 * 60 * 24) read <$> lookupEnv "CACHE_INVALIDATION_TIME"
  updateTime <- fromInteger . maybe (60 * 10) read <$> lookupEnv "CACHE_UPDATE_TIME"
  isDebug <- (== Just "true") <$> lookupEnv "DEBUG"
  isProdLogs <- (/= Just "false") <$> lookupEnv "PROD_LOGS"
  let cacheMap :: forall a x. (forall m. AppMConstraints m => x -> m a) -> IO (CacheMap ServerError AppMConstraints x a)
      cacheMap = CM.initCacheMap invalidationTime updateTime
      decodeCSVDefaultConfig :: BSL.ByteString -> Either String (DefaultConfig l)
      decodeCSVDefaultConfig bs = do
        x <- C.decode C.NoHeader bs
        pure $ DefaultConfig . CT.fromFlatList . V.toList $ x
      either500S :: (KatipContext m, MonadError ServerError m) => Either String x -> m x
      either500S (Right x) = pure x
      either500S (Left err) = do
        logLocM ErrorS $ logStr err
        throwError err500 {errBody = BSLC.pack err}
  notificationCmd <-
    (fmap . fmap) (Command . pack) $
      lookupEnv "NOTIFICATION_COMMAND" <&> \case
        Just "" -> Nothing
        x -> x
  depOverrideKeysCache' <- cacheMap $ \() -> do
    (_, Stdout out, _, _) <- deploymentOverrideKeys >>= runCommandArgs deploymentOverrideKeysCommand
    pure $ T.lines out
  depOverridesCache' <- cacheMap $ \() -> do
    (_, Stdout out, _, _) <- defaultDeploymentOverridesArgs >>= runCommandArgs deploymentOverridesCommand
    either500S $ decodeCSVDefaultConfig . BSL.fromStrict . T.encodeUtf8 $ out
  appOverrideKeysCache' <- cacheMap $ \cfg -> do
    (_, Stdout out, _, _) <- applicationOverrideKeys cfg >>= runCommandArgs applicationOverrideKeysCommand
    pure $ T.lines out
  appOverridesCache' <- cacheMap $ \cfg -> do
    (_, Stdout out, _, _) <- defaultApplicationOverridesArgs cfg >>= runCommandArgs applicationOverridesCommand
    either500S $ decodeCSVDefaultConfig . BSL.fromStrict . T.encodeUtf8 $ out
  let logConfig =
        LogConfig
          { project = projName
          , debug = isDebug
          , prodLogs = isProdLogs
          }
  runLog logConfig $ do
    void $ do
      let termHandler = terminationHandler bgWorkersC gracefulShutdownAct shutdownS
      installShutdownHandler [sigTERM] termHandler
    dbPool' <-
      liftBase $
        createPool
          (fmap (either (error . show) id) . acquire $ unDBConnectionString $ octopodDB opts)
          release
          1
          30
          (unDBPoolSize $ octopodDBPoolSize opts)
    channel <- liftBase . atomically $ newBroadcastTChan
    lockedDs <- initLockedDeployments
    let appSt =
          AppState
            { dbPool = dbPool'
            , eventSink = channel
            , bgWorkersCounter = bgWorkersC
            , gracefulShutdownActivated = gracefulShutdownAct
            , shutdownSem = shutdownS
            , projectName = projName
            , baseDomain = domain
            , namespace = ns
            , statusUpdateTimeout = stUpdateTimeout
            , creationCommand = creationCmd
            , updateCommand = updateCmd
            , archiveCommand = archiveCmd
            , checkingCommand = checkingCmd
            , cleanupCommand = cleanupCmd
            , archiveCheckingCommand = archiveCheckingCmd
            , configCheckingCommand = tagCheckingCmd
            , infoCommand = infoCmd
            , notificationCommand = notificationCmd
            , deploymentOverridesCommand = dOverridesCmd
            , deploymentOverrideKeysCommand = dKeysCmd
            , applicationOverridesCommand = aOverridesCmd
            , applicationOverrideKeysCommand = aKeysCmd
            , unarchiveCommand = unarchiveCmd
            , lockedDeployments = lockedDs
            , depOverridesCache = depOverridesCache'
            , depOverrideKeysCache = depOverrideKeysCache'
            , appOverridesCache = appOverridesCache'
            , appOverrideKeysCache = appOverrideKeysCache'
            , gitSha = sha
            , controlScriptTimeout = scriptTimeout
            }
        serverPort = octopodPort opts
        powerServerPort = unServerPort serverPort
        uiServerPort = unServerPort $ octopodUIPort opts
        wsServerPort = unServerPort $ octopodWSPort opts
    (`runReaderT` appSt) $ do
      runServer uiServerPort
        `raceM_` runPowerServer powerServerPort powerAuthorizationHeader
        `raceM_` runWsServer wsServerPort channel
        `raceM_` runStatusUpdater
        `raceM_` runShutdownHandler
        `raceM_` runArchiveCleanup archRetention

raceM_ :: MonadBaseControl IO m => m a -> m b -> m ()
raceM_ a b = liftBaseWith $ \runInBase ->
  race (runInBase a) (runInBase b) >>= \case
    Left x -> void $ restoreM x
    Right y -> void $ restoreM y

runHasQL ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m) =>
  HasQL.Session a ->
  m a
runHasQL s = do
  p <- asks dbPool
  liftBaseOp (withResource p) $ \conn ->
    liftBase (HasQL.run s conn) >>= \case
      Right x -> pure x
      Left err -> do
        logLocM ErrorS $ logStr $ displayException err
        throwIO err

runArchiveCleanup ::
  forall m.
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m) =>
  NominalDiffTime ->
  m ()
runArchiveCleanup retention = do
  isShuttingDown >>= \case
    True -> pure ()
    False -> do
      cutoff <- liftBase getCurrentTime <&> addUTCTime (negate retention)
      dNames <- runStatement $
        select $ do
          ds <- each deploymentSchema
          where_ $ (ds ^. #status) ==. litExpr Archived
          where_ $ ds ^. #archivedAt <. litExpr (Just cutoff)
          pure $ ds ^. #name
      for_ dNames $ \dName -> (>>= logLeft) . runExceptT $
        runDeploymentBgWorker Nothing dName (pure ()) $ \() -> cleanupDeployment dName
      threadDelayMicro (realToFrac $ retention / 10)
      runArchiveCleanup retention
  where
    threadDelayMicro :: Micro -> m ()
    threadDelayMicro (MkFixed i) = threadDelay (fromInteger i)

runTransaction ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m) =>
  Transaction a ->
  m a
runTransaction q = katipAddNamespace "transaction" $ runHasQL $ transaction Serializable Write q

runStatement ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m) =>
  Statement () a ->
  m a
runStatement q = katipAddNamespace "statement" $ runHasQL $ HasQL.statement () q

runApp ::
  forall api context m.
  ( HasServer api context
  , KatipContext m
  , MonadBase IO m
  , MonadReader AppState m
  , HasContextEntry (context .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  Port ->
  Proxy api ->
  Servant.Context context ->
  ServerT api AppM ->
  m ()
runApp portNum api ctx srv = do
  appstate <- ask
  logEnv <- getLogEnv
  logCtx <- getKatipContext
  ns <- getKatipNamespace
  let hoist :: AppM a -> Handler a
      hoist act = runKatipContextT logEnv logCtx ns $ runReaderT act appstate
      log req respStatus mSize = runKatipContextT logEnv logCtx ns $ do
        logLocM DebugS $ show' req <> " " <> show' respStatus <> " " <> show' mSize
  liftBase $
    runSettings (setPort portNum $ setLogger log defaultSettings) $
      serveWithContext api ctx $ hoistServerWithContext api (Proxy @context) hoist srv

-- | Request handlers of the Web UI API application.
server :: ServerT API AppM
server =
  defaultDeploymentKeysH
    :<|> defaultDeploymentOverridesH
    :<|> defaultApplicationKeysH
    :<|> defaultApplicationOverridesH
    :<|> ( listH :<|> createH :<|> archiveH :<|> updateH
            :<|> infoH
            :<|> fullInfoH
            :<|> statusH
            :<|> restoreH
         )
    :<|> pingH
    :<|> projectNameH

lookupCache :: (Ord x, AppMConstraints m) => (AppState -> CacheMap ServerError AppMConstraints x y) -> x -> m y
lookupCache f x = do
  cm <- asks f
  CM.lookupBlocking cm x

defaultDeploymentKeysH :: AppMConstraints m => m [Text]
defaultDeploymentKeysH = lookupCache depOverrideKeysCache ()

defaultDeploymentOverridesH :: AppMConstraints m => m (DefaultConfig 'DeploymentLevel)
defaultDeploymentOverridesH = lookupCache depOverridesCache ()

defaultApplicationKeysH :: AppMConstraints m => Config 'DeploymentLevel -> m [Text]
defaultApplicationKeysH = lookupCache appOverrideKeysCache

defaultApplicationOverridesH :: AppMConstraints m => Config 'DeploymentLevel -> m (DefaultConfig 'ApplicationLevel)
defaultApplicationOverridesH = lookupCache appOverridesCache

runServer :: (KatipContext m, MonadBase IO m, MonadReader AppState m) => Port -> m ()
runServer portNum = katipAddNamespace "app" $ runApp portNum (Proxy @API) EmptyContext server

-- | Application with the octo CLI API.
runPowerServer :: (KatipContext m, MonadBase IO m, MonadReader AppState m) => Port -> AuthHeader -> m ()
runPowerServer portNum h = katipAddNamespace "powerApp" $ do
  jwk' <- liftBase $ genJWK (RSAGenParam (4096 `div` 8))
  let ctx :: Servant.Context Ctx
      ctx =
        h
          :. (defaultJWTSettings jwk' :: JWTSettings)
          :. (defaultCookieSettings :: CookieSettings)
          :. EmptyContext
  runApp portNum (Proxy @PowerAPI) ctx powerServer

type Ctx = '[AuthHeader, JWTSettings, CookieSettings]

-- | Request handlers of application with the octo CLI API.
powerServer :: ServerT PowerAPI AppM
powerServer (Authenticated ()) =
  ( powerListH :<|> createH :<|> archiveH :<|> updateH
      :<|> powerInfoH
      :<|> powerFullInfoH
      :<|> statusH
      :<|> cleanupH
      :<|> restoreH
      :<|> deleteH
  )
    :<|> getActionInfoH
powerServer _ = throwAll err401

-- | Application with the WS API.
runWsServer :: (KatipContext m, MonadBase IO m, MonadReader AppState m) => Port -> TChan WSEvent -> m ()
runWsServer portNum channel =
  katipAddNamespace "wsApp" $
    runApp portNum (Proxy @WebSocketAPI) EmptyContext $ wsServer channel

-- | Request handlers of the application with the WS API.
wsServer :: TChan WSEvent -> Server WebSocketAPI
wsServer = eventS

-- | Handles WS events.
eventS :: MonadIO m => TChan WSEvent -> ConduitT () Value m ()
eventS channel = do
  dupChannel <- liftIO . atomically $ dupTChan channel
  forever $ do
    event <- liftIO . atomically . readTChan $ dupChannel
    yield . toJSON $ event

-- | Handles the 'list' request of the Web UI API.
listH :: AppM [DeploymentFullInfo]
listH = getFullInfo

-- | Handles the 'list' request of the octo CLI API.
powerListH :: AppM [DeploymentFullInfo]
powerListH = getFullInfo

-- | Handles the 'full_info' request of the Web UI API.
fullInfoH ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) =>
  DeploymentName ->
  m DeploymentFullInfo
fullInfoH dName = do
  getSingleFullInfo dName >>= \case
    Just fullInfo -> pure fullInfo
    Nothing ->
      throwError
        err404
          { errBody = validationError ["Name not found"]
          }

-- | Handles the 'full_info' request of the octo CLI API.
powerFullInfoH :: DeploymentName -> AppM DeploymentFullInfo
powerFullInfoH = fullInfoH

getSingleFullInfo ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m) =>
  DeploymentName ->
  m (Maybe DeploymentFullInfo)
getSingleFullInfo dName = do
  deploymentsSchema <- runStatement . select $ do
    d <- each deploymentSchema
    where_ $ d ^. #name ==. litExpr dName
    pure d
  deployments <- for deploymentsSchema extractDeploymentFullInfo
  logLocM DebugS $ "get deployments: " <> show' deployments
  return $ listToMaybe deployments

getFullInfo ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m) =>
  m [DeploymentFullInfo]
getFullInfo = do
  deploymentsSchema <-
    runStatement . select . orderBy (view #updatedAt >$< desc) $
      each deploymentSchema
  deployments <- for deploymentsSchema extractDeploymentFullInfo
  logLocM DebugS $ "get deployments: " <> show' deployments
  return deployments

-- | Handles the 'create' request.
createH :: Deployment -> AppM CommandResponse
createH dep = do
  failIfGracefulShutdownActivated
  unless (isNameValid $ dep ^. #name) $ do
    let badNameText =
          "Deployment name length should be longer than 2 characters, \
          \under 17 characters and begin with a letter."
    throwError
      err400
        { errBody = validationError [badNameText]
        }
  failIfImageNotFound dep
  failIfGracefulShutdownActivated
  runDeploymentBgWorker
    Nothing
    (dep ^. #name)
    ( do
        res <-
          try $
            runStatement $
              insert
                Insert
                  { into = deploymentSchema
                  , rows =
                      values
                        [ DeploymentSchema
                            { id_ = unsafeDefault
                            , name = litExpr $ dep ^. #name
                            , appOverrides = litExpr $ dep ^. #appOverrides
                            , deploymentOverrides = litExpr $ dep ^. #deploymentOverrides
                            , createdAt = now
                            , updatedAt = now
                            , archivedAt = litExpr Nothing
                            , status = litExpr CreatePending
                            , statusUpdatedAt = now
                            , checkedAt = now
                            , metadata = litExpr (DeploymentMetadata [])
                            }
                        ]
                  , onConflict = Abort
                  , returning = pure ()
                  }
        case res of
          Right () -> pure ()
          Left (HasQL.QueryError _ _ (HasQL.ResultError (HasQL.ServerError code _ _ _)))
            | code == unique_violation ->
              throwError
                err400
                  { errBody = validationError ["Deployment already exists"]
                  }
          Left _ ->
            throwError err409 {errBody = appError "Some database error"}
    )
    $ \() -> do
      sendReloadEvent
      updateDeploymentInfo (dep ^. #name)
      (ec, out, err, elTime) <- createDeployment dep
      -- calling it directly now is fine since there is no previous status.
      createDeploymentLog dep CreateAction ec elTime out err
      sendReloadEvent
      handleExitCode ec
  pure Success

-- | Updates deployment info.
updateDeploymentInfo ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) =>
  DeploymentName ->
  m ()
updateDeploymentInfo dName = do
  DeploymentFullInfo {deployment = dep} <- getDeploymentS dName
  cfg <- getDeploymentConfig dep
  (ec, out, err, _) <- runCommandArgs infoCommand =<< infoCommandArgs cfg dep
  case ec of
    ExitSuccess -> case parseDeploymentMetadata (unStdout out) of
      Right dMeta -> upsertDeploymentMetadatum dName dMeta
      Left err' -> logLocM ErrorS $ "could not get deployment info, could not parse CSV: " <> logStr err'
    ExitFailure _ ->
      logLocM ErrorS $
        "could not get deployment info, exit code: " <> show' ec
          <> ", stdout: "
          <> logStr (unStdout out)
          <> "stderr: "
          <> logStr (unStderr err)

getDeploymentConfig :: AppMConstraints m => Deployment -> m FullConfig
getDeploymentConfig dep = do
  depCfg <- applyOverrides (dep ^. #deploymentOverrides) <$> defaultDeploymentOverridesH
  appCfg <- applyOverrides (dep ^. #appOverrides) <$> defaultApplicationOverridesH depCfg
  pure
    FullConfig
      { appConfig = appCfg
      , depConfig = depCfg
      }

-- | Helper to create a new deployment.
createDeployment ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m, MonadError ServerError m) =>
  Deployment ->
  m (ExitCode, Stdout, Stderr, Duration)
createDeployment dep = do
  cfg <- getDeploymentConfig dep
  res <- runCommandArgs creationCommand =<< createCommandArgs cfg dep
  logLocM InfoS $ "deployment created, deployment: " <> show' dep
  pure res

-- | Helper to get deployment logs.
selectDeploymentLogs ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m) =>
  DeploymentName ->
  m [DeploymentLog]
selectDeploymentLogs dName = (fmap . fmap) extractDeploymentLog . runStatement . select
  . orderBy (view #createdAt >$< desc)
  $ do
    dls <- each deploymentLogSchema
    ds <- each deploymentSchema
    where_ $ ds ^. #name ==. litExpr dName
    where_ $ dls ^. #deploymentId ==. ds ^. #id_
    pure dls

data StatusTransitionProcessOutput = StatusTransitionProcessOutput
  { exitCode :: ExitCode
  , duration :: Duration
  , stdout :: Stdout
  , stderr :: Stderr
  }
  deriving stock (Generic, Show)

data DeploymentStatusTransition
  = TransitionArchived
  | TransitionCreate
  | TransitionUpdate
  | TransitionRestore StatusTransitionProcessOutput
  | TransitionArchivePending StatusTransitionProcessOutput
  | TransitionUpdatePending StatusTransitionProcessOutput
  | TransitionCreatePending StatusTransitionProcessOutput
  | TransitionCleanupFailed StatusTransitionProcessOutput
  | TransitionFailure FailureType
  deriving stock (Show)

transitionStatus :: DeploymentStatusTransition -> DeploymentStatus
transitionStatus TransitionArchived {} = Archived
transitionStatus TransitionCreate {} = Running
transitionStatus TransitionUpdate {} = Running
transitionStatus TransitionRestore {} = CreatePending
transitionStatus TransitionArchivePending {} = ArchivePending
transitionStatus TransitionUpdatePending {} = UpdatePending
transitionStatus TransitionCreatePending {} = CreatePending
transitionStatus TransitionCleanupFailed {} = CleanupFailed
transitionStatus (TransitionFailure t) = Failure t

processOutput :: DeploymentStatusTransition -> Maybe (StatusTransitionProcessOutput, Action)
processOutput TransitionArchived = Nothing
processOutput TransitionCreate = Nothing
processOutput TransitionUpdate = Nothing
processOutput (TransitionRestore x) = Just (x, RestoreAction)
processOutput (TransitionArchivePending x) = Just (x, ArchiveAction)
processOutput (TransitionUpdatePending x) = Just (x, UpdateAction)
processOutput (TransitionCreatePending x) = Just (x, CreateAction)
processOutput (TransitionCleanupFailed x) = Just (x, CleanupAction)
processOutput TransitionFailure {} = Nothing

transitionToStatusS ::
  (KatipContext m, MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m) =>
  DeploymentName ->
  DeploymentStatusTransition ->
  m ()
transitionToStatusS dName tran =
  runExceptT (transitionToStatus dName tran) >>= transitionErrorToServerError

transitionErrorToServerError :: MonadError ServerError m => Either StatusTransitionError a -> m a
transitionErrorToServerError = \case
  Right x -> return x
  Left (InvalidStatusTransition dName a b) ->
    throwError
      err409
        { errBody =
            "Unable to transition deployment "
              <> (BSL.fromStrict . T.encodeUtf8 . unDeploymentName) dName
              <> " from "
              <> show'' a
              <> " to "
              <> show'' b
              <> "."
        }
  Left (DeploymentNotFound dName) ->
    throwError
      err404
        { errBody =
            "Couldn't find deployment "
              <> (BSL.fromStrict . T.encodeUtf8 . unDeploymentName) dName
              <> "."
        }
  where
    show'' = BSLC.pack . show

getDeploymentS ::
  (KatipContext m, MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) =>
  DeploymentName ->
  m DeploymentFullInfo
getDeploymentS dName =
  getSingleFullInfo dName >>= \case
    Just x -> return x
    Nothing -> throwError err404 {errBody = "Deployment not found."}

transitionToStatus ::
  (KatipContext m, MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m) =>
  DeploymentName ->
  DeploymentStatusTransition ->
  ExceptT StatusTransitionError m ()
transitionToStatus dName s = do
  let newS = transitionStatus s
  logLocM InfoS $ "Transitioning deployment " <> show' (unDeploymentName dName) <> " " <> show' s
  res <- runTransaction . runExceptT $ do
    oldS <-
      (lift . statement () . select)
        ( do
            d <- each deploymentSchema
            where_ $ d ^. #name ==. litExpr dName
            pure $ (d ^. #status)
        )
        >>= \case
          [x] -> pure x
          _ -> throwError $ DeploymentNotFound dName
    assertStatusTransitionPossible dName oldS newS
    dep <-
      (fmap . fmap) extractDeployment
        . lift
        . statement ()
        . update
        $ Update
          { target = deploymentSchema
          , from = pure ()
          , set = \() dep ->
              dep
                & #status .~ litExpr newS
                & #updatedAt .~ now
                & #statusUpdatedAt .~ now
                & if newS == Archived then #archivedAt .~ nullify now else id
          , updateWhere = \() dep -> dep ^. #name ==. litExpr dName
          , returning = Projection id
          }
    pure (dep, oldS)
  (deps, oldS) <- either throwError pure res
  dep <- lift $ ensureOne deps
  lift $
    for_ (processOutput s) $ \(output, act) ->
      createDeploymentLog
        dep
        act
        (output ^. #exitCode)
        (output ^. #duration)
        (output ^. #stdout)
        (output ^. #stderr)
  notificationCmd <- asks notificationCommand
  for_ notificationCmd $ \nCmd ->
    runBgWorker . void $
      runCommandArgs' nCmd
        =<< notificationCommandArgs dName oldS newS
  sendReloadEvent

assertStatusTransitionPossible ::
  MonadError StatusTransitionError m =>
  DeploymentName ->
  DeploymentStatus ->
  DeploymentStatus ->
  m ()
assertStatusTransitionPossible dName old new =
  unless
    (possibleTransitions old new)
    (throwError $ InvalidStatusTransition dName old new)

assertDeploymentTransitionPossible ::
  (KatipContext m, MonadError StatusTransitionError m, MonadReader AppState m, MonadBaseControl IO m) =>
  DeploymentName ->
  DeploymentStatus ->
  m ()
assertDeploymentTransitionPossible dName new = do
  dep <-
    getSingleFullInfo dName >>= \case
      Just x -> return x
      Nothing -> throwError $ DeploymentNotFound dName
  assertStatusTransitionPossible dName (recordedStatus $ dep ^. #status) new

assertDeploymentTransitionPossibleS ::
  (KatipContext m, MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m) =>
  DeploymentName ->
  DeploymentStatus ->
  m ()
assertDeploymentTransitionPossibleS dName new =
  runExceptT (assertDeploymentTransitionPossible dName new) >>= transitionErrorToServerError

data StatusTransitionError
  = InvalidStatusTransition DeploymentName DeploymentStatus DeploymentStatus
  | DeploymentNotFound DeploymentName
  deriving stock (Show)

possibleTransitions :: DeploymentStatus -> DeploymentStatus -> Bool
possibleTransitions CreatePending = anyPred [is #_Running, is #_Failure]
possibleTransitions Running = anyPred [is #_Failure, is #_ArchivePending, is #_UpdatePending]
possibleTransitions UpdatePending = anyPred [is #_Failure, is #_Running]
possibleTransitions (Failure _) = anyPred [is #_UpdatePending, is #_Running, is #_ArchivePending, is #_CleanupFailed]
possibleTransitions ArchivePending = anyPred [is #_ArchivePending, is #_Archived]
possibleTransitions Archived = anyPred [is #_CreatePending, is #_CleanupFailed]
possibleTransitions CleanupFailed = const False

anyPred :: [a -> Bool] -> a -> Bool
anyPred preds x = any ($ x) preds

-- | Handles the 'archive' request.
archiveH :: DeploymentName -> AppM CommandResponse
archiveH dName = do
  failIfGracefulShutdownActivated
  runDeploymentBgWorker (Just ArchivePending) dName (pure ()) $ \() -> do
    (view #deployment -> dep) <- getDeploymentS dName
    cfg <- getDeploymentConfig dep
    (ec, out, err, elTime) <- runCommandArgs archiveCommand =<< archiveCommandArgs cfg dep
    transitionToStatusS dName $
      TransitionArchivePending
        StatusTransitionProcessOutput
          { exitCode = ec
          , duration = elTime
          , stdout = out
          , stderr = err
          }
    handleExitCode ec
  pure Success

-- | Handles the 'update' request.
updateH :: DeploymentName -> DeploymentUpdate -> AppM CommandResponse
updateH dName dUpdate = do
  failIfGracefulShutdownActivated
  olDep <- getDeploymentS dName <&> (^. #deployment)
  failIfImageNotFound
    ( olDep
        & field' @"appOverrides" %~ (dUpdate ^. #appOverrides <>)
        & field' @"deploymentOverrides" %~ (dUpdate ^. #deploymentOverrides <>)
    )
  failIfGracefulShutdownActivated
  runDeploymentBgWorker (Just UpdatePending) dName (pure ()) $ \() -> do
    dep <-
      fmap extractDeployment $
        runStatement
          ( update
              Update
                { target = deploymentSchema
                , from = pure ()
                , set = \() ds ->
                    ds & #appOverrides .~ litExpr (dUpdate ^. #appOverrides)
                      & #deploymentOverrides .~ litExpr (dUpdate ^. #deploymentOverrides)
                , updateWhere = \() ds -> ds ^. #name ==. litExpr dName
                , returning = Projection id
                }
          )
          >>= ensureOne

    updateDeploymentInfo dName
    sendReloadEvent
    cfg <- getDeploymentConfig dep
    (ec, out, err, elTime) <- runCommandArgs updateCommand =<< updateCommandArgs cfg dep
    logLocM DebugS $
      logStr $
        "deployment updated, name: "
          <> unDeploymentName dName
    transitionToStatusS dName $
      TransitionUpdatePending
        StatusTransitionProcessOutput
          { exitCode = ec
          , duration = elTime
          , stdout = out
          , stderr = err
          }
    handleExitCode ec
  return Success

-- | Handles the 'info' request of the Web UI API.
infoH :: DeploymentName -> AppM [DeploymentInfo]
infoH dName = do
  dInfo <- getInfo dName
  logLocM DebugS $ "get deployment info: " <> show' dInfo
  pure [dInfo]

-- | Handles the 'info' request of the octo CLI API.
powerInfoH :: DeploymentName -> AppM [DeploymentInfo]
powerInfoH dName = do
  dInfo <- getInfo dName
  logLocM DebugS $ "get deployment info: " <> show' dInfo
  pure [dInfo]

-- | Helper to get deployment info from the database.
getInfo :: DeploymentName -> AppM DeploymentInfo
getInfo dName = do
  dep <- getDeploymentS dName
  dLogs <- selectDeploymentLogs dName
  pure $
    DeploymentInfo
      { deployment = dep ^. #deployment
      , metadata = dep ^. #metadata
      , logs = dLogs
      }

-- | Handles the 'ping' request.
pingH :: AppM Text
pingH = do
  _ <- runStatement $ select $ pure $ litExpr True
  asks gitSha

-- | Handles the 'project_name' request.
projectNameH :: AppM ProjectName
projectNameH = asks projectName

-- | Handles the 'status' request.
statusH :: DeploymentName -> AppM CurrentDeploymentStatus
statusH dName = do
  (view #deployment -> dep) <- getDeploymentS dName
  cfg <- getDeploymentConfig dep
  (ec, _, _, _) <- runCommandArgs checkingCommand =<< checkCommandArgs cfg dep
  pure . CurrentDeploymentStatus $
    case ec of
      ExitSuccess -> Ok
      _ -> Error

-- | Handles the 'cleanup' request.
cleanupH :: DeploymentName -> AppM CommandResponse
cleanupH dName = do
  failIfGracefulShutdownActivated
  runDeploymentBgWorker Nothing dName (pure ()) $ \() -> cleanupDeployment dName
  pure Success

-- | Deletes the deployment from the DB.
deleteH :: DeploymentName -> AppM CommandResponse
deleteH dName = do
  failIfGracefulShutdownActivated
  runDeploymentBgWorker Nothing dName (pure ()) $ \() -> deleteAllDeployment dName
  pure Success

-- | Helper to cleanup deployment.
cleanupDeployment ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m, MonadError ServerError m) =>
  DeploymentName ->
  m ()
cleanupDeployment dName = do
  (view #deployment -> dep) <- getDeploymentS dName
  cfg <- getDeploymentConfig dep
  (ec, out, err, elTime) <- runCommandArgs cleanupCommand =<< cleanupCommandArgs cfg dep
  katipAddContext (FilePayload "stdout" $ T.encodeUtf8 $ unStdout out) $
    katipAddContext (FilePayload "stdout" $ T.encodeUtf8 $ unStderr err) $
      logLocM DebugS "deployment destroyed"
  case ec of
    ExitSuccess -> do
      deleteDeploymentLogs dName
      deleteDeployment dName
      logLocM InfoS $ logStr $ "deployment destroyed, name: " <> unDeploymentName dName
    ExitFailure _ -> do
      transitionToStatusS dName $
        TransitionCleanupFailed
          StatusTransitionProcessOutput
            { exitCode = ec
            , duration = elTime
            , stdout = out
            , stderr = err
            }
      pure ()
  sendReloadEvent
  handleExitCode ec

deleteAllDeployment ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m) =>
  DeploymentName ->
  m ()
deleteAllDeployment dName = do
  deleteDeploymentLogs dName
  deleteDeployment dName
  logLocM InfoS $ logStr $ "deployment destroyed, name: " <> unDeploymentName dName

-- | Helper to delete deployment logs.
deleteDeploymentLogs ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m) =>
  DeploymentName ->
  m ()
deleteDeploymentLogs dName =
  runStatement $
    delete
      Delete
        { from = deploymentLogSchema
        , using = each deploymentSchema
        , deleteWhere = \ds dls -> ds ^. #name ==. litExpr dName &&. dls ^. #deploymentId ==. ds ^. #id_
        , returning = pure ()
        }

-- | Helper to delete a deployment.
deleteDeployment ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m) =>
  DeploymentName ->
  m ()
deleteDeployment dName =
  runStatement $
    delete
      Delete
        { from = deploymentSchema
        , using = pure ()
        , deleteWhere = \() ds -> ds ^. #name ==. litExpr dName
        , returning = pure ()
        }

-- | Handles the 'restore' request.
restoreH :: DeploymentName -> AppM CommandResponse
restoreH dName = do
  failIfGracefulShutdownActivated
  dep <- getDeploymentS dName
  failIfImageNotFound $ dep ^. #deployment
  failIfGracefulShutdownActivated
  runDeploymentBgWorker (Just CreatePending) dName (pure ()) $ \() -> do
    (view #deployment -> dep') <- getDeploymentS dName
    updateDeploymentInfo dName
    cfg <- getDeploymentConfig dep'
    (ec, out, err, elTime) <- runCommandArgs unarchiveCommand =<< unarchiveCommandArgs cfg dep'
    transitionToStatusS dName $
      TransitionRestore
        StatusTransitionProcessOutput
          { exitCode = ec
          , duration = elTime
          , stdout = out
          , stderr = err
          }
    handleExitCode ec
  pure Success

-- | Helper to get action info.
getActionInfoH :: ActionId -> AppM ActionInfo
getActionInfoH aId = do
  rows' <- runStatement $
    select $ do
      dls <- each deploymentLogSchema
      where_ $ dls ^. #actionId ==. litExpr aId
      pure (dls ^. #stdout, dls ^. #stderr)
  case rows' of
    (out, err) : _ -> pure $ ActionInfo out err
    _ ->
      throwError err400 {errBody = appError "Action not found"}

-- | Helper to handle exit code.
handleExitCode :: MonadBase IO m => ExitCode -> m ()
handleExitCode ExitSuccess = return ()
handleExitCode (ExitFailure c) = throwIO $ DeploymentFailed c

-- | Helper to log a deployment action.
createDeploymentLog ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m) =>
  Deployment ->
  Action ->
  ExitCode ->
  Duration ->
  Stdout ->
  Stderr ->
  m ()
createDeploymentLog dep act ec dur out err = do
  dId' <- runStatement $
    select $ do
      ds <- each deploymentSchema
      where_ $ ds ^. #name ==. litExpr (dep ^. #name)
      pure $ ds ^. #id_
  for_ dId' $ \dId ->
    runStatement $
      insert
        Insert
          { into = deploymentLogSchema
          , rows =
              values
                [ DeploymentLogSchema
                    { actionId = unsafeDefault
                    , deploymentId = litExpr dId
                    , action = litExpr act
                    , exitCode = litExpr $ case ec of
                        ExitSuccess -> 0
                        ExitFailure errCode -> fromIntegral errCode
                    , createdAt = now
                    , archived = false
                    , duration = litExpr dur
                    , stdout = litExpr out
                    , stderr = litExpr err
                    , deploymentAppOverrides = litExpr $ dep ^. #appOverrides
                    , deploymentDepOverrides = litExpr $ dep ^. #deploymentOverrides
                    }
                ]
          , onConflict = Abort
          , returning = pure ()
          }

ensureOne :: MonadError ServerError m => [a] -> m a
ensureOne [x] = return x
ensureOne _ = throwError err500

-- | Helper to insert or update deployment metadata.
upsertDeploymentMetadatum ::
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m) =>
  DeploymentName ->
  DeploymentMetadata ->
  m ()
upsertDeploymentMetadatum dName dMetadata =
  runStatement $
    update
      Update
        { target = deploymentSchema
        , from = pure ()
        , set = \() ds -> ds & #metadata .~ litExpr dMetadata
        , updateWhere = \() ds -> ds ^. #name ==. litExpr dName
        , returning = pure ()
        }

-- | Checks the existence of a deployment tag.
-- Returns 404 'Tag not found' response if the deployment tag doesn't exist.
failIfImageNotFound :: Deployment -> AppM ()
failIfImageNotFound dep = do
  cfg <- getDeploymentConfig dep
  (ec, Stdout out, _, _) <- runCommandArgs configCheckingCommand =<< configCheckCommandArgs cfg dep
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      throwError err400 {errBody = BSL.fromStrict $ T.encodeUtf8 out}

-- | Helper to create an application-level error.
appError :: Text -> BSL.ByteString
appError = encode . AppError

-- | Helper to create a validation-level error.
validationError :: [Text] -> BSL.ByteString
validationError nameErrors =
  encode $ ValidationError nameErrors

-- | Helper to send an event to the WS event channel.
sendReloadEvent :: (MonadBase IO m, MonadReader AppState m) => m ()
sendReloadEvent = do
  sink <- asks eventSink
  liftBase $ atomically $ writeTChan sink FrontendPleaseUpdateEverything

-- | Runs the status updater.
runStatusUpdater :: (KatipContext m, MonadBaseControl IO m, MonadReader AppState m) => m ()
runStatusUpdater = do
  let interval = 15 :: NominalDiffTime

  forever $ do
    currentTime <- liftBase getCurrentTime
    let cutoff = addUTCTime (negate interval) currentTime
    (>>= logLeft) . runExceptT $ do
      rows' <- runStatement . select $ do
        ds <- each deploymentSchema
        where_ $ ds ^. #checkedAt <. litExpr cutoff
        where_ $ not_ $ (ds ^. #status) `in_` (litExpr <$> [Archived, CleanupFailed])
        pure (ds ^. #name, ds ^. #status, now `diffTime` (ds ^. #statusUpdatedAt))
      checkResult <- for rows' $ \(dName, dStatus, Timestamp -> ts) -> do
        timeout <- asks statusUpdateTimeout
        mEc <-
          getSingleFullInfo dName >>= \case
            Nothing -> do
              logLocM ErrorS $ logStr $ "Couldn't find deployment: " <> unDeploymentName dName
              pure Nothing
            Just (view #deployment -> dep) -> do
              cfg <- getDeploymentConfig dep
              case dStatus of
                ArchivePending -> do
                  (ec, _, _, _) <- runCommandArgs archiveCheckingCommand =<< archiveCheckArgs cfg dep
                  pure $ Just ec
                _ -> do
                  (ec, _, _, _) <- runCommandArgs checkingCommand =<< checkCommandArgs cfg dep
                  pure $ Just ec
        pure $ mEc <&> \ec -> (dName, statusTransition ec dStatus ts timeout, dStatus)
      updated <-
        for (catMaybes checkResult) $ \(dName, transitionM, dStatus) ->
          let updateCheckedAt s =
                runStatement $
                  update
                    Update
                      { target = deploymentSchema
                      , from = pure ()
                      , set = \() ds -> ds & #checkedAt .~ now
                      , updateWhere = \() ds -> ds ^. #name ==. litExpr dName &&. ds ^. #status ==. litExpr s
                      , returning = pure ()
                      }
           in case transitionM of
                Nothing -> updateCheckedAt dStatus $> False
                Just transition ->
                  ($> True) $
                    (runExceptT . runExceptT) (transitionToStatus dName transition) >>= \case
                      Right (Right ()) -> updateCheckedAt (transitionStatus transition)
                      Left e -> logLocM ErrorS $ show' e
                      Right (Left e) -> logLocM ErrorS $ show' e
      when (Prelude.or updated) sendReloadEvent
      threadDelay 2000000

logLeft :: (HasCallStack, KatipContext m, Exception e) => Either e () -> m ()
logLeft (Left err) = logLocM ErrorS $ logStr $ displayException err
logLeft (Right ()) = pure ()

-- | Returns the new deployment status.
statusTransition ::
  ExitCode ->
  DeploymentStatus ->
  Timestamp ->
  Timeout ->
  Maybe DeploymentStatusTransition
statusTransition ExitSuccess ArchivePending _ _ = Just TransitionArchived
statusTransition ExitSuccess Running _ _ = Nothing
statusTransition ExitSuccess _ _ _ = Just TransitionCreate
statusTransition (ExitFailure n) Running _ _ = Just . TransitionFailure $ failureStatusType n
statusTransition (ExitFailure n) CreatePending (Timestamp (CalendarDiffTime _ ts)) (Timeout (CalendarDiffTime _ timeout))
  | ts > timeout =
    Just . TransitionFailure $ failureStatusType n
statusTransition (ExitFailure n) UpdatePending (Timestamp (CalendarDiffTime _ ts)) (Timeout (CalendarDiffTime _ timeout))
  | ts > coerce timeout =
    Just . TransitionFailure $ failureStatusType n
statusTransition (ExitFailure _) _ _ _ = Nothing

failureStatusType :: Int -> FailureType
failureStatusType 2 = PartialAvailability
failureStatusType 3 = TagMismatch
failureStatusType _ = GenericFailure

-- | Checks if graceful shutdown has been activated activated.
-- Returns 405 'Graceful shutdown activated' response
-- if graceful shutdown has been activated.
failIfGracefulShutdownActivated :: AppM ()
failIfGracefulShutdownActivated = do
  gracefulShutdown <- isShuttingDown
  if gracefulShutdown
    then throwError err405 {errBody = appError "Graceful shutdown activated"}
    else pure ()

isShuttingDown :: (MonadReader AppState m, MonadBase IO m) => m Bool
isShuttingDown = do
  gracefulShutdownAct <- gracefulShutdownActivated <$> ask
  readIORef $ gracefulShutdownAct

-- | Handles the graceful shutdown signal.
-- Sends a signal to the 'shutdownSem' semaphore
-- if the background worker counter is 0.
terminationHandler :: MonadBase IO m => IORef Int -> IORef Bool -> MVar () -> m ()
terminationHandler bgWorkersC gracefulShudownAct shutdownS = do
  atomicWriteIORef gracefulShudownAct True
  c <- readIORef bgWorkersC
  if c == 0
    then putMVar shutdownS ()
    else pure ()

-- | Waits on the 'shutdownSem' semaphore to determine when the
-- server can shut down.
runShutdownHandler :: (MonadBase IO m, MonadReader AppState m) => m ()
runShutdownHandler = do
  sem <- asks shutdownSem
  takeMVar sem

-- | Runs background the worker and increases the background worker counter.
-- Decreases background worker counter
-- after the background worker exits.
-- Sends a signal to 'shutdownSem' semaphore
-- if the background worker counter is 0
-- and graceful shutdown is activated.
runBgWorker :: (MonadBaseControl IO m, MonadReader AppState m) => m () -> m ()
runBgWorker act = void $ forkFinally act' cleanup
  where
    act' = do
      state <- ask
      atomicModifyIORef' (bgWorkersCounter state) $ \c -> (c + 1, ())
      act
    cleanup _ = do
      state <- ask
      c <- atomicModifyIORef' (bgWorkersCounter state) $ \c -> (c - 1, c - 1)
      isShutdown <- readIORef (gracefulShutdownActivated state)
      if isShutdown && c == 0
        then putMVar (shutdownSem state) ()
        else pure ()

runBgWorkerSync ::
  (MonadBaseControl IO m, MonadReader AppState m) =>
  -- | The computation to run, takes a callback for returning a result (and monadic state)
  ((a -> m ()) -> m ()) ->
  m a
runBgWorkerSync act = do
  result <- newEmptyMVar
  runBgWorker $ do
    let respondSync res = do
          state <- liftBaseWith $ \runInBase -> runInBase $ pure res
          tryPutMVar result (Right state) >>= \case
            True -> pure ()
            False -> error "result already submitted by bg worker"
    void $
      try (liftBaseWith $ \runInBase -> runInBase $ act respondSync) >>= \case
        Left e -> tryPutMVar result (Left e)
        Right _ -> tryPutMVar result $ Left $ errorCallException "bg worker did not submit result"
  takeMVar result >>= \case
    Left exc -> throwIO exc
    Right stm -> restoreM stm

-- | Same as 'runBgWorker' but also locks the specified deployment.
runDeploymentBgWorker ::
  forall m a.
  (KatipContext m, MonadBaseControl IO m, MonadReader AppState m, MonadError ServerError m) =>
  -- | Status to set after the first part has finished.
  Maybe DeploymentStatus ->
  DeploymentName ->
  -- | Think of this as running synchronously.
  -- The resulting monad state will be forwarded to the calling context.
  m a ->
  -- | The computation to run asynchronously.
  (a -> m ()) ->
  m a
runDeploymentBgWorker newS dName pre post = do
  liftEither =<< runBgWorkerSync worker
  where
    worker respondSync = do
      withLockedDeployment
        dName
        (respondSync $ Left err409 {errBody = "The deployment is currently being processed."})
        ( do
            let preWithAssert = pre <* for newS (assertDeploymentTransitionPossibleS dName)
            result <- catchError (Right <$> preWithAssert) (pure . Left)
            respondSync result
            liftEither result >>= \res -> do
              sendReloadEvent
              post res
        )

extractDeploymentFullInfo ::
  (MonadReader AppState m, MonadBase IO m) =>
  DeploymentSchema Result ->
  m DeploymentFullInfo
extractDeploymentFullInfo d = do
  let dName = d ^. #name
  locked <- isDeploymentLocked dName
  pure
    DeploymentFullInfo
      { deployment = extractDeployment d
      , status =
          if locked
            then DeploymentPending $ d ^. #status
            else DeploymentNotPending $ d ^. #status
      , metadata = d ^. #metadata
      , createdAt = d ^. #createdAt
      , updatedAt = d ^. #updatedAt
      }
