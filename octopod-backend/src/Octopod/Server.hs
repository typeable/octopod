module Octopod.Server (runOctopodServer) where

import Common.Validation
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import qualified Control.Concurrent.Lifted as L
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO)
import qualified Control.Exception.Lifted as L
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
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Coerce
import Data.Conduit (ConduitT, yield)
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Product
import Data.IORef
import Data.Int
import Data.Maybe
import Data.Pool
import Data.Text (lines, pack, unpack, unwords)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Data.Traversable
import Database.PostgreSQL.Simple hiding (Connection, Query, (:.))
import qualified Database.PostgreSQL.Simple
import Hasql.Connection
import qualified Hasql.Session as HasQL
import Hasql.Statement
import Hasql.Transaction
import Hasql.Transaction.Sessions
import Network.Wai.Handler.Warp
import Octopod.API
import Octopod.PowerAPI
import Octopod.PowerAPI.Auth.Server
import Octopod.Schema
import Octopod.Server.Args
import Octopod.Server.ControlScriptUtils
import Octopod.Server.Logger
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
import System.Log.FastLogger
import System.Posix.Signals (sigTERM)
import Types
import Prelude hiding (lines, log, unlines, unwords)

type PgPool = Pool Database.PostgreSQL.Simple.Connection

type AppM = ReaderT AppState Handler

-- | Octopod Server state definition.
data AppState = AppState
  { -- | postgres pool
    pool :: PgPool
  , dbPool :: Pool Connection
  , -- | logger
    logger :: TimedFastLogger
  , -- | channel for WS events for the frontend
    eventSink :: TChan WSEvent
  , -- | background workers counter
    bgWorkersCounter :: IORef Int
  , -- | flag of activating graceful shutdown
    gracefulShutdownActivated :: IORef Bool
  , -- | semaphore for graceful shutdown
    shutdownSem :: MVar ()
  , -- | project name
    projectName :: ProjectName
  , -- | base domain
    baseDomain :: Domain
  , -- | namespace
    namespace :: Namespace
  , -- | archive retention
    archiveRetention :: ArchiveRetention
  , -- | status update timeout
    statusUpdateTimeout :: Timeout
  , -- | creation command path
    creationCommand :: Command
  , -- | update command path
    updateCommand :: Command
  , -- | deletion command path
    archiveCommand :: Command
  , -- | checking command path
    checkingCommand :: Command
  , -- | cleanup command path
    cleanupCommand :: Command
  , -- | archive checking command path
    archiveCheckingCommand :: Command
  , -- | tag checking command path
    tagCheckingCommand :: Command
  , infoCommand :: Command
  , notificationCommand :: Maybe Command
  , -- | Deployments currently being processed which has not yet been
    -- recorded in the database.
    lockedDeployments :: LockedDeployments
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

runOctopodServer :: IO ()
runOctopodServer = do
  logger' <- newLogger
  logInfo logger' "started"
  bgWorkersC <- newIORef 0
  gracefulShutdownAct <- newIORef False
  shutdownS <- newEmptyMVar
  void $ do
    let termHandler = terminationHandler bgWorkersC gracefulShutdownAct shutdownS
    installShutdownHandler logger' [sigTERM] termHandler
  opts <- parseArgs
  let a ?! e = a >>= maybe (die e) pure
      getEnvOrDie eName = lookupEnv eName ?! (eName <> " is not set")
  projName <- coerce . pack <$> getEnvOrDie "PROJECT_NAME"
  domain <- coerce . pack <$> getEnvOrDie "BASE_DOMAIN"
  ns <- coerce . pack <$> getEnvOrDie "NAMESPACE"
  archRetention <- ArchiveRetention . fromIntegral . read @Int <$> getEnvOrDie "ARCHIVE_RETENTION"
  stUpdateTimeout <- Timeout . CalendarDiffTime 0 . fromIntegral . read @Int <$> getEnvOrDie "STATUS_UPDATE_TIMEOUT"
  creationCmd <- coerce . pack <$> getEnvOrDie "CREATION_COMMAND"
  updateCmd <- coerce . pack <$> getEnvOrDie "UPDATE_COMMAND"
  archiveCmd <- coerce . pack <$> getEnvOrDie "ARCHIVE_COMMAND"
  checkingCmd <- coerce . pack <$> getEnvOrDie "CHECKING_COMMAND"
  cleanupCmd <- coerce . pack <$> getEnvOrDie "CLEANUP_COMMAND"
  archiveCheckingCmd <- coerce . pack <$> getEnvOrDie "ARCHIVE_CHECKING_COMMAND"
  tagCheckingCmd <- coerce . pack <$> getEnvOrDie "TAG_CHECKING_COMMAND"
  infoCmd <- coerce . pack <$> getEnvOrDie "INFO_COMMAND"
  powerAuthorizationHeader <- AuthHeader . BSC.pack <$> getEnvOrDie "POWER_AUTHORIZATION_HEADER"
  notificationCmd <-
    (fmap . fmap) (Command . pack) $
      lookupEnv "NOTIFICATION_COMMAND" <&> \case
        Just "" -> Nothing
        x -> x
  pgPool <-
    initConnectionPool
      (unDBConnectionString $ octopodDB opts)
      (unDBPoolSize $ octopodDBPoolSize opts)
  dbPool' <-
    createPool
      (fmap (either (error . show) id) . acquire $ unDBConnectionString $ octopodDB opts)
      release
      1
      30
      (unDBPoolSize $ octopodDBPoolSize opts)
  channel <- liftIO . atomically $ newBroadcastTChan
  lockedDs <- initLockedDeployments
  let appSt =
        AppState
          pgPool
          dbPool'
          logger'
          channel
          bgWorkersC
          gracefulShutdownAct
          shutdownS
          projName
          domain
          ns
          archRetention
          stUpdateTimeout
          creationCmd
          updateCmd
          archiveCmd
          checkingCmd
          cleanupCmd
          archiveCheckingCmd
          tagCheckingCmd
          infoCmd
          notificationCmd
          lockedDs
      app' = app appSt
      wsApp' = wsApp channel
      serverPort = octopodPort opts
      uiServerPort = unServerPort $ octopodUIPort opts
      powerServerPort = unServerPort serverPort
      wsServerPort = unServerPort $ octopodWSPort opts
  powerApp' <- powerApp powerAuthorizationHeader appSt
  (run uiServerPort app')
    `race_` (run powerServerPort powerApp')
    `race_` (run wsServerPort wsApp')
    `race_` (runStatusUpdater appSt)
    `race_` (runShutdownHandler appSt)

runTransaction ::
  (MonadReader AppState m, MonadBaseControl IO m) =>
  Transaction a ->
  m a
runTransaction q = do
  p <- asks dbPool
  withResource p $ \conn ->
    liftBase $ HasQL.run (transaction Serializable Write q) conn >>= either throwIO pure

runStatement ::
  (MonadReader AppState m, MonadBaseControl IO m) =>
  Statement () a ->
  m a
runStatement q = do
  p <- asks dbPool
  withResource p $ \conn ->
    liftBase $ HasQL.run (HasQL.statement () q) conn >>= either throwIO pure

-- | Initializes the connection pool.
initConnectionPool :: ByteString -> Int -> IO PgPool
initConnectionPool dbConnStr =
  createPool (connectPostgreSQL dbConnStr) close 1 30

-- | Helper to run the server.
nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s

-- | Application with the Web UI API.
app :: AppState -> Application
app s = serve api $ hoistServer api (nt s) server
  where
    api = Proxy @API

-- | Request handlers of the Web UI API application.
server :: ServerT API AppM
server =
  ( listH :<|> createH :<|> archiveH :<|> updateH
      :<|> infoH
      :<|> fullInfoH
      :<|> statusH
      :<|> restoreH
  )
    :<|> pingH
    :<|> projectNameH

-- | Application with the octo CLI API.
powerApp :: AuthHeader -> AppState -> IO Application
powerApp h s = do
  jwk' <- liftIO $ genJWK (RSAGenParam (4096 `div` 8))
  let ctx :: Servant.Context Ctx
      ctx =
        h
          :. (defaultJWTSettings jwk' :: JWTSettings)
          :. (defaultCookieSettings :: CookieSettings)
          :. EmptyContext

  return $
    serveWithContext api ctx $
      hoistServerWithContext api (Proxy @Ctx) (nt s) powerServer
  where
    api = Proxy @PowerAPI

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
  )
    :<|> getActionInfoH
    :<|> cleanArchiveH
powerServer _ = throwAll err401

-- | Application with the WS API.
wsApp :: TChan WSEvent -> Application
wsApp channel = serve api $ wsServer channel
  where
    api = Proxy @WebSocketAPI

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
  (MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) =>
  DeploymentName ->
  m DeploymentFullInfo
fullInfoH dName = do
  getSingleFullInfo dName >>= \case
    Just fullInfo -> pure fullInfo
    Nothing ->
      throwError
        err404
          { errBody = validationError ["Name not found"] []
          }

-- | Handles the 'full_info' request of the octo CLI API.
powerFullInfoH :: DeploymentName -> AppM DeploymentFullInfo
powerFullInfoH = fullInfoH

getSingleFullInfo ::
  (MonadReader AppState m, MonadBaseControl IO m) =>
  DeploymentName ->
  m (Maybe DeploymentFullInfo)
getSingleFullInfo dName = do
  AppState {logger = l} <- ask
  deploymentsSchema <- runStatement . select $ do
    d <- each deploymentSchema
    where_ $ d ^. #name ==. litExpr dName
    pure d
  deployments <- forM deploymentsSchema extractDeploymentFullInfo
  liftBase . logInfo l $ "get deployments: " <> (pack . show $ deployments)
  return $ listToMaybe deployments

getFullInfo ::
  (MonadReader AppState m, MonadBaseControl IO m) =>
  m [DeploymentFullInfo]
getFullInfo = do
  AppState {logger = l} <- ask
  deploymentsSchema <- runStatement . select $ each deploymentSchema
  deployments <- forM deploymentsSchema extractDeploymentFullInfo
  liftBase . logInfo l $ "get deployments: " <> (pack . show $ deployments)
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
        { errBody = validationError [badNameText] []
        }
  t1 <- liftBase getCurrentTime
  failIfImageNotFound dep
  failIfGracefulShutdownActivated
  runDeploymentBgWorker
    Nothing
    (dep ^. #name)
    ( do
        res <-
          L.try $
            runStatement $
              insert
                Insert
                  { into = deploymentSchema
                  , rows =
                      values
                        [ DeploymentSchema
                            { id_ = unsafeDefault
                            , name = litExpr $ dep ^. #name
                            , tag = litExpr $ dep ^. #tag
                            , appOverrides = litExpr $ dep ^. #appOverrides
                            , deploymentOverrides = litExpr $ dep ^. #deploymentOverrides
                            , createdAt = now
                            , updatedAt = now
                            , archivedAt = litExpr Nothing
                            , status = litExpr Running
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
                  { errBody = validationError ["Deployment already exists"] []
                  }
          Left _ ->
            throwError err409 {errBody = appError "Some database error"}
    )
    $ \() -> do
      st <- ask
      liftBase $ sendReloadEvent st
      updateDeploymentInfo (dep ^. #name)
      (ec, out, err) <- createDeployment dep
      t2 <- liftBase getCurrentTime
      let elTime = elapsedTime t2 t1
      -- calling it directly now is fine since there is no previous status.
      createDeploymentLog dep CreateAction ec elTime out err
      liftBase $ sendReloadEvent st
      liftBase $ handleExitCode ec
  pure Success

-- | Updates deployment info.
updateDeploymentInfo ::
  (MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) =>
  DeploymentName ->
  m ()
updateDeploymentInfo dName = do
  log <- asks (logWarning . logger)
  DeploymentFullInfo {deployment = dep} <- getDeploymentS dName
  dc <- getDefaultConfig dName
  (ec, out, err) <- runCommandArgs infoCommand =<< infoCommandArgs dc dep
  case ec of
    ExitSuccess -> do
      dMeta <- liftBase $ parseDeploymentMetadata (lines . unStdout $ out)
      upsertDeploymentMetadatum dName dMeta
    ExitFailure _ ->
      liftBase $
        log $
          "could not get deployment info, exit code: " <> (pack . show $ ec)
            <> ", stdout: "
            <> coerce out
            <> "stderr: "
            <> coerce err

getDefaultConfig :: Monad m => DeploymentName -> m FullDefaultConfig
getDefaultConfig = undefined

-- | Helper to create a new deployment.
createDeployment ::
  (MonadBaseControl IO m, MonadReader AppState m) =>
  Deployment ->
  m (ExitCode, Stdout, Stderr)
createDeployment dep = do
  st <- ask
  dCfg <- getDefaultConfig $ dep ^. #name
  let log :: Text -> IO ()
      log = logInfo (logger st)
      args =
        ControlScriptArgs
          [ "--project-name"
          , T.unpack . coerce $ projectName st
          , "--base-domain"
          , T.unpack . coerce $ baseDomain st
          , "--namespace"
          , T.unpack . coerce $ namespace st
          , "--name"
          , T.unpack . coerce $ dep ^. #name
          , "--tag"
          , T.unpack . coerce $ dep ^. #tag
          ]
          <> fullConfigArgs dCfg dep
  (ec, out, err) <- runCommandArgs creationCommand args
  liftBase . log $ "deployment created, deployment: " <> (pack . show $ dep)
  pure (ec, out, err)

-- | Helper to get deployment logs.
selectDeploymentLogs ::
  (MonadBaseControl IO m, MonadReader AppState m) =>
  DeploymentName ->
  m [DeploymentLog]
selectDeploymentLogs dName = (fmap . fmap) extractDeploymentLog . runStatement . select $ do
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
  | TransitionFailure FailureType
  deriving stock (Show)

transitionStatus :: DeploymentStatusTransition -> DeploymentStatus
transitionStatus TransitionArchived {} = Archived
transitionStatus TransitionCreate {} = Running
transitionStatus TransitionUpdate {} = Running
transitionStatus TransitionRestore {} = Running
transitionStatus TransitionArchivePending {} = ArchivePending
transitionStatus TransitionUpdatePending {} = UpdatePending
transitionStatus TransitionCreatePending {} = CreatePending
transitionStatus (TransitionFailure t) = Failure t

processOutput :: DeploymentStatusTransition -> Maybe (StatusTransitionProcessOutput, Action)
processOutput TransitionArchived = Nothing
processOutput TransitionCreate = Nothing
processOutput TransitionUpdate = Nothing
processOutput (TransitionRestore x) = Just (x, RestoreAction)
processOutput (TransitionArchivePending x) = Just (x, ArchiveAction)
processOutput (TransitionUpdatePending x) = Just (x, UpdateAction)
processOutput (TransitionCreatePending x) = Just (x, CreateAction)
processOutput TransitionFailure {} = Nothing

transitionToStatusS ::
  (MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m) =>
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
  (MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) =>
  DeploymentName ->
  m DeploymentFullInfo
getDeploymentS dName =
  getSingleFullInfo dName >>= \case
    Just x -> return x
    Nothing -> throwError err404 {errBody = "Deployment not found."}

transitionToStatus ::
  (MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m) =>
  DeploymentName ->
  DeploymentStatusTransition ->
  ExceptT StatusTransitionError m ()
transitionToStatus dName s = do
  st <- ask
  let log = liftBase . logInfo (logger st)
      newS = transitionStatus s
  log $ "Transitioning deployment " <> (show' . unDeploymentName) dName <> " " <> show' s
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
                & if newS == Archived then #archivedAt .~ nullify now else id
          , updateWhere = \() dep -> dep ^. #name ==. litExpr dName
          , returning = Projection id
          }
    pure (dep, oldS)
  (deps, oldS) <- either throwError pure res
  dep <- lift $ ensureOne deps
  lift $
    forM_ (processOutput s) $ \(output, act) ->
      createDeploymentLog
        dep
        act
        (output ^. #exitCode)
        (output ^. #duration)
        (output ^. #stdout)
        (output ^. #stderr)
  notificationCmd <- asks notificationCommand
  forM_ notificationCmd $ \nCmd ->
    runBgWorker . void $
      runCommandArgs' nCmd
        =<< notificationCommandArgs dName (dep ^. #tag) oldS newS
  liftBase $ sendReloadEvent st

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
  (MonadError StatusTransitionError m, MonadReader AppState m, MonadBaseControl IO m) =>
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
  (MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m) =>
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
possibleTransitions (Failure _) = anyPred [is #_UpdatePending, is #_Running, is #_ArchivePending]
possibleTransitions ArchivePending = anyPred [is #_ArchivePending, is #_Archived]
possibleTransitions Archived = anyPred [is #_CreatePending]

anyPred :: [a -> Bool] -> a -> Bool
anyPred preds x = any ($ x) preds

-- | Handles the 'archive' request.
archiveH :: DeploymentName -> AppM CommandResponse
archiveH dName = do
  failIfGracefulShutdownActivated
  t1 <- liftBase getCurrentTime
  st <- ask
  let log = liftBase . logInfo (logger st)
      args =
        [ "--project-name"
        , coerce $ projectName st
        , "--base-domain"
        , coerce $ baseDomain st
        , "--namespace"
        , coerce $ namespace st
        , "--name"
        , coerce dName
        ]
      cmd = coerce $ archiveCommand st
  runDeploymentBgWorker (Just ArchivePending) dName (pure ()) $ \() -> do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    t2 <- liftBase getCurrentTime
    let elTime = elapsedTime t2 t1
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
  t1 <- liftIO getCurrentTime
  st <- ask
  let log = logInfo (logger st)
  olDep <- getDeploymentS dName <&> (^. #deployment)
  failIfImageNotFound
    ( olDep
        & field' @"appOverrides" <>~ dUpdate ^. #appOverrides
        & field' @"deploymentOverrides" <>~ dUpdate ^. #deploymentOverrides
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
                      & #tag .~ litExpr (dUpdate ^. #newTag)
                , updateWhere = \() ds -> ds ^. #name ==. litExpr dName
                , returning = Projection id
                }
          )
          >>= ensureOne

    updateDeploymentInfo dName
    liftBase $ sendReloadEvent st
    dCfg <- getDefaultConfig dName
    let args =
          ControlScriptArgs
            [ "--project-name"
            , T.unpack . coerce $ projectName st
            , "--base-domain"
            , T.unpack . coerce $ baseDomain st
            , "--namespace"
            , T.unpack . coerce $ namespace st
            , "--name"
            , T.unpack . coerce $ dName
            , "--tag"
            , T.unpack . coerce $ dep ^. #tag
            ]
            <> fullConfigArgs dCfg dep
    (ec, out, err) <- runCommandArgs updateCommand args
    liftBase . log $
      "deployment updated, name: "
        <> coerce dName
        <> ", tag: "
        <> coerce (dep ^. #tag)
    t2 <- liftBase getCurrentTime
    let elTime = elapsedTime t2 t1
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
  st <- ask
  dInfo <- getInfo dName
  liftIO . logInfo (logger st) $
    "get deployment info: " <> (pack . show $ dInfo)
  pure [dInfo]

-- | Handles the 'info' request of the octo CLI API.
powerInfoH :: DeploymentName -> AppM [DeploymentInfo]
powerInfoH dName = do
  st <- ask
  dInfo <- getInfo dName
  liftIO . logInfo (logger st) $
    "get deployment info: " <> (pack . show $ dInfo)
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
pingH :: AppM NoContent
pingH = do
  _ <- runStatement $ select $ pure $ litExpr True
  pure NoContent

-- | Handles the 'project_name' request.
projectNameH :: AppM ProjectName
projectNameH = asks projectName

-- | Handles the 'status' request.
statusH :: DeploymentName -> AppM CurrentDeploymentStatus
statusH dName = do
  dep <- getDeploymentS dName
  dCfg <- getDefaultConfig dName
  (ec, _, _) <- runCommandArgs checkingCommand =<< checkCommandArgs dCfg (dep ^. #deployment)
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

-- | Helper to cleanup deployment.
cleanupDeployment ::
  (MonadBaseControl IO m, MonadReader AppState m) =>
  DeploymentName ->
  m ()
cleanupDeployment dName = do
  st <- ask
  let log = logInfo (logger st)
      args =
        [ "--project-name"
        , coerce $ projectName st
        , "--base-domain"
        , coerce $ baseDomain st
        , "--namespace"
        , coerce $ namespace st
        , "--name"
        , coerce dName
        ]
      cmd = coerce $ cleanupCommand st
  liftBase . log $ "call " <> unwords (cmd : args)
  (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
  liftBase $ print out >> print err
  deleteDeploymentLogs dName
  deleteDeployment dName
  liftBase $ log $ "deployment destroyed, name: " <> coerce dName
  liftBase $ sendReloadEvent st
  handleExitCode ec

-- | Helper to delete deployment logs.
deleteDeploymentLogs ::
  (MonadBaseControl IO m, MonadReader AppState m) =>
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
  (MonadBaseControl IO m, MonadReader AppState m) =>
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

-- | Handles the 'clean-archive' request.
cleanArchiveH :: AppM CommandResponse
cleanArchiveH = do
  failIfGracefulShutdownActivated
  st <- ask
  let archRetention = unArchiveRetention . archiveRetention $ st
  cutoff <- liftBase getCurrentTime <&> addUTCTime (negate archRetention)
  dNames <- runStatement $
    select $ do
      ds <- each deploymentSchema
      where_ $ (ds ^. #status) `in_` (litExpr <$> archivedStatuses)
      where_ $ ds ^. #archivedAt <. litExpr (Just cutoff)
      pure $ ds ^. #name
  runBgWorker . void $
    for dNames $ \dName ->
      runDeploymentBgWorker Nothing dName (pure ()) $ \() -> cleanupDeployment dName

  pure Success

-- | Handles the 'restore' request.
restoreH :: DeploymentName -> AppM CommandResponse
restoreH dName = do
  failIfGracefulShutdownActivated
  t1 <- liftBase getCurrentTime
  dep <- getDeploymentS dName
  failIfImageNotFound $ dep ^. #deployment
  failIfGracefulShutdownActivated
  runDeploymentBgWorker (Just CreatePending) dName (pure ()) $ \() -> do
    dep' <- getDeploymentS dName
    updateDeploymentInfo dName
    (ec, out, err) <- createDeployment $ dep' ^. #deployment
    t2 <- liftBase getCurrentTime
    let elTime = elapsedTime t2 t1
    transitionToStatusS dName $
      TransitionCreatePending
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
handleExitCode :: MonadBaseControl IO m => ExitCode -> m ()
handleExitCode ExitSuccess = return ()
handleExitCode (ExitFailure c) = liftBase . throwIO $ DeploymentFailed c

-- | Helper to log a deployment action.
createDeploymentLog ::
  (MonadBaseControl IO m, MonadReader AppState m) =>
  Deployment ->
  Action ->
  ExitCode ->
  Duration ->
  Stdout ->
  Stderr ->
  m ()
createDeploymentLog dep act ec dur out err =
  runStatement $
    insert
      Insert
        { into = deploymentLogSchema
        , rows = do
            dId <- do
              ds <- each deploymentSchema
              where_ $ ds ^. #name ==. litExpr (dep ^. #name)
              pure $ ds ^. #id_
            values
              [ DeploymentLogSchema
                  { actionId = unsafeDefault
                  , deploymentId = dId
                  , action = litExpr act
                  , deploymentTag = litExpr $ dep ^. #tag
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
  (MonadBaseControl IO m, MonadReader AppState m) =>
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
  dCfg <- getDefaultConfig $ dep ^. #name
  (ec, _, _) <- runCommandArgs tagCheckingCommand =<< tagCheckCommandArgs dCfg dep
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      throwError err400 {errBody = validationError [] ["Tag not found"]}

-- | Helper to create an application-level error.
appError :: Text -> BSL.ByteString
appError = encode . AppError

-- | Helper to create a validation-level error.
validationError :: [Text] -> [Text] -> BSL.ByteString
validationError nameErrors tagErrors =
  encode $ ValidationError nameErrors tagErrors

-- | Helper to send an event to the WS event channel.
sendReloadEvent :: AppState -> IO ()
sendReloadEvent state =
  atomically $ writeTChan (eventSink state) FrontendPleaseUpdateEverything

-- | Returns time delta between 2 timestamps.
elapsedTime :: UTCTime -> UTCTime -> Duration
elapsedTime t1 t2 = Duration . calendarTimeTime $ t2 `diffUTCTime` t1

-- | Runs the status updater.
runStatusUpdater :: AppState -> IO ()
runStatusUpdater state = do
  currentTime <- liftBase getCurrentTime
  let interval = 30 :: NominalDiffTime
      cutoff = addUTCTime (negate interval) currentTime
  let logErr :: MonadBase IO m => Text -> m ()
      logErr = liftBase . logWarning (logger state)

  forever $
    flip runReaderT state $ do
      rows' <- runStatement . select $ do
        ds <- each deploymentSchema
        where_ $ ds ^. #checkedAt <. litExpr cutoff
        where_ $ ds ^. #status /=. litExpr Archived
        pure (ds ^. #name, ds ^. #status, now `diffTime` (ds ^. #statusUpdatedAt), ds ^. #tag)
      checkResult <- for rows' $ \(dName, dStatus, Timestamp -> ts, _) -> do
        let timeout = statusUpdateTimeout state
        mEc <- case dStatus of
          ArchivePending -> do
            (ec, _, _) <- runCommandArgs archiveCheckingCommand =<< archiveCheckArgs dName
            pure $ Just ec
          _ -> do
            getSingleFullInfo dName >>= \case
              Nothing -> liftBase (logErr $ "Couldn't find deployment: " <> coerce dName) $> Nothing
              Just dep -> do
                dCfg <- getDefaultConfig dName
                (ec, _, _) <- runCommandArgs checkingCommand =<< checkCommandArgs dCfg (dep ^. #deployment)
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
                      Left e -> logErr $ show' e
                      Right (Left e) -> logErr $ show' e
      when (Prelude.or updated) $ liftBase $ sendReloadEvent state
      liftBase $ threadDelay 5000000

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
  gracefulShutdownAct <- gracefulShutdownActivated <$> ask
  gracefulShutdown <- liftIO . readIORef $ gracefulShutdownAct
  if gracefulShutdown
    then throwError err405 {errBody = appError "Graceful shutdown activated"}
    else pure ()

-- | Handles the graceful shutdown signal.
-- Sends a signal to the 'shutdownSem' semaphore
-- if the background worker counter is 0.
terminationHandler :: IORef Int -> IORef Bool -> MVar () -> IO ()
terminationHandler bgWorkersC gracefulShudownAct shutdownS = do
  atomicWriteIORef gracefulShudownAct True
  c <- readIORef bgWorkersC
  if c == 0
    then putMVar shutdownS ()
    else pure ()

-- | Waits on the 'shutdownSem' semaphore to determine when the
-- server can shut down.
runShutdownHandler :: AppState -> IO ()
runShutdownHandler = takeMVar . shutdownSem

-- | Runs background the worker and increases the background worker counter.
-- Decreases background worker counter
-- after the background worker exits.
-- Sends a signal to 'shutdownSem' semaphore
-- if the background worker counter is 0
-- and graceful shutdown is activated.
runBgWorker :: (MonadBaseControl IO m, MonadReader AppState m) => m () -> m ()
runBgWorker act = void $ L.forkFinally act' cleanup
  where
    act' = do
      state <- ask
      liftBase (atomicModifyIORef' (bgWorkersCounter state) (\c -> (c + 1, c))) >> act
    cleanup _ = do
      state <- ask
      liftBase $ do
        c <- atomicModifyIORef' (bgWorkersCounter state) (\c -> (c - 1, c - 1))
        gracefulShutdown <- readIORef (gracefulShutdownActivated state)
        if gracefulShutdown && c == 0
          then putMVar (shutdownSem state) ()
          else pure ()

-- | Same as 'runBgWorker' but also locks the specified deployment.
runDeploymentBgWorker ::
  forall m a.
  (MonadBaseControl IO m, MonadReader AppState m, MonadError ServerError m) =>
  Maybe DeploymentStatus ->
  DeploymentName ->
  -- | Think of this as running synchronously.
  -- The resulting monad state will be forwarded to the calling context.
  m a ->
  -- | The computation to run asynchronously.
  (a -> m ()) ->
  m ()
runDeploymentBgWorker newS dName pre post = do
  st <- ask
  mainThread <- L.myThreadId
  (err :: MVar (Either ServerError (StM m a))) <- L.newEmptyMVar
  runBgWorker $
    withLockedDeployment
      dName
      (L.putMVar err $ Left err409 {errBody = "The deployment is currently being processed."})
      ( do
          L.try (liftBaseWith \runInBase -> runInBase pre) >>= \case
            Left (e :: L.SomeException) -> L.throwTo mainThread e
            Right x -> do
              proceed <- flip L.finally (L.tryPutMVar err (Left err500)) $ case newS of
                Just newS' -> do
                  runExceptT (assertDeploymentTransitionPossibleS dName newS') >>= \case
                    Left e -> L.putMVar err (Left e) $> False
                    Right () -> L.putMVar err (Right x) $> True
                Nothing -> L.putMVar err (Right x) $> True
              liftBase (sendReloadEvent st)
              when proceed $ restoreM x >>= post
      )
  L.readMVar err >>= \case
    Left e -> throwError e
    Right (stm :: StM m a) -> do
      -- This might be a bad idea in general, but with just ReaderT and ExceptT
      -- it should be fine.
      _ :: a <- restoreM stm
      return ()

extractDeploymentFullInfo ::
  (MonadReader AppState m, MonadBase IO m) =>
  DeploymentSchema Result ->
  m DeploymentFullInfo
extractDeploymentFullInfo d = do
  let dName = d ^. #name
  locked <- isDeploymentLocked dName
  dCfg <- getDefaultConfig dName
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
      , deploymentDefaultConfig = dCfg
      }
