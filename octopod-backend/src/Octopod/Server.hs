module Octopod.Server (runOctopodServer) where

import Chronos (Time, getTime, now)
import Common.Validation (isNameValid)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import qualified Control.Concurrent.Lifted as L
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, displayException, throwIO, try)
import qualified Control.Exception.Lifted as L
import Control.Lens hiding (Context, pre)
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
import Data.Foldable (foldrM)
import Data.Functor
import Data.Generics.Labels ()
import Data.IORef
import Data.Int (Int64)
import Data.Maybe
import Data.Pool
import Data.Text (lines, pack, unpack, unwords)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable
import Database.PostgreSQL.Simple hiding ((:.))
import Database.PostgreSQL.Simple.Instances ()
import Database.PostgreSQL.Simple.Transaction
import Network.Wai.Handler.Warp
import Octopod.API
import Octopod.PowerAPI
import Octopod.PowerAPI.Auth.Server
import Octopod.Server.Args
import Octopod.Server.ControlScriptUtils
import Octopod.Server.Logger
import Octopod.Server.Posix
import Options.Generic
import Orphans ()
import PostgreSQL.ErrorCodes (unique_violation)
import Servant
import Servant.Auth.Server
import System.Environment (lookupEnv)
import System.Exit
import System.Log.FastLogger
import System.Posix.Signals (sigTERM)
import Types
import Prelude hiding (lines, log, unlines, unwords)

type PgPool = Pool Connection

type AppM = ReaderT AppState Handler

-- | Octopod Server state definition.
data AppState = AppState
  { -- | postgres pool
    pool :: PgPool
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
  archRetention <- coerce . read @Int <$> getEnvOrDie "ARCHIVE_RETENTION"
  stUpdateTimeout <- coerce . read @Int <$> getEnvOrDie "STATUS_UPDATE_TIMEOUT"
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
  channel <- liftIO . atomically $ newBroadcastTChan
  lockedDs <- initLockedDeployments
  let appSt =
        AppState
          pgPool
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
  let ctx :: Context Ctx
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
listH = hidePrivateOverridesInFullInfos <$> getFullInfo FullInfoForAll

-- | Handles the 'list' request of the octo CLI API.
powerListH :: AppM [DeploymentFullInfo]
powerListH = getFullInfo FullInfoForAll

-- | Handles the 'full_info' request of the Web UI API.
fullInfoH ::
  (MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m) =>
  DeploymentName ->
  m DeploymentFullInfo
fullInfoH dName = do
  fullInfoList <-
    hidePrivateOverridesInFullInfos
      <$> getFullInfo (FullInfoOnlyForOne dName)
  case fullInfoList of
    fullInfo : _ -> pure fullInfo
    [] ->
      throwError
        err404
          { errBody = validationError ["Name not found"] []
          }

-- | Handles the 'full_info' request of the octo CLI API.
powerFullInfoH :: DeploymentName -> AppM DeploymentFullInfo
powerFullInfoH dName = do
  fullInfoList <- getFullInfo $ FullInfoOnlyForOne dName
  case fullInfoList of
    fullInfo : _ -> pure fullInfo
    [] ->
      throwError
        err404
          { errBody = validationError ["Name not found"] []
          }

-- | Hides private overrides in 'full_info' response.
hidePrivateOverridesInFullInfos :: [DeploymentFullInfo] -> [DeploymentFullInfo]
hidePrivateOverridesInFullInfos dFullInfos = do
  dFullInfos <&> \(DeploymentFullInfo dep s ct ut u) ->
    let hidePrivate (Deployment n t appOvs depOvs) =
          Deployment n t (hideP appOvs) (hideP depOvs)
        hideP o =
          coerce o <&> \(Override k v vis) ->
            let v' = case vis of
                  Private -> "*"
                  Public -> v
             in coerce $ Override k v' vis
     in DeploymentFullInfo (hidePrivate dep) s ct ut u

getFullInfo ::
  (MonadReader AppState m, MonadBaseControl IO m) =>
  FullInfoListType ->
  m [DeploymentFullInfo]
getFullInfo lType = do
  p <- asks pool
  liftBaseOp (withResource p) $ \conn -> getFullInfo' conn lType

-- | Helper to get full_info from the database.
getFullInfo' ::
  (MonadReader AppState m, MonadBase IO m) =>
  Connection ->
  FullInfoListType ->
  m [DeploymentFullInfo]
getFullInfo' conn listType = do
  AppState {logger = l} <- ask
  deployments <- do
    rows <- case listType of
      FullInfoForAll -> liftBase $ query_ conn qAll
      FullInfoOnlyForOne dName -> liftBase $ query conn qOne (Only dName)
    for rows $ \(n, t, ct, ut, st) -> do
      (appOvs, depOvs) <- liftBase $ selectOverrides conn n
      dMeta <- liftBase $ selectDeploymentMetadata conn n
      st' <-
        isDeploymentLocked n <&> \case
          True -> DeploymentPending st
          False -> DeploymentNotPending st
      pure $ do
        let dep = Deployment n t appOvs depOvs
        DeploymentFullInfo dep st' dMeta ct ut
  liftBase . logInfo l $ "get deployments: " <> (pack . show $ deployments)
  return deployments
  where
    qAll =
      "SELECT name, tag, extract(epoch from created_at)::int, \
      \extract(epoch from updated_at)::int, status \
      \FROM deployments ORDER BY name"
    qOne =
      "SELECT name, tag, extract(epoch from created_at)::int, \
      \extract(epoch from updated_at)::int, status \
      \FROM deployments \
      \WHERE name = ?"

-- | Handles the 'create' request.
createH :: Deployment -> AppM CommandResponse
createH dep = do
  failIfGracefulShutdownActivated
  unless (isNameValid $ name dep) $ do
    let badNameText =
          "Deployment name length should be longer than 2 characters, \
          \under 17 characters and begin with a letter."
    throwError
      err400
        { errBody = validationError [badNameText] []
        }
  t1 <- liftIO $ now
  st <- ask
  let pgPool = pool st
  failIfImageNotFound dep
  failIfGracefulShutdownActivated
  runDeploymentBgWorker
    Nothing
    (name dep)
    ( do
        let q =
              "INSERT INTO deployments (name, tag, status) \
              \VALUES (?, ?, ?) RETURNING id"
            createDep :: PgPool -> Deployment -> IO [Only Int]
            createDep p Deployment {name = n, tag = t} =
              withResource p $ \conn ->
                query conn q (n, t, CreatePending)
        res :: Either SqlError [Only Int] <- liftIO . try $ createDep pgPool dep
        case res of
          Right ((Only depId) : _) ->
            pure . DeploymentId $ depId
          Right [] ->
            throwError
              err404
                { errBody = validationError ["Name not found"] []
                }
          Left (SqlError code _ _ _ _)
            | code == unique_violation ->
              throwError
                err400
                  { errBody = validationError ["Deployment already exists"] []
                  }
          Left (SqlError _ _ _ _ _) ->
            throwError err409 {errBody = appError "Some database error"}
    )
    $ \dId -> do
      liftIO . withResource pgPool $ \conn ->
        upsertNewOverrides conn dId (appOverrides dep) (deploymentOverrides dep)
      liftBase $ sendReloadEvent st
      updateDeploymentInfo (name dep)
      (ec, out, err) <- liftBase $ createDeployment dep st
      t2 <- liftBase $ now
      let elTime = elapsedTime t2 t1
      withResource pgPool $ \conn ->
        -- calling it directly now is fine since there is no previous status.
        createDeploymentLog conn dep "create" ec elTime out err
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
  pgPool <- asks pool
  DeploymentFullInfo {deployment = dep} <-
    withResource pgPool $ \conn -> getDeploymentS conn dName
  (ec, out, err) <- runCommandArgs infoCommand =<< infoCommandArgs dep
  liftBase $
    case ec of
      ExitSuccess -> do
        dMeta <- parseDeploymentMetadata (lines . unStdout $ out)
        upsertDeploymentMetadata pgPool dName dMeta
      ExitFailure _ ->
        log $
          "could not get deployment info, exit code: " <> (pack . show $ ec)
            <> ", stdout: "
            <> coerce out
            <> "stderr: "
            <> coerce err

-- | Helper to create a new deployment.
createDeployment :: Deployment -> AppState -> IO (ExitCode, Stdout, Stderr)
createDeployment dep st = do
  let log :: Text -> IO ()
      log = logInfo (logger st)
      args =
        [ "--project-name"
        , coerce $ projectName st
        , "--base-domain"
        , coerce $ baseDomain st
        , "--namespace"
        , coerce $ namespace st
        , "--name"
        , coerce $ name dep
        , "--tag"
        , coerce $ tag dep
        ]
          ++ applicationOverridesToArgs (appOverrides dep)
          ++ deploymentOverridesToArgs (deploymentOverrides dep)
      cmd = coerce $ creationCommand st

  liftIO $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    log $ "deployment created, deployment: " <> (pack . show $ dep)
    pure (ec, out, err)

-- | Helper to get deployment logs.
selectDeploymentLogs ::
  PgPool ->
  DeploymentId ->
  IO [DeploymentLog]
selectDeploymentLogs pgPool dId = do
  let q =
        "SELECT id, action::text, tag, exit_code, \
        \duration, extract(epoch from created_at)::int \
        \FROM deployment_logs \
        \WHERE deployment_id = ? \
        \ORDER BY created_at DESC \
        \LIMIT 20"
  withResource pgPool $ \conn -> do
    rows <- query conn q (Only . unDeploymentId $ dId)
    -- FIXME: use FromRow instance instead
    for rows $ \(ai, a, t, ec, d, ts) -> do
      (appOvs, depOvs) <- selectLogOverrides conn (ActionId ai)
      pure $ DeploymentLog (ActionId ai) a t appOvs depOvs ec (Duration d) ts

-- | Helper to get a deployment.
selectDeployment ::
  DeploymentName ->
  AppM Deployment
selectDeployment dName = do
  pgPool <- asks pool
  liftBaseOp (withResource pgPool) $ \conn ->
    liftIO (selectDeploymentIO conn dName) >>= either throwError pure

selectDeploymentIO ::
  Connection ->
  DeploymentName ->
  IO (Either ServerError Deployment)
selectDeploymentIO conn dName = do
  let q = "SELECT name, tag FROM deployments WHERE name = ?"
  retrieved <- liftBase $ query conn q (Only dName)
  case retrieved of
    [(n, t)] -> do
      (appOvs, stOvs) <- liftBase $ selectOverrides conn n
      pure . Right $ Deployment n t appOvs stOvs
    [] -> pure . Left $ err404 {errBody = "Deployment not found."}
    _ -> pure . Left $ err500

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
processOutput (TransitionRestore x) = Just (x, Action "restore")
processOutput (TransitionArchivePending x) = Just (x, Action "archive")
processOutput (TransitionUpdatePending x) = Just (x, Action "update")
processOutput (TransitionCreatePending x) = Just (x, Action "create")
processOutput TransitionFailure {} = Nothing

transitionToStatusS ::
  (MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m, MonadIO m) =>
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
  (MonadReader AppState m, MonadBase IO m, MonadError ServerError m) =>
  Connection ->
  DeploymentName ->
  m DeploymentFullInfo
getDeploymentS conn dName =
  (getFullInfo' conn (FullInfoOnlyForOne dName)) >>= \case
    [x] -> return x
    _ -> throwError err404 {errBody = "Deployment not found."}

transitionToStatus ::
  (MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m, MonadIO m) =>
  DeploymentName ->
  DeploymentStatusTransition ->
  ExceptT StatusTransitionError m ()
transitionToStatus dName s = do
  p <- asks pool
  st <- ask
  let log = liftBase . logInfo (logger st)
  (oldS, newS, dep :: DeploymentFullInfo) <- withResource p $ \conn -> liftBaseOp_ (withTransaction conn) $ do
    dep <- lift $ getDeploymentS conn dName
    let oldS = recordedStatus $ dep ^. #status
        newS = transitionStatus s
    assertStatusTransitionPossible dName oldS newS
    log $ "Transitioning deployment " <> (show' . unDeploymentName) dName <> " " <> show' s
    let q =
          "UPDATE deployments \
          \SET status = ?, status_updated_at = now() "
            <> (if newS == ArchivePending then ", archived_at = now()" else mempty)
            <> " WHERE name = ?"
    void . liftBase $ execute conn q (newS, dName)
    lift $
      forM_ (processOutput s) $ \(output, act) ->
        createDeploymentLog
          conn
          (dep ^. #deployment)
          act
          (output ^. #exitCode)
          (output ^. #duration)
          (output ^. #stdout)
          (output ^. #stderr)
    return (oldS, newS, dep)
  notificationCmd <- asks notificationCommand
  forM_ notificationCmd $ \nCmd ->
    runBgWorker . void $
      runCommandArgs' nCmd
        =<< notificationCommandArgs dName (dep ^. #deployment . #tag) oldS newS
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
  p <- asks pool
  dep <- withResource p $ \conn ->
    (getFullInfo' conn (FullInfoOnlyForOne dName)) >>= \case
      [x] -> return x
      _ -> throwError $ DeploymentNotFound dName
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
  t1 <- liftBase now
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
    t2 <- liftBase now
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
  t1 <- liftIO $ now
  st <- ask
  let DeploymentUpdate
        { newTag = dTag
        , newAppOverrides = newAppOvs
        , oldAppOverrides = oldAppOvs
        , newDeploymentOverrides = newDepOvs
        , oldDeploymentOverrides = oldDepOvs
        } = dUpdate
      pgPool = pool st
      log = logInfo (logger st)
  dId <- selectDeploymentId pgPool dName
  olDep <- selectDeployment dName
  failIfImageNotFound (applyDeploymentUpdate dUpdate olDep)
  failIfGracefulShutdownActivated
  runDeploymentBgWorker (Just UpdatePending) dName (pure ()) $ \() -> do
    (appOvs, depOvs) <- liftBase . withResource pgPool $ \conn ->
      withTransaction conn $ do
        deleteOldOverrides conn dId oldAppOvs oldDepOvs
        upsertNewOverrides conn dId newAppOvs newDepOvs
        updateTag conn dId dTag
        selectOverrides conn dName
    updateDeploymentInfo dName
    liftBase $ sendReloadEvent st
    let args =
          [ "--project-name"
          , coerce $ projectName st
          , "--base-domain"
          , coerce $ baseDomain st
          , "--namespace"
          , coerce $ namespace st
          , "--name"
          , coerce $ dName
          , "--tag"
          , coerce $ dTag
          ]
            ++ applicationOverridesToArgs appOvs
            ++ deploymentOverridesToArgs depOvs
        cmd = coerce $ updateCommand st
    liftBase . log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- liftBase $ runCommand (unpack cmd) (unpack <$> args)
    liftBase . log $
      "deployment updated, name: "
        <> coerce dName
        <> ", tag: "
        <> coerce dTag
    t2 <- liftBase now
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

-- | Helper to get overrides from the database.
selectOverrides ::
  Connection ->
  DeploymentName ->
  IO (ApplicationOverrides, DeploymentOverrides)
selectOverrides conn dName = do
  let q =
        "SELECT key, value, scope::text, visibility::text \
        \FROM deployment_overrides \
        \WHERE deployment_id = ( \
        \SELECT id FROM deployments WHERE name = ? \
        \)"
      parseVis :: Text -> OverrideVisibility
      parseVis = read . unpack
      parseScope :: Text -> OverrideScope
      parseScope = read . unpack
      toOverrides (k, v, s, vis) (appOvs, depOvs) =
        pure $ case parseScope s of
          ApplicationScope ->
            (ApplicationOverride (Override k v $ parseVis vis) : appOvs, depOvs)
          DeploymentScope ->
            (appOvs, DeploymentOverride (Override k v $ parseVis vis) : depOvs)
  rows <- query conn q (Only dName)
  foldrM toOverrides ([], []) rows

-- | Helper to get override logs from the database.
selectLogOverrides ::
  Connection ->
  ActionId ->
  IO (ApplicationOverrides, DeploymentOverrides)
selectLogOverrides conn aId = do
  let q =
        "SELECT key, value, scope::text, visibility::text \
        \FROM deployment_log_overrides \
        \WHERE deployment_log_id = ?"
      parseVis :: Text -> OverrideVisibility
      parseVis = read . unpack
      parseScope :: Text -> OverrideScope
      parseScope = read . unpack
      toOverrides (k, v, s, vis) (appOvs, depOvs) =
        pure $ case parseScope s of
          ApplicationScope ->
            (ApplicationOverride (Override k v $ parseVis vis) : appOvs, depOvs)
          DeploymentScope ->
            (appOvs, DeploymentOverride (Override k v $ parseVis vis) : depOvs)
  rows <- query conn q (Only . unActionId $ aId)
  foldrM toOverrides ([], []) rows

-- | Helper to delete overrides from the database.
deleteOldOverrides ::
  Connection ->
  DeploymentId ->
  ApplicationOverrides ->
  DeploymentOverrides ->
  IO ()
deleteOldOverrides conn dId appOvs depOvs = do
  let q =
        "DELETE FROM deployment_overrides \
        \WHERE deployment_id = ? AND key = ? AND scope = ?"
      dId' = unDeploymentId dId
  void $
    for appOvs $ \o -> do
      let oKey = overrideKey . unApplicationOverride $ o
          oScope = show ApplicationScope
      execute conn q (dId', oKey, oScope)
  void $
    for depOvs $ \o -> do
      let oKey = overrideKey . unDeploymentOverride $ o
          oScope = show DeploymentScope
      execute conn q (dId', oKey, oScope)

-- | Helper to get the deployment id from the database.
selectDeploymentId :: PgPool -> DeploymentName -> AppM DeploymentId
selectDeploymentId pgPool dName = do
  dIds :: [(Only Int)] <- liftIO $
    withResource pgPool $ \conn ->
      query conn "SELECT id FROM deployments WHERE name = ?" (Only dName)
  case dIds of
    [(Only dId)] -> pure . DeploymentId $ dId
    [] ->
      throwError
        err404
          { errBody = validationError ["Name not found"] []
          }
    _ ->
      throwError
        err406
          { errBody = validationError ["More than one name found"] []
          }

-- | Helper to insert or update overrides.
upsertNewOverrides ::
  Connection ->
  DeploymentId ->
  ApplicationOverrides ->
  DeploymentOverrides ->
  IO ()
upsertNewOverrides conn dId appOvs depOvs = do
  let q =
        "INSERT INTO deployment_overrides \
        \(key, value, deployment_id, scope, visibility) \
        \VALUES (?, ?, ?, ?, ?) \
        \ON CONFLICT (key, deployment_id, scope) \
        \DO \
        \UPDATE SET value = ?, visibility = ?, updated_at = now()"
      dId' = unDeploymentId dId
  void $
    for appOvs $ \o -> do
      let oKey = overrideKey . unApplicationOverride $ o
          oValue = overrideValue . unApplicationOverride $ o
          oScope = show ApplicationScope
          oVis = show . overrideVisibility . unApplicationOverride $ o
      execute conn q (oKey, oValue, dId', oScope, oVis, oValue, oVis)
  void $
    for depOvs $ \o -> do
      let oKey = overrideKey . unDeploymentOverride $ o
          oValue = overrideValue . unDeploymentOverride $ o
          oScope = show DeploymentScope
          oVis = show . overrideVisibility . unDeploymentOverride $ o
      execute conn q (oKey, oValue, dId', oScope, oVis, oValue, oVis)

updateTag ::
  Connection ->
  DeploymentId ->
  DeploymentTag ->
  IO ()
updateTag conn (DeploymentId dId) (DeploymentTag dTag) = do
  let q = "UPDATE deployments SET tag=? WHERE id=?;"
  void $ execute conn q (dTag, dId)

-- | Handles the 'info' request of the Web UI API.
infoH :: DeploymentName -> AppM [DeploymentInfo]
infoH dName = do
  st <- ask
  dInfo <- getInfo dName
  liftIO . logInfo (logger st) $
    "get deployment info: " <> (pack . show $ dInfo)
  pure [hidePrivateOverridesInInfo dInfo]

-- | Handles the 'info' request of the octo CLI API.
powerInfoH :: DeploymentName -> AppM [DeploymentInfo]
powerInfoH dName = do
  st <- ask
  dInfo <- getInfo dName
  liftIO . logInfo (logger st) $
    "get deployment info: " <> (pack . show $ dInfo)
  pure [dInfo]

-- | Hides private overrides of 'info' response.
hidePrivateOverridesInInfo :: DeploymentInfo -> DeploymentInfo
hidePrivateOverridesInInfo (DeploymentInfo dep dMeta dLogs) =
  let dep' =
        let (Deployment n t ao so) = dep
         in Deployment n t (hideP ao) (hideP so)
      dLogs' =
        dLogs <&> \(DeploymentLog ai a t ao so ec d ct) ->
          DeploymentLog ai a t (hideP ao) (hideP so) ec d ct
      hideP o =
        coerce o <&> \(Override k v vis) ->
          let v' = case vis of
                Private -> "*"
                Public -> v
           in coerce $ Override k v' vis
   in DeploymentInfo dep' dMeta dLogs'

-- | Helper to get deployment info from the database.
getInfo :: DeploymentName -> AppM DeploymentInfo
getInfo dName = do
  st <- ask
  let pgPool = pool st
  dep <- selectDeployment dName
  dId <- selectDeploymentId pgPool dName
  liftIO $ do
    depLogs <- selectDeploymentLogs pgPool dId
    dMeta <- withResource pgPool $ \conn ->
      selectDeploymentMetadata conn dName
    let depInfo = DeploymentInfo dep dMeta $ reverse depLogs
    pure depInfo

-- | Handles the 'ping' request.
pingH :: AppM NoContent
pingH = do
  pgPool <- pool <$> ask
  _ :: [Only Int] <- liftIO $
    withResource pgPool $ \conn ->
      query_ conn "SELECT 1"
  pure NoContent

-- | Handles the 'project_name' request.
projectNameH :: AppM ProjectName
projectNameH = projectName <$> ask

-- | Handles the 'status' request.
statusH :: DeploymentName -> AppM CurrentDeploymentStatus
statusH dName = do
  pgPool <- asks pool
  dep <- withResource pgPool $ \conn -> getDeploymentS conn dName
  (ec, _, _) <- runCommandArgs checkingCommand =<< checkCommandArgs (dep ^. #deployment)
  pure . CurrentDeploymentStatus $
    case ec of
      ExitSuccess -> Ok
      _ -> Error

-- | Handles the 'cleanup' request.
cleanupH :: DeploymentName -> AppM CommandResponse
cleanupH dName = do
  failIfGracefulShutdownActivated
  st <- ask
  runDeploymentBgWorker Nothing dName (pure ()) $ \() -> liftBase $ cleanupDeployment dName st
  pure Success

-- | Helper to cleanup deployment.
cleanupDeployment :: DeploymentName -> AppState -> IO ()
cleanupDeployment dName st = do
  let log = logInfo (logger st)
      pgPool = pool st
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
  log $ "call " <> unwords (cmd : args)
  (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
  print out >> print err
  void $ deleteDeploymentLogOverrides pgPool dName
  void $ deleteDeploymentLogs pgPool dName
  void $ deleteDeploymentMetadata pgPool dName
  void $ deleteDeploymentOverrides pgPool dName
  void $ deleteDeployment pgPool dName
  log $ "deployment destroyed, name: " <> coerce dName
  sendReloadEvent st
  handleExitCode ec

-- | Helper to delete deployment log overrides.
deleteDeploymentLogOverrides :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogOverrides p n = withResource p $ \conn ->
  execute
    conn
    "DELETE FROM deployment_log_overrides WHERE deployment_log_id in ( \
    \SELECT id FROM deployment_logs WHERE deployment_id in ( \
    \SELECT id FROM deployments where name = ? \
    \) \
    \)"
    (Only n)

-- | Helper to delete deployment logs.
deleteDeploymentLogs :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogs p n = withResource p $ \conn ->
  execute
    conn
    "DELETE FROM deployment_logs WHERE deployment_id in\
    \  (SELECT id FROM deployments where name = ?)"
    (Only n)

-- | Helper to delete deployment overrides.
deleteDeploymentOverrides :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentOverrides p n = withResource p $ \conn ->
  execute
    conn
    "DELETE FROM deployment_overrides WHERE deployment_id in\
    \  (SELECT id FROM deployments where name = ?)"
    (Only n)

-- | Helper to delete a deployment.
deleteDeployment :: PgPool -> DeploymentName -> IO Int64
deleteDeployment p n = withResource p $ \conn ->
  execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

-- | Handles the 'clean-archive' request.
cleanArchiveH :: AppM CommandResponse
cleanArchiveH = do
  failIfGracefulShutdownActivated
  st <- ask
  let pgPool = pool st
      archRetention = unArchiveRetention . archiveRetention $ st
      q =
        "SELECT name FROM deployments \
        \WHERE status in ? AND archived_at + interval '?' second < now()"
  retrieved :: [Only DeploymentName] <- liftIO $
    withResource pgPool $ \conn -> query conn q (In archivedStatuses, archRetention)
  runBgWorker . void $
    for retrieved $ \(Only dName) ->
      runDeploymentBgWorker Nothing dName (pure ()) $ \() -> liftBase $ cleanupDeployment dName st

  pure Success

-- | Handles the 'restore' request.
restoreH :: DeploymentName -> AppM CommandResponse
restoreH dName = do
  failIfGracefulShutdownActivated
  t1 <- liftIO $ now
  st <- ask
  dep <- selectDeployment dName
  failIfImageNotFound dep
  failIfGracefulShutdownActivated
  runDeploymentBgWorker (Just CreatePending) dName (pure ()) $ \() -> do
    dep' <- selectDeployment dName
    updateDeploymentInfo dName
    (ec, out, err) <- liftBase $ createDeployment dep' st
    t2 <- liftBase now
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
  st <- ask
  let pgPool = pool st
      aId' = Only . unActionId $ aId
      q = "SELECT stdout, stderr FROM deployment_logs WHERE id = ?"
  rows :: [(Text, Text)] <- liftIO $
    withResource pgPool $ \conn -> query conn q aId'
  case rows of
    (out, err) : _ -> pure $ ActionInfo out err
    _ ->
      throwError err400 {errBody = appError "Action not found"}

-- | Helper to handle exit code.
handleExitCode :: MonadBaseControl IO m => ExitCode -> m ()
handleExitCode ExitSuccess = return ()
handleExitCode (ExitFailure c) = liftBase . throwIO $ DeploymentFailed c

-- | Helper to log a deployment action.
createDeploymentLog ::
  (MonadError ServerError m, MonadIO m) =>
  Connection ->
  Deployment ->
  Action ->
  ExitCode ->
  Duration ->
  Stdout ->
  Stderr ->
  m ()
createDeploymentLog conn dep act ec dur out err = do
  let (Deployment dName dTag appOvs depOvs) = dep
      exitCode' =
        case ec of
          ExitSuccess -> 0
          ExitFailure errCode -> errCode
      dur' = unDuration dur
      out' = unStdout out
      err' = unStderr err
      qInsertLog =
        "INSERT INTO deployment_logs \
        \(deployment_id, action, tag, exit_code, \
        \duration, stdout, stderr) \
        \(\
        \SELECT id, ?, ?, ?, ?, ?, ? \
        \FROM deployments \
        \WHERE name = ? \
        \) RETURNING id"
      qInsertLogOverride =
        "INSERT INTO deployment_log_overrides \
        \(key, value, deployment_log_id, scope, visibility) \
        \VALUES (?, ?, ?, ?, ?)"
  (Only aId) :: Only Int <-
    liftIO
      ( query
          conn
          qInsertLog
          (act, dTag, exitCode', dur', out', err', dName)
      )
      >>= ensureOne
  void $
    for appOvs $ \o -> do
      let oKey = overrideKey . unApplicationOverride $ o
          oValue = overrideValue . unApplicationOverride $ o
          oScope = show ApplicationScope
          oVis = show . overrideVisibility . unApplicationOverride $ o
      liftIO $ execute conn qInsertLogOverride (oKey, oValue, aId, oScope, oVis)
  void $
    for depOvs $ \o -> do
      let oKey = overrideKey . unDeploymentOverride $ o
          oValue = overrideValue . unDeploymentOverride $ o
          oScope = show DeploymentScope
          oVis = show . overrideVisibility . unDeploymentOverride $ o
      liftIO $ execute conn qInsertLogOverride (oKey, oValue, aId, oScope, oVis)

ensureOne :: MonadError ServerError m => [a] -> m a
ensureOne [x] = return x
ensureOne _ = throwError err500

-- | Helper to get deployment metadata from the database.
selectDeploymentMetadata ::
  Connection ->
  DeploymentName ->
  IO [DeploymentMetadata]
selectDeploymentMetadata conn dName = do
  let q =
        "SELECT key, value FROM deployment_metadata \
        \WHERE deployment_id = (SELECT id FROM deployments WHERE name = ?) \
        \ORDER BY id ASC"
  rows <- query conn q (Only dName)
  for rows $ \(k, v) -> pure $ DeploymentMetadata k v

-- | Helper to delete deployment metadata.
deleteDeploymentMetadata ::
  PgPool ->
  DeploymentName ->
  IO ()
deleteDeploymentMetadata pgPool dName = do
  let q =
        "DELETE FROM deployment_metadata \
        \WHERE deployment_id = (SELECT id FROM deployments WHERE name = ?)"
  void $ withResource pgPool $ \conn -> execute conn q (Only dName)

-- | Helper to insert or update deployment metadata.
upsertDeploymentMetadata ::
  PgPool ->
  DeploymentName ->
  [DeploymentMetadata] ->
  IO ()
upsertDeploymentMetadata pgPool dName dMetadatas = do
  withResource pgPool $ \conn -> withTransactionSerializable conn $ do
    void $
      execute
        conn
        "DELETE FROM deployment_metadata \
        \WHERE deployment_id = (SELECT id FROM deployments WHERE name = ?)"
        (Only dName)
    forM_ dMetadatas $ \dMeta ->
      execute
        conn
        "INSERT INTO deployment_metadata \
        \(deployment_id, key, value, created_at, updated_at) \
        \(SELECT id, ?, ?, now(), now() FROM deployments WHERE name = ?)"
        (deploymentMetadataKey dMeta, deploymentMetadataValue dMeta, dName)

-- | Checks the existence of a deployment tag.
-- Returns 404 'Tag not found' response if the deployment tag doesn't exist.
failIfImageNotFound :: Deployment -> AppM ()
failIfImageNotFound dep = do
  (ec, _, _) <- runCommandArgs tagCheckingCommand =<< tagCheckCommandArgs dep
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
elapsedTime :: Time -> Time -> Duration
elapsedTime t1 t2 =
  Duration . fromIntegral . (`div` 1000000) . abs $ getTime t2 - getTime t1

-- | Runs the status updater.
runStatusUpdater :: AppState -> IO ()
runStatusUpdater state = do
  let pgPool = pool state
      interval = 30 :: Int
      selectDeps =
        "SELECT name, status::text, \
        \extract(epoch from now())::int - \
        \extract(epoch from status_updated_at)::int \
        \, tag \
        \FROM deployments \
        \WHERE checked_at < now() - interval '?' second AND status != 'Archived'"
      updateCheckedAt =
        "UPDATE deployments SET checked_at = now() WHERE name = ? AND status = ?"
      logErr :: Text -> IO ()
      logErr = logWarning (logger state)

  forever $ do
    rows :: [(DeploymentName, DeploymentStatus, Int, DeploymentTag)] <- liftIO $
      withResource pgPool $ \conn -> query conn selectDeps (Only interval)
    let checkList :: [(DeploymentName, DeploymentStatus, Timestamp, DeploymentTag)] =
          (\(n, s, t, dTag) -> (n, s, coerce t, dTag)) <$> rows
    checkResult <- for checkList $ \(dName, dStatus, ts, _) -> do
      let timeout = statusUpdateTimeout state
      (ec, _, _) <- flip runReaderT state case dStatus of
        ArchivePending -> runCommandArgs archiveCheckingCommand =<< archiveCheckArgs dName
        _ -> do
          liftBase (withResource pgPool $ \conn -> selectDeploymentIO conn dName) >>= \case
            Right dep -> runCommandArgs checkingCommand =<< checkCommandArgs dep
            Left err -> do
              log <- asks logger
              let err' = T.pack $ displayException $ err
              liftIO $ logWarning log err'
              pure (ExitFailure 1, Stdout "Didn't call script", Stderr err')

      pure (dName, statusTransition ec dStatus ts timeout, dStatus)
    updated <-
      for checkResult $ \(dName, transitionM, dStatus) ->
        withResource pgPool $ \conn ->
          case transitionM of
            Nothing -> execute conn updateCheckedAt (dName, dStatus) $> False
            Just transition ->
              ($> True) $
                (flip runReaderT state . runExceptT . runExceptT) (transitionToStatus dName transition) >>= \case
                  Right (Right ()) -> void $ execute conn updateCheckedAt (dName, transitionStatus transition)
                  Left e -> logErr $ show' e
                  Right (Left e) -> logErr $ show' e
    when (or updated) $ sendReloadEvent state
    threadDelay 5000000

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
statusTransition (ExitFailure n) CreatePending ts timeout
  | ts > coerce timeout =
    Just . TransitionFailure $ failureStatusType n
statusTransition (ExitFailure n) UpdatePending ts timeout
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
