module Octopod.Server (runOctopodServer) where


import           Chronos (Time, getTime, now)
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race_)
import qualified Control.Concurrent.Lifted as L
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception (Exception, throwIO, try)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson (Value(..), encode, toJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce
import           Data.Conduit (ConduitT, yield)
import           Data.Foldable (foldrM)
import           Data.Functor
import           Data.Int (Int64)
import           Data.IORef
import           Data.Maybe
import           Data.Pool
import           Data.Text (lines, pack, unpack, unwords)
import           Data.Text.IO (hGetContents)
import           Data.Traversable
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Options.Generic
import           PostgreSQL.ErrorCodes (unique_violation)
import           Prelude hiding (lines, log, unlines, unwords)
import           Servant
import           System.Environment (lookupEnv)
import           System.Exit
import           System.Log.FastLogger
import           System.Posix.Signals (sigTERM)
import           System.Process.Typed


import           Common.Validation (isNameValid)
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import qualified Data.ByteString.Lazy.Char8 as BSLC
import           Data.Generics.Labels ()
import qualified Data.Text.Encoding as T
import           Database.PostgreSQL.Simple.Transaction
import           Octopod.API
import           Octopod.PowerAPI
import           Octopod.Server.Args
import           Octopod.Server.ControlScriptUtils
import           Octopod.Server.Logger
import           Octopod.Server.Posix
import           Orphans ()
import           TLS (createTLSOpts)
import           Types

type PgPool = Pool Connection
type AppM   = ReaderT AppState Handler

-- | Octopod Server state definition.
data AppState = AppState
  { pool :: PgPool
  -- ^ postgres pool
  , logger :: TimedFastLogger
  -- ^ logger
  , eventSink :: TChan WSEvent
  -- ^ channel for WS events for the frontend
  , bgWorkersCounter :: IORef Int
  -- ^ background workers counter
  , gracefulShutdownActivated :: IORef Bool
  -- ^ flag of activating graceful shutdown
  , shutdownSem :: MVar ()
  -- ^ semaphore for graceful shutdown
  , projectName :: ProjectName
  -- ^ project name
  , baseDomain :: Domain
  -- ^ base domain
  , namespace :: Namespace
  -- ^ namespace
  , archiveRetention :: ArchiveRetention
  -- ^ archive retention
  , statusUpdateTimeout :: Timeout
  -- ^ status update timeout
  , creationCommand :: Command
  -- ^ creation command path
  , updateCommand :: Command
  -- ^ update command path
  , archiveCommand :: Command
  -- ^ deletion command path
  , checkingCommand :: Command
  -- ^ checking command path
  , cleanupCommand :: Command
  -- ^ cleanup command path
  , archiveCheckingCommand :: Command
  -- ^ archive checking command path
  , tagCheckingCommand :: Command
  -- ^ tag checking command path
  , infoCommand :: Command
  }

-- | Deployment exception definition.
data DeploymentException
  = DeploymentFailed Int
  | ThereIsActiveDeployment
  deriving (Show)

instance Exception DeploymentException

-- | Deployment list type definition.
data DeploymentListType
  = AllDeployments
  | ArchivedOnlyDeployments
  | ActiveOnlyDeployments
  deriving (Show)

-- | Full info list type definition.
data FullInfoListType
  = FullInfoForAll
  | FullInfoOnlyForOne DeploymentName
  deriving (Show)

runOctopodServer :: IO ()
runOctopodServer = do
  logger' <- newLogger
  logInfo logger' "started"
  bgWorkersC <- newIORef 0
  gracefulShutdownAct <- newIORef False
  shutdownS <- newEmptyMVar
  void $ do
    let
      termHandler = terminationHandler bgWorkersC gracefulShutdownAct shutdownS
    installShutdownHandler logger' [sigTERM] termHandler
  opts <- parseArgs
  let
    a ?! e = a >>= maybe (die e) pure
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
  pgPool <- initConnectionPool
    (unDBConnectionString $ octopodDB opts)
    (unDBPoolSize $ octopodDBPoolSize opts)
  channel <- liftIO . atomically $ newBroadcastTChan
  let
    appSt        =
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
    app'         = app appSt
    powerApp'    = powerApp appSt
    wsApp'       = wsApp channel
    serverPort   = octopodPort opts
    uiServerPort = unServerPort $ octopodUIPort opts
    wsServerPort = unServerPort $ octopodWSPort opts
    tlsOpts      =
      createTLSOpts (octopodTLSCertPath opts) (octopodTLSKeyPath opts)
        (octopodTLSStorePath opts) serverPort
    warpOpts     = setPort (unServerPort serverPort) defaultSettings
    in
      (run uiServerPort app')
      `race_` (runTLS tlsOpts warpOpts powerApp')
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
    :<|> infoH :<|> fullInfoH :<|> statusH :<|> restoreH
  ) :<|> pingH :<|> projectNameH

-- | Application with the octo CLI API.
powerApp :: AppState -> Application
powerApp s = serve api $ hoistServer api (nt s) powerServer
  where
    api = Proxy @PowerAPI

-- | Request handlers of application with the octo CLI API.
powerServer :: ServerT PowerAPI AppM
powerServer =
  ( powerListH :<|> createH :<|> archiveH :<|> updateH
    :<|> powerInfoH :<|> powerFullInfoH :<|> statusH :<|> cleanupH :<|> restoreH
  ) :<|> getActionInfoH :<|> cleanArchiveH

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
fullInfoH
  :: (MonadReader AppState m, MonadBaseControl IO m, MonadError ServerError m)
  => DeploymentName -> m DeploymentFullInfo
fullInfoH dName = do
  fullInfoList <- hidePrivateOverridesInFullInfos <$>
    getFullInfo (FullInfoOnlyForOne dName)
  case fullInfoList of
    fullInfo : _ -> pure fullInfo
    [] -> throwError err404
      { errBody = validationError ["Name not found"] [] }

-- | Handles the 'full_info' request of the octo CLI API.
powerFullInfoH :: DeploymentName -> AppM DeploymentFullInfo
powerFullInfoH dName = do
  fullInfoList <- getFullInfo $ FullInfoOnlyForOne dName
  case fullInfoList of
    fullInfo : _ -> pure fullInfo
    [] -> throwError err404
      { errBody = validationError ["Name not found"] [] }

-- | Hides private overrides in 'full_info' response.
hidePrivateOverridesInFullInfos :: [DeploymentFullInfo] -> [DeploymentFullInfo]
hidePrivateOverridesInFullInfos dFullInfos =  do
  dFullInfos <&> \(DeploymentFullInfo dep s ct ut u) ->
    let
      hidePrivate (Deployment n t appOvs depOvs) =
        Deployment n t (hideP appOvs) (hideP depOvs)
      hideP o =
        coerce o <&> \(Override k v vis) ->
          let
            v' = case vis of
              Private -> "*"
              Public -> v
          in coerce $ Override k v' vis
    in DeploymentFullInfo (hidePrivate dep) s ct ut u

getFullInfo
  :: (MonadReader AppState m, MonadBaseControl IO m)
  => FullInfoListType
  -> m [DeploymentFullInfo]
getFullInfo lType = do
  p <- asks pool
  liftBaseOp (withResource p) $ \conn -> getFullInfo' conn lType

-- | Helper to get full_info from the database.
getFullInfo'
  :: (MonadReader AppState m, MonadBase IO m)
  => Connection
  -> FullInfoListType
  -> m [DeploymentFullInfo]
getFullInfo' conn listType = do
  AppState {logger = l} <- ask
  deployments <- liftBase $ do
    rows <- case listType of
      FullInfoForAll -> query_ conn qAll
      FullInfoOnlyForOne dName -> query conn qOne (Only dName)
    for rows $ \(n, t, ct, ut, st) -> do
      (appOvs, depOvs) <- selectOverrides conn n
      dMeta <- selectDeploymentMetadata conn n
      pure $ do
        let dep = Deployment n t appOvs depOvs
        DeploymentFullInfo dep (read st) dMeta ct ut
  liftBase . logInfo l $ "get deployments: " <> (pack . show $ deployments)
  return deployments
  where
    qAll                 =
      "SELECT name, tag, extract(epoch from created_at)::int, \
        \extract(epoch from updated_at)::int, status::text \
      \FROM deployments ORDER BY name"
    qOne                 =
      "SELECT name, tag, extract(epoch from created_at)::int, \
        \extract(epoch from updated_at)::int, status::text \
      \FROM deployments \
      \WHERE name = ?"

-- | Handles the 'create' request.
createH :: Deployment -> AppM CommandResponse
createH dep = do
  failIfGracefulShutdownActivated
  unless (isNameValid $ name dep) $ do
    let
      badNameText =
        "Deployment name length should be longer than 2 characters, \
        \under 17 characters and begin with a letter."
    throwError err400
      { errBody = validationError [badNameText] [] }
  t1 <- liftIO $ now
  st <- ask
  let
    q                                            =
      "INSERT INTO deployments (name, tag, status) \
      \VALUES (?, ?, ?) RETURNING id"
    pgPool                                       = pool st
    createDep :: PgPool -> Deployment -> IO [Only Int]
    createDep p Deployment { name = n, tag = t } =
      withResource p $ \conn ->
        query conn q (n, t, show CreatePending)
  failIfImageNotFound (name dep) (tag dep)
  failIfGracefulShutdownActivated
  res :: Either SqlError [Only Int] <- liftIO . try $ createDep pgPool dep
  dId <- case res of
    Right ((Only depId) : _) ->
      pure . DeploymentId $ depId
    Right [] ->
      throwError err404
        { errBody = validationError ["Name not found"] [] }
    Left (SqlError code _ _ _ _) | code == unique_violation ->
      throwError err400
        { errBody = validationError ["Deployment already exists"] [] }
    Left (SqlError _ _ _ _ _) ->
      throwError err409 { errBody = appError "Some database error" }
  liftIO . withResource pgPool $ \conn ->
    upsertNewOverrides conn dId (appOverrides dep) (deploymentOverrides dep)
  runBgWorker $ liftBase $ do
    sendReloadEvent st
    updateDeploymentInfo (name dep) st
    (ec, out, err) <- createDeployment dep st
    t2 <- now
    let
      elTime = elapsedTime t2 t1
    liftIO . withResource pgPool $ \conn ->
      -- calling it directly now is fine since there is no previous status.
      createDeploymentLog conn dep "create" ec elTime out err
    sendReloadEvent st
    handleExitCode ec
  pure Success

-- | Updates deployment info.
updateDeploymentInfo :: DeploymentName -> AppState -> IO ()
updateDeploymentInfo dName st = do
  let
    log  = logWarning (logger st)
    args = infoCommandArgs (projectName st) (baseDomain st) (namespace st) dName
    cmd  = coerce $ infoCommand st
  liftIO $ do
    (ec, out, err) <- runCommand (unpack cmd) (coerce args)
    case ec of
      ExitSuccess -> do
        dMeta <- parseDeploymentMetadata (lines . unStdout $ out)
        upsertDeploymentMetadata (pool st) dName dMeta
      ExitFailure _ ->
        log $
          "could not get deployment info, exit code: " <> (pack . show $ ec)
          <> ", stdout: " <> coerce out <> "stderr: " <> coerce err

-- | Helper to create a new deployment.
createDeployment :: Deployment -> AppState -> IO (ExitCode, Stdout, Stderr)
createDeployment dep st = do
  let
    log :: Text -> IO ()
    log  = logInfo (logger st)
    args =
      [ "--project-name", coerce $ projectName st
      , "--base-domain", coerce $ baseDomain st
      , "--namespace", coerce $ namespace st
      , "--name", coerce $ name dep
      , "--tag", coerce $ tag dep
      ] ++ applicationOverridesToArgs (appOverrides dep)
        ++ deploymentOverridesToArgs (deploymentOverrides dep)
    cmd  = coerce $ creationCommand st

  liftIO $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    log $ "deployment created, deployment: " <> (pack . show $ dep)
    pure (ec, out, err)

-- | Converts an application-level override list to command arguments.
applicationOverrideToArg :: ApplicationOverride -> [Text]
applicationOverrideToArg o = ["--app-env-override", overrideToArg . coerce $ o]

-- | Helper to convert an application-level override to command arguments.
applicationOverridesToArgs :: ApplicationOverrides -> [Text]
applicationOverridesToArgs ovs = concat [applicationOverrideToArg o | o <- ovs ]

-- | Converts a deployment-level override list to command arguments.
deploymentOverrideToArg :: DeploymentOverride -> [Text]
deploymentOverrideToArg o =
  ["--deployment-override", overrideToArg . coerce $ o]

-- | Helper to convert a deployment-level override to command arguments.
deploymentOverridesToArgs :: DeploymentOverrides -> [Text]
deploymentOverridesToArgs ovs = concat [deploymentOverrideToArg o | o <- ovs]

-- | Helper to get deployment logs.
selectDeploymentLogs
  :: PgPool
  -> DeploymentId
  -> IO [DeploymentLog]
selectDeploymentLogs pgPool dId = do
  let
    q =
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
selectDeployment
  :: DeploymentName
  -> AppM Deployment
selectDeployment dName = do
  pgPool <- asks pool
  let q = "SELECT name, tag FROM deployments WHERE name = ?"
  liftBaseOp (withResource pgPool) $ \conn -> do
    retrieved <- liftBase $ query conn q (Only dName)
    case retrieved of
      [(n, t)] -> do
        (appOvs, stOvs) <- liftBase $ selectOverrides conn n
        pure $ Deployment n t appOvs stOvs
      [] -> throwError err404 {errBody = "Deployment not found."}
      _ -> throwError err500

data StatusTransitionProcessOutput = StatusTransitionProcessOutput
  { exitCode :: ExitCode
  , duration :: Duration
  , stdout :: Stdout
  , stderr :: Stderr
  } deriving (Generic, Show)

data DeploymentStatusTransition
  = TransitionArchived
  | TransitionCreate
  | TransitionUpdate
  | TransitionRestore StatusTransitionProcessOutput
  | TransitionArchivePending StatusTransitionProcessOutput
  | TransitionUpdatePending StatusTransitionProcessOutput
  | TransitionCreatePending StatusTransitionProcessOutput
  | TransitionFailure
  deriving Show

transitionStatus :: DeploymentStatusTransition -> DeploymentStatus
transitionStatus TransitionArchived {} = Archived
transitionStatus TransitionCreate {} = Running
transitionStatus TransitionUpdate {} = Running
transitionStatus TransitionRestore {} = Running
transitionStatus TransitionArchivePending {} = ArchivePending
transitionStatus TransitionUpdatePending {} = UpdatePending
transitionStatus TransitionCreatePending {} = CreatePending
transitionStatus TransitionFailure {} = Failure

processOutput :: DeploymentStatusTransition -> Maybe (StatusTransitionProcessOutput, Action)
processOutput TransitionArchived = Nothing
processOutput TransitionCreate = Nothing
processOutput TransitionUpdate = Nothing
processOutput (TransitionRestore x) = Just (x, Action "restore")
processOutput (TransitionArchivePending x) = Just (x, Action "archive")
processOutput (TransitionUpdatePending x) = Just (x, Action "update")
processOutput (TransitionCreatePending x) = Just (x, Action "create")
processOutput TransitionFailure = Nothing

transitionToStatusS
  :: (MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m)
  => DeploymentName -> DeploymentStatusTransition -> m ()
transitionToStatusS dName tran = runExceptT (transitionToStatus dName tran) >>= \case
  Right x -> return x
  Left (InvalidStatusTransition a b) -> throwError err409
    { errBody
      = "Unable to transition deployment "
      <> (BSL.fromStrict . T.encodeUtf8 . unDeploymentName) dName
      <> " from " <> show'' a <> " to " <> show'' b <> "."
    }
  Left (DeploymentNotFound _) -> throwError err404
    { errBody
      = "Couldn't find deployment "
      <> (BSL.fromStrict . T.encodeUtf8 . unDeploymentName) dName <> "."
    }
  where
    show'' = BSLC.pack . show

transitionToStatus
  :: (MonadError ServerError m, MonadReader AppState m, MonadBaseControl IO m)
  => DeploymentName -> DeploymentStatusTransition -> ExceptT StatusTransitionError m ()
transitionToStatus dName s = do
  p <- asks pool
  st <- ask
  let log = liftBase . logInfo (logger st)
  () <- withResource p $ \conn -> liftBaseOp_ (withTransaction conn) . void $ do
    let newS = transitionStatus s
    dep <- lift (getFullInfo' conn (FullInfoOnlyForOne dName)) >>= \case
      [x] -> return x
      _ -> throwError $ DeploymentNotFound dName
    assertStatusTransitionPossible (dep ^. #status) newS
    log $ "Transitioning deployment " <> (show' . unDeploymentName) dName <> " " <> show' s
    let
      q =
        "UPDATE deployments \
        \SET status = ?, status_updated_at = now() "
        <> (if newS == ArchivePending then ", archived_at = now()" else mempty)
        <> " WHERE name = ?"
    void . liftBase $ execute conn q (show newS, dName)
    liftBase $ forM_ (processOutput s) $ \(output, act) ->
      createDeploymentLog
        conn
        (dep ^. #deployment)
        act
        (output ^. #exitCode)
        (output ^. #duration)
        (output ^. #stdout)
        (output ^. #stderr)
  liftBase $ sendReloadEvent st

assertStatusTransitionPossible
  :: MonadError StatusTransitionError m
  => DeploymentStatus
  -> DeploymentStatus
  -> m ()
assertStatusTransitionPossible old new =
  unless (new `elem` possibleTransitions old) (throwError $ InvalidStatusTransition old new)

data StatusTransitionError
  = InvalidStatusTransition DeploymentStatus DeploymentStatus
  | DeploymentNotFound DeploymentName
  deriving Show

possibleTransitions :: DeploymentStatus -> [DeploymentStatus]
possibleTransitions CreatePending = [Running, Failure]
possibleTransitions Running = [Failure, ArchivePending, UpdatePending]
possibleTransitions UpdatePending = [Failure, Running]
possibleTransitions Failure = [UpdatePending, Running, ArchivePending]
possibleTransitions ArchivePending = [ArchivePending, Archived]
possibleTransitions Archived = [CreatePending]

-- | Handles the 'archive' request.
archiveH :: DeploymentName -> AppM CommandResponse
archiveH dName = do
  failIfGracefulShutdownActivated
  t1 <- liftBase now
  st <- ask
  let
    log     = liftBase . logInfo (logger st)
    args    =
      [ "--project-name", coerce $ projectName st
      , "--base-domain", coerce $ baseDomain st
      , "--namespace", coerce $ namespace st
      , "--name", coerce dName
      ]
    cmd     = coerce $ archiveCommand st
  runBgWorker $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    t2 <- liftBase now
    let elTime = elapsedTime t2 t1
    transitionToStatusS dName $ TransitionArchivePending StatusTransitionProcessOutput
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
  let
    DeploymentUpdate
      { newTag = dTag
      , newAppOverrides = newAppOvs
      , oldAppOverrides = oldAppOvs
      , newDeploymentOverrides = newDepOvs
      , oldDeploymentOverrides = oldDepOvs
      } = dUpdate
    pgPool = pool st
    log  = logInfo (logger st)
  dId <- selectDeploymentId pgPool dName
  failIfImageNotFound dName dTag
  failIfGracefulShutdownActivated
  runBgWorker $ do
    (appOvs, depOvs) <- liftBase . withResource pgPool $ \conn ->
      withTransaction conn $ do
        deleteOldOverrides conn dId oldAppOvs oldDepOvs
        upsertNewOverrides conn dId newAppOvs newDepOvs
        selectOverrides conn dName
    liftBase $ updateDeploymentInfo dName st
    liftBase $ sendReloadEvent st
    let
      args =
        [ "--project-name", coerce $ projectName st
        , "--base-domain", coerce $ baseDomain st
        , "--namespace", coerce $ namespace st
        , "--name", coerce $ dName
        , "--tag", coerce $ dTag
        ] ++ applicationOverridesToArgs appOvs
          ++ deploymentOverridesToArgs depOvs
      cmd  = coerce $ updateCommand st
    liftBase . log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- liftBase $ runCommand (unpack cmd) (unpack <$> args)
    liftBase . log $ "deployment updated, name: "
      <> coerce dName <> ", tag: " <> coerce dTag
    t2 <- liftBase now
    let elTime = elapsedTime t2 t1
    transitionToStatusS dName $ TransitionUpdatePending StatusTransitionProcessOutput
      { exitCode = ec
      , duration = elTime
      , stdout = out
      , stderr = err
      }
    handleExitCode ec
  return Success

-- | Helper to get overrides from the database.
selectOverrides
  :: Connection
  -> DeploymentName
  -> IO (ApplicationOverrides, DeploymentOverrides)
selectOverrides conn dName = do
  let
    q                                         =
      "SELECT key, value, scope::text, visibility::text \
      \FROM deployment_overrides \
      \WHERE deployment_id = ( \
        \SELECT id FROM deployments WHERE name = ? \
      \)"
    parseVis :: Text -> OverrideVisibility
    parseVis                                   = read . unpack
    parseScope :: Text -> OverrideScope
    parseScope                                 = read . unpack
    toOverrides (k, v, s, vis) (appOvs, depOvs) =
      pure $ case parseScope s of
        ApplicationScope ->
          (ApplicationOverride (Override k v $ parseVis vis) : appOvs, depOvs)
        DeploymentScope ->
          (appOvs, DeploymentOverride (Override k v $ parseVis vis) : depOvs)
  rows <- query conn q (Only dName)
  foldrM toOverrides ([], []) rows

-- | Helper to get override logs from the database.
selectLogOverrides
  :: Connection
  -> ActionId
  -> IO (ApplicationOverrides, DeploymentOverrides)
selectLogOverrides conn aId = do
  let
    q                                         =
      "SELECT key, value, scope::text, visibility::text \
      \FROM deployment_log_overrides \
      \WHERE deployment_log_id = ?"
    parseVis :: Text -> OverrideVisibility
    parseVis                                   = read . unpack
    parseScope :: Text -> OverrideScope
    parseScope                                 = read . unpack
    toOverrides (k, v, s, vis) (appOvs, depOvs) =
      pure $ case parseScope s of
        ApplicationScope ->
          (ApplicationOverride (Override k v $ parseVis vis) : appOvs, depOvs)
        DeploymentScope ->
          (appOvs, DeploymentOverride (Override k v $ parseVis vis) : depOvs)
  rows <- query conn q (Only . unActionId $ aId)
  foldrM toOverrides ([], []) rows

-- | Helper to delete overrides from the database.
deleteOldOverrides
  :: Connection
  -> DeploymentId
  -> ApplicationOverrides
  -> DeploymentOverrides
  -> IO ()
deleteOldOverrides conn dId appOvs depOvs = do
  let
    q    =
      "DELETE FROM deployment_overrides \
      \WHERE deployment_id = ? AND key = ? AND scope = ?"
    dId' = unDeploymentId dId
  void $ for appOvs $ \o -> do
    let
      oKey   = overrideKey . unApplicationOverride $ o
      oScope = show ApplicationScope
    execute conn q (dId', oKey, oScope)
  void $ for depOvs $ \o -> do
    let
      oKey   = overrideKey . unDeploymentOverride $ o
      oScope = show DeploymentScope
    execute conn q (dId', oKey, oScope)

-- | Helper to get the deployment id from the database.
selectDeploymentId :: PgPool -> DeploymentName -> AppM DeploymentId
selectDeploymentId pgPool dName = do
  dIds :: [(Only Int)] <- liftIO $ withResource pgPool $ \conn ->
    query conn "SELECT id FROM deployments WHERE name = ?" (Only dName)
  case dIds of
    [(Only dId)] -> pure . DeploymentId $ dId
    [] -> throwError err404
      { errBody = validationError ["Name not found"] [] }
    _ -> throwError err406
      { errBody = validationError ["More than one name found"] [] }

-- | Helper to insert or update overrides.
upsertNewOverrides
  :: Connection
  -> DeploymentId
  -> ApplicationOverrides
  -> DeploymentOverrides
  -> IO ()
upsertNewOverrides conn dId appOvs depOvs = do
  let
    q   =
      "INSERT INTO deployment_overrides \
      \(key, value, deployment_id, scope, visibility) \
      \VALUES (?, ?, ?, ?, ?) \
      \ON CONFLICT (key, deployment_id, scope) \
      \DO \
        \UPDATE SET value = ?, visibility = ?, updated_at = now()"
    dId' = unDeploymentId dId
  void $ for appOvs $ \o -> do
    let
      oKey   = overrideKey . unApplicationOverride $ o
      oValue = overrideValue . unApplicationOverride $ o
      oScope = show ApplicationScope
      oVis   = show . overrideVisibility . unApplicationOverride $ o
    execute conn q (oKey, oValue, dId', oScope, oVis, oValue, oVis)
  void $ for depOvs $ \o -> do
    let
      oKey   = overrideKey . unDeploymentOverride $ o
      oValue = overrideValue . unDeploymentOverride $ o
      oScope = show DeploymentScope
      oVis   = show . overrideVisibility . unDeploymentOverride $ o
    execute conn q (oKey, oValue, dId', oScope, oVis, oValue, oVis)

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
  let
    dep'    =
      let (Deployment n t ao so) = dep
      in Deployment n t (hideP ao) (hideP so)
    dLogs'  = dLogs <&> \(DeploymentLog ai a t ao so ec d ct) ->
      DeploymentLog ai a t (hideP ao) (hideP so) ec d ct
    hideP o = coerce o <&> \(Override k v vis) ->
      let
        v' = case vis of
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
  _ :: [Only Int] <- liftIO $ withResource pgPool $ \conn ->
    query_ conn "SELECT 1"
  pure NoContent

-- | Handles the 'project_name' request.
projectNameH :: AppM ProjectName
projectNameH = projectName <$> ask

-- | Handles the 'status' request.
statusH :: DeploymentName -> AppM CurrentDeploymentStatus
statusH dName = do
  st <- ask
  let
    log    = logInfo (logger st)
    cmd    = checkingCommand st
    args   =
      [ "--namespace", coerce $ namespace st
      , "--name", coerce $ dName ]
  liftIO $ log $ "call " <> unwords (coerce cmd : args)
  ec <- liftIO $ runCommandWithoutPipes (unpack $ coerce cmd) (unpack <$> args)
  pure . CurrentDeploymentStatus $
    case ec of
      ExitSuccess -> Ok
      _ -> Error

-- | Handles the 'cleanup' request.
cleanupH :: DeploymentName -> AppM CommandResponse
cleanupH dName = do
  failIfGracefulShutdownActivated
  st <- ask
  runBgWorker $ liftBase $ cleanupDeployment dName st
  pure Success

-- | Helper to cleanup deployment.
cleanupDeployment :: DeploymentName -> AppState -> IO ()
cleanupDeployment dName st = do
  let
    log     = logInfo (logger st)
    pgPool  = pool st
    args    =
      [ "--project-name", coerce $ projectName st
      , "--base-domain", coerce $ baseDomain st
      , "--namespace", coerce $ namespace st
      , "--name", coerce dName
      ]
    cmd     = coerce $ cleanupCommand st
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
deleteDeploymentLogOverrides p n = withResource p $ \conn -> execute
  conn
  "DELETE FROM deployment_log_overrides WHERE deployment_log_id in ( \
    \SELECT id FROM deployment_logs WHERE deployment_id in ( \
      \SELECT id FROM deployments where name = ? \
    \) \
  \)"
  (Only n)

-- | Helper to delete deployment logs.
deleteDeploymentLogs :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogs p n = withResource p $ \conn -> execute
  conn
  "DELETE FROM deployment_logs WHERE deployment_id in\
  \  (SELECT id FROM deployments where name = ?)"
  (Only n)

-- | Helper to delete deployment overrides.
deleteDeploymentOverrides :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentOverrides p n = withResource p $ \conn -> execute
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
  let
    pgPool  = pool st
    archRetention = unArchiveRetention . archiveRetention $ st
    q =
      "SELECT name FROM deployments \
      \WHERE status in ? AND archived_at + interval '?' second < now()"
  retrieved :: [Only DeploymentName] <- liftIO $
    withResource pgPool $ \conn -> query conn q (In (show <$> archivedStatuses), archRetention)
  runBgWorker . void . liftBase $
    for retrieved $ \(Only dName) -> cleanupDeployment dName st

  pure Success

-- | Handles the 'restore' request.
restoreH :: DeploymentName -> AppM CommandResponse
restoreH dName = do
  failIfGracefulShutdownActivated
  t1 <- liftIO $ now
  st <- ask
  dep <- selectDeployment dName
  failIfImageNotFound (name dep) (tag dep)
  failIfGracefulShutdownActivated
  runBgWorker $ do
    liftBase $ updateDeploymentInfo dName st
    (ec, out, err) <- liftBase $ createDeployment dep st
    t2 <- liftBase now
    let elTime = elapsedTime t2 t1
    transitionToStatusS dName $ TransitionCreatePending StatusTransitionProcessOutput
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
  let
    pgPool = pool st
    aId' = Only . unActionId $ aId
    q = "SELECT stdout, stderr FROM deployment_logs WHERE id = ?"
  rows :: [(Text, Text)] <- liftIO $
    withResource pgPool $ \conn -> query conn q aId'
  case rows of
    (out, err) : _ -> pure $ ActionInfo out err
    _ ->
      throwError err400 { errBody = appError "Action not found" }

-- | Helper to run command with pipes.
runCommand :: MonadBase IO m => FilePath -> [String] -> m (ExitCode, Stdout, Stderr)
runCommand cmd args = do
  let proc' c a = setStdout createPipe . setStderr createPipe $ proc c a
  liftBase $ withProcessWait (proc' cmd args) $ \p -> do
    out <- hGetContents . getStdout $ p
    err <- hGetContents . getStderr $ p
    ec <- waitExitCode p
    pure (ec, Stdout out, Stderr err)

-- | Helper to run command without pipes.
runCommandWithoutPipes :: FilePath -> [String] -> IO ExitCode
runCommandWithoutPipes cmd args = do
  withProcessWait (proc cmd args) $ \p -> do
    ec <- waitExitCode p
    pure ec

-- | Helper to handle exit code.
handleExitCode :: MonadBaseControl IO m => ExitCode -> m ()
handleExitCode ExitSuccess = return ()
handleExitCode (ExitFailure c) = liftBase . throwIO $ DeploymentFailed c

-- | Helper to log a deployment action.
createDeploymentLog
  :: Connection
  -> Deployment
  -> Action
  -> ExitCode
  -> Duration
  -> Stdout
  -> Stderr
  -> IO ()
createDeploymentLog conn dep act ec dur out err = do
  let
    (Deployment dName dTag appOvs depOvs) = dep
    exitCode'                             =
      case ec of
        ExitSuccess -> 0
        ExitFailure errCode -> errCode
    dur'                                  = unDuration dur
    out'                                  = unStdout out
    err'                                  = unStderr err
    qInsertLog                            =
      "INSERT INTO deployment_logs \
      \(deployment_id, action, tag, exit_code, \
      \duration, stdout, stderr) \
      \(\
        \SELECT id, ?, ?, ?, ?, ?, ? \
        \FROM deployments \
        \WHERE name = ? \
      \) RETURNING id"
    qInsertLogOverride                   =
      "INSERT INTO deployment_log_overrides \
      \(key, value, deployment_log_id, scope, visibility) \
      \VALUES (?, ?, ?, ?, ?)"
  aIds :: [Only Int] <- query conn qInsertLog
    (act, dTag, exitCode', dur', out', err', dName)
  void $ for appOvs $ \o -> do
    let
      [Only aId]  = aIds
      oKey        = overrideKey . unApplicationOverride $ o
      oValue      = overrideValue . unApplicationOverride $ o
      oScope      = show ApplicationScope
      oVis        = show . overrideVisibility . unApplicationOverride $ o
    execute conn qInsertLogOverride (oKey, oValue, aId, oScope, oVis)
  void $ for depOvs $ \o -> do
    let
      [Only aId]  = aIds
      oKey        = overrideKey . unDeploymentOverride $ o
      oValue      = overrideValue . unDeploymentOverride $ o
      oScope      = show DeploymentScope
      oVis        = show . overrideVisibility . unDeploymentOverride $ o
    execute conn qInsertLogOverride (oKey, oValue, aId, oScope, oVis)

-- | Helper to get deployment metadata from the database.
selectDeploymentMetadata
  :: Connection
  -> DeploymentName
  -> IO [DeploymentMetadata]
selectDeploymentMetadata conn dName = do
  let
    q =
      "SELECT key, value FROM deployment_metadata \
      \WHERE deployment_id = (SELECT id FROM deployments WHERE name = ?) \
      \ORDER BY id ASC"
  rows <- query conn q (Only dName)
  for rows $ \(k, v) -> pure $ DeploymentMetadata k v

-- | Helper to delete deployment metadata.
deleteDeploymentMetadata
  :: PgPool
  -> DeploymentName
  -> IO ()
deleteDeploymentMetadata pgPool dName = do
  let
    q =
      "DELETE FROM deployment_metadata \
      \WHERE deployment_id = (SELECT id FROM deployments WHERE name = ?)"
  void $ withResource pgPool $ \conn -> execute conn q (Only dName)

-- | Helper to insert or update deployment metadata.
upsertDeploymentMetadata
  :: PgPool
  -> DeploymentName
  -> [DeploymentMetadata]
  -> IO ()
upsertDeploymentMetadata pgPool dName dMetadatas = do
  let
  withResource pgPool $ \conn -> withTransactionSerializable conn $ do
    void $ execute
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
failIfImageNotFound :: DeploymentName -> DeploymentTag -> AppM ()
failIfImageNotFound dName dTag = do
  st <- ask
  let
    log :: Text -> IO ()
    log  = logInfo (logger st)
    args =
      [ "--project-name", coerce $ projectName st
      , "--base-domain", coerce $ baseDomain st
      , "--namespace", coerce $ namespace st
      , "--name", coerce $ dName
      , "--tag", coerce $ dTag
      ]
    cmd  = coerce $ tagCheckingCommand st

  ec <- liftIO $ do
    log $ "call " <> unwords (cmd : args)
    runCommandWithoutPipes (unpack cmd) (unpack <$> args)
  case ec of
    ExitSuccess -> pure ()
    ExitFailure _ ->
      throwError err400 { errBody = validationError [] ["Tag not found"] }

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
  let
    pgPool             = pool state
    interval           = 30 :: Int
    selectDeps         =
      "SELECT name, status::text, \
      \extract(epoch from now())::int - \
        \extract(epoch from status_updated_at)::int \
      \FROM deployments \
      \WHERE checked_at < now() - interval '?' second AND status != 'Archived'"
    updateCheckedAt    =
      "UPDATE deployments SET checked_at = now() WHERE name = ?"
    logErr :: Text -> IO ()
    logErr  = logWarning (logger state)

  forever $ do
    rows :: [(DeploymentName, Text, Int)] <- liftIO $
      withResource pgPool $ \conn -> query conn selectDeps (Only interval)
    let
      checkList :: [(DeploymentName, DeploymentStatus, Timestamp)] =
        (\(n, s, t) -> (n, read . unpack $ s, coerce t)) <$> rows
    checkResult <- for checkList $ \(dName, dStatus, ts) -> do
      let
        args              =
          [ "--project-name", unpack . coerce $ projectName state
          , "--base-domain", unpack . coerce $ baseDomain state
          , "--namespace", unpack . coerce $ namespace state
          , "--name", unpack . coerce $ dName ]
        cmd ArchivePending = unpack . coerce $ archiveCheckingCommand state
        cmd _ = unpack . coerce $ checkingCommand state
        timeout = statusUpdateTimeout state
      ec <- runCommandWithoutPipes (cmd dStatus) args
      pure (dName, statusTransition ec dStatus ts timeout, ts)
    updated <-
      for checkResult $ \(dName, transitionM, _) ->
        withResource pgPool $ \conn ->
          case transitionM of
            Nothing -> execute conn updateCheckedAt (Only dName) $> False
            Just transition ->
              ($> True) $ (flip runReaderT state . runExceptT . runExceptT) (transitionToStatus dName transition) >>= \case
                Right (Right ()) -> void $ execute conn updateCheckedAt (Only dName)
                Left e -> logErr $ show' e
                Right (Left e) -> logErr $ show' e
    when (or updated) $ sendReloadEvent state
    threadDelay 5000000

-- | Returns the new deployment status.
statusTransition
  :: ExitCode
  -> DeploymentStatus
  -> Timestamp
  -> Timeout
  -> Maybe DeploymentStatusTransition
statusTransition ExitSuccess     ArchivePending _ _ = Just TransitionArchived
statusTransition ExitSuccess     Running        _ _ = Nothing
statusTransition ExitSuccess     _             _ _ = Just TransitionCreate
statusTransition (ExitFailure _) Running       _ _ = Just TransitionFailure
statusTransition (ExitFailure _) CreatePending ts timeout | ts > coerce timeout =
  Just TransitionFailure
statusTransition (ExitFailure _) UpdatePending ts timeout | ts > coerce timeout =
  Just TransitionFailure
statusTransition (ExitFailure _) _ _ _ = Nothing

-- | Checks if graceful shutdown has been activated activated.
-- Returns 405 'Graceful shutdown activated' response
-- if graceful shutdown has been activated.
failIfGracefulShutdownActivated :: AppM ()
failIfGracefulShutdownActivated = do
  gracefulShutdownAct <- gracefulShutdownActivated <$> ask
  gracefulShutdown <- liftIO . readIORef $ gracefulShutdownAct
  if gracefulShutdown
    then throwError err405 { errBody = appError "Graceful shutdown activated" }
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
