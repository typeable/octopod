module Octopod.Server (runOctopodServer) where


import           Chronos (Time, getTime, now)
import           Control.Applicative
import           Control.Concurrent (forkFinally, threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception (Exception, throwIO, try)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson (Value(..), encode, toJSON)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Coerce
import           Data.Conduit (ConduitT, yield)
import           Data.Foldable (foldrM)
import           Data.Functor ((<&>))
import           Data.Int (Int64)
import           Data.IORef
import           Data.Maybe
import           Data.Pool
import           Data.Text (lines, pack, unpack, unwords)
import           Data.Text.IO (hGetContents)
import           Data.Traversable
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Instances ()
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


import           Common.Utils
import           Common.Validation (isNameValid)
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

-- | Deployment search errors definition.
data DeploymentNotFound
  = DeploymentNotFound
  | ArchivedDeploymentNotFound
  | ActiveDeploymentNotFound
  | DeploymentHasPendingStatus DeploymentStatus

-- | Definition of a filter by pending statuses.
data FilterByPending
  = AllowPending
  | DenyPending

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
fullInfoH :: DeploymentName -> AppM DeploymentFullInfo
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
  dFullInfos <&> \(DeploymentFullInfo dep s a ct ut u) ->
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
    in DeploymentFullInfo (hidePrivate dep) s a ct ut u

-- | Helper to get full_info from the database.
getFullInfo :: FullInfoListType -> AppM [DeploymentFullInfo]
getFullInfo listType = do
  AppState {pool = p, logger = l} <- ask
  deployments <- liftIO $ withResource p $ \conn -> do
    rows <- case listType of
      FullInfoForAll -> query_ conn qAll
      FullInfoOnlyForOne (dName) -> query conn qOne (Only dName)
    for rows $ \(n, t, a, ct, ut, st) -> do
      (appOvs, depOvs) <- selectOverrides conn n
      dMeta <- selectDeploymentMetadata conn n
      pure $ do
        let dep = (Deployment n t appOvs depOvs)
        DeploymentFullInfo dep st a dMeta ct ut
  liftIO . logInfo l $ "get deployments: " <> (pack . show $ deployments)
  return deployments
  where
    qAll                 =
      "SELECT name, tag, archived, extract(epoch from created_at)::int, \
        \extract(epoch from updated_at)::int, status::text \
      \FROM deployments ORDER BY name"
    qOne                 =
      "SELECT name, tag, archived, extract(epoch from created_at)::int, \
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
        query conn q (n, t, CreatePending)
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
  liftIO . runBgWorker st $ do
    sendReloadEvent st
    updateDeploymentInfo (name dep) st
    (ec, out, err) <- createDeployment dep st
    t2 <- now
    let
      arch   = ArchivedFlag False
      elTime = elapsedTime t2 t1
    createDeploymentLog pgPool dep "create" ec arch elTime out err
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
  :: PgPool
  -> DeploymentName
  -> DeploymentListType
  -> FilterByPending
  -> AppM (Either DeploymentNotFound Deployment)
selectDeployment pgPool dName lType filterByPending = do
  let
    baseQuery                 =
      "SELECT name, tag, status::text FROM deployments \
      \WHERE name = ?"
    q AllDeployments = baseQuery
    q ArchivedOnlyDeployments = baseQuery <> " AND archived = 't'"
    q ActiveOnlyDeployments = baseQuery <> " AND archived = 'f'"
    allowPending              =
      case filterByPending of
        AllowPending -> True
        DenyPending -> False
  result <- liftIO . withResource pgPool $ \conn -> do
    retrieved <- query conn (q lType) (Only dName)
    for retrieved $ \(n, t, s) -> do
      (appOvs, stOvs) <- selectOverrides conn n
      pure $ (Deployment n t appOvs stOvs, read s)
  pure $
    case result of
      [(_, st)] | isPending st && not allowPending ->
        Left $ DeploymentHasPendingStatus st
      [(dep, _)] ->
        Right dep
      _ ->
        Left $
          case lType of
            AllDeployments -> DeploymentNotFound
            ArchivedOnlyDeployments -> ArchivedDeploymentNotFound
            ActiveOnlyDeployments -> ActiveDeploymentNotFound

-- | Handles deployment search result.
unwrapOrValidateError
  :: AppM (Either DeploymentNotFound Deployment)
  -> AppM Deployment
unwrapOrValidateError deploymentSearchResult = do
  deploymentSearchResult' <- deploymentSearchResult
  case deploymentSearchResult' of
    Right dep -> pure dep
    Left DeploymentNotFound ->
      throwError err404
        { errBody = validationError ["Deployment not found"] [] }
    Left ArchivedDeploymentNotFound ->
      throwError err404
        { errBody = validationError ["Archived deployment not found"] [] }
    Left ActiveDeploymentNotFound ->
      throwError err404
        { errBody = validationError ["Active deployment not found"] [] }
    Left (DeploymentHasPendingStatus st) ->
      let
        err =
          "You can not apply this operation \
          \on deployment with \"" <> (pack . show $ st) <> "\" status"
      in throwError err405 { errBody = validationError [err] [] }

-- | Handles the 'archive' request.
archiveH :: DeploymentName -> AppM CommandResponse
archiveH dName = do
  failIfGracefulShutdownActivated
  t1 <- liftIO $ now
  st <- ask
  let
    log     = logInfo (logger st)
    pgPool  = pool st
    args    =
      [ "--project-name", coerce $ projectName st
      , "--base-domain", coerce $ baseDomain st
      , "--namespace", coerce $ namespace st
      , "--name", coerce dName
      ]
    cmd     = coerce $ archiveCommand st
    arch    = ArchivedFlag True
  dep <- unwrapOrValidateError
    $ selectDeployment pgPool dName ActiveOnlyDeployments DenyPending
  liftIO . runBgWorker st $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    void $ archiveDeployment pgPool dName
    sendReloadEvent st
    t2 <- now
    let elTime = elapsedTime t2 t1
    void $ createDeploymentLog pgPool dep "archive" ec arch elTime out err
    log $ "deployment archived, name: " <> coerce dName
    sendReloadEvent st
    handleExitCode ec
  pure Success

-- | Handles the 'archive' request.
archiveDeployment :: PgPool -> DeploymentName -> IO Int64
archiveDeployment p dName = withResource p $ \conn -> do
  let
    q =
      "UPDATE deployments \
      \SET archived = 't', archived_at = now(), \
        \status = ?, status_updated_at = now() \
      \WHERE name = ?"
  execute conn q (ArchivePending, dName)

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

  void $ unwrapOrValidateError
    $ selectDeployment pgPool dName ActiveOnlyDeployments DenyPending
  dId <- selectDeploymentId pgPool dName
  failIfImageNotFound dName dTag
  failIfGracefulShutdownActivated
  liftIO . runBgWorker st $ do
    (appOvs, depOvs) <- withResource pgPool $ \conn ->
      withTransaction conn $ do
        deleteOldOverrides conn dId oldAppOvs oldDepOvs
        upsertNewOverrides conn dId newAppOvs newDepOvs
        void $ updateDeployment conn dName dTag
        selectOverrides conn dName
    updateDeploymentInfo dName st
    sendReloadEvent st
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
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    log $ "deployment updated, name: "
      <> coerce dName <> ", tag: " <> coerce dTag
    void $ do
      t2 <- now
      let
        dep    = Deployment dName dTag appOvs depOvs
        arch   = ArchivedFlag False
        elTime = elapsedTime t2 t1
      createDeploymentLog pgPool dep "update" ec arch elTime out err
    sendReloadEvent st
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

-- | Helper to update a deployment.
updateDeployment
  :: Connection
  -> DeploymentName
  -> DeploymentTag
  -> IO Int64
updateDeployment conn dName dTag = do
  let
    q =
      "UPDATE deployments \
      \SET tag = ?, updated_at = now(), \
        \status = ?, status_updated_at = now() \
      \WHERE name = ?"
  execute conn q (dTag, UpdatePending, dName)

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
  dep <- unwrapOrValidateError
    $ selectDeployment pgPool dName AllDeployments AllowPending
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
    pgPool = pool st
    log    = logInfo (logger st)
    cmd    = checkingCommand st
    args   =
      [ "--namespace", coerce $ namespace st
      , "--name", coerce $ dName ]
  void $ unwrapOrValidateError
    $ selectDeployment pgPool dName AllDeployments AllowPending
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
  void $ unwrapOrValidateError
    $ selectDeployment (pool st) dName ArchivedOnlyDeployments DenyPending
  liftIO . runBgWorker st $ cleanupDeployment dName st
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
      \WHERE archived = 't' AND archived_at + interval '?' second < now()"
  retrieved :: [Only DeploymentName] <- liftIO $
    withResource pgPool $ \conn -> query conn q (Only archRetention)
  liftIO . runBgWorker st . void $
    for retrieved $ \(Only dName) -> cleanupDeployment dName st

  pure Success

-- | Handles the 'restore' request.
restoreH :: DeploymentName -> AppM CommandResponse
restoreH dName = do
  failIfGracefulShutdownActivated
  t1 <- liftIO $ now
  st <- ask
  let pgPool  = pool st
  dep <- unwrapOrValidateError
    $ selectDeployment pgPool dName ArchivedOnlyDeployments DenyPending
  failIfImageNotFound (name dep) (tag dep)
  failIfGracefulShutdownActivated
  liftIO . runBgWorker st $ do
    updateDeploymentInfo dName st
    let
      q =
        "UPDATE deployments \
        \SET archived = 'f', archived_at = null, status = 'CreatePending' \
        \WHERE name = ?"
    void $ withResource pgPool $ \conn -> execute conn q (Only dName)
    sendReloadEvent st
    (ec, out, err) <- createDeployment dep st
    t2 <- now
    let
      arch   = ArchivedFlag False
      elTime = elapsedTime t2 t1
    createDeploymentLog pgPool dep "restore" ec arch elTime out err
    sendReloadEvent st
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
runCommand :: FilePath -> [String] -> IO (ExitCode, Stdout, Stderr)
runCommand cmd args = do
  let proc' c a = setStdout createPipe . setStderr createPipe $ proc c a
  withProcessWait (proc' cmd args) $ \p -> do
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
handleExitCode :: ExitCode -> IO ()
handleExitCode ExitSuccess = return ()
handleExitCode (ExitFailure c) = throwIO $ DeploymentFailed c

-- | Helper to log a deployment action.
createDeploymentLog
  :: PgPool
  -> Deployment
  -> Action
  -> ExitCode
  -> ArchivedFlag
  -> Duration
  -> Stdout
  -> Stderr
  -> IO ()
createDeploymentLog pgPool dep act ec arch dur out err = do
  let
    (Deployment dName dTag appOvs depOvs) = dep
    exitCode'                             =
      case ec of
        ExitSuccess -> 0
        ExitFailure errCode -> errCode
    arch'                                 = unArchivedFlag arch
    dur'                                  = unDuration dur
    out'                                  = unStdout out
    err'                                  = unStderr err
    qInsertLog                            =
      "INSERT INTO deployment_logs \
      \(deployment_id, action, tag, exit_code, archived, \
      \duration, stdout, stderr) \
      \(\
        \SELECT id, ?, ?, ?, ?, ?, ?, ? \
        \FROM deployments \
        \WHERE name = ? \
      \) RETURNING id"
    qInsertLogOverride                   =
      "INSERT INTO deployment_log_overrides \
      \(key, value, deployment_log_id, scope, visibility) \
      \VALUES (?, ?, ?, ?, ?)"

  void $ withResource pgPool $ \conn ->
    withTransaction conn $ do
      aIds :: [Only Int] <- query conn qInsertLog
        (act, dTag, exitCode', arch', dur', out', err', dName)
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
    updateStatus       =
      "UPDATE deployments \
      \SET status = ?, status_updated_at = now(), checked_at = now() \
      \WHERE name = ? and status = ?"
    updateCheckedAt    =
      "UPDATE deployments SET checked_at = now() WHERE name = ? and status = ?"

  forever $ do
    rows :: [(DeploymentName, DeploymentStatus, Int)] <- liftIO $
      withResource pgPool $ \conn -> query conn selectDeps (Only interval)
    let
      checkList :: [(DeploymentName, DeploymentStatus, Timestamp)] =
        (\(n, s, t) -> (n, s, coerce t)) <$> rows
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
      pure (dName, newStatus ec dStatus ts timeout, ts)
    void $
      for (zip checkList checkResult) $ \((dName, oldSt, _), (_, newSt, _)) ->
        withResource pgPool $ \conn ->
          if oldSt == newSt
            then execute conn updateCheckedAt (dName, oldSt)
            else execute conn updateStatus (newSt, dName, oldSt)
    if checkList == checkResult
      then pure ()
      else sendReloadEvent state
    threadDelay 5000000

-- | Returns the new deployment status.
newStatus
  :: ExitCode
  -> DeploymentStatus
  -> Timestamp
  -> Timeout
  -> DeploymentStatus
newStatus ExitSuccess     ArchivePending _ _ = Archived
newStatus ExitSuccess     _             _ _ = Running
newStatus (ExitFailure n) Running       _ _ = Failure $ failureStatusType n
newStatus (ExitFailure n) CreatePending ts timeout | ts > coerce timeout =
  Failure $ failureStatusType n
newStatus (ExitFailure n) UpdatePending ts timeout | ts > coerce timeout =
  Failure $ failureStatusType n
newStatus (ExitFailure _) ArchivePending _ _ = ArchivePending
newStatus (ExitFailure _) oldStatus     _ _ = oldStatus

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
runBgWorker :: AppState -> IO () -> IO ()
runBgWorker state act = void $ forkFinally act' cleanup
  where
    act' =
      atomicModifyIORef' (bgWorkersCounter state) (\c -> (c + 1, c)) >> act
    cleanup _ = do
      c <- atomicModifyIORef' (bgWorkersCounter state) (\c -> (c - 1, c - 1))
      gracefulShutdown <- readIORef (gracefulShutdownActivated state)
      if gracefulShutdown && c == 0
        then putMVar (shutdownSem state) ()
        else pure ()
