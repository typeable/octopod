module DMS (runDMS) where


import           Chronos (Time, getTime, now)
import           Control.Applicative
import           Control.Concurrent (forkFinally, threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception (throwIO, try, Exception)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Aeson (Value (..), encode, toJSON)
import           Data.Coerce
import           Data.Conduit (ConduitT, yield)
import           Data.Foldable (foldrM)
import           Data.Functor ((<&>))
import           Data.Int (Int64)
import           Data.IORef
import           Data.Maybe
import           Data.Pool
import           Data.Text (pack, unpack, unwords)
import           Data.Text.IO (hGetContents)
import           Data.Traversable
import           Database.PostgreSQL.Simple
import           Network.Wai.Handler.Warp
import           Network.Wai.Handler.WarpTLS
import           Options.Generic
import           PostgreSQL.ErrorCodes (unique_violation)
import           Prelude hiding (lines, unlines, unwords, log)
import           Servant
import           System.Environment (lookupEnv)
import           System.Exit
import           System.Log.FastLogger
import           System.Posix.Signals (sigTERM)
import           System.Process.Typed


import           API
import           Common.API
import           Common.Validation (isNameValid)
import           DMS.Args
import           DMS.AWS
import           DMS.Logger
import           DMS.Unix
import           Orphans ()
import           TLS (createTLSOpts)
import           Types


type PgPool = Pool Connection
type AppM   = ReaderT AppState Handler

data AppState = AppState
  { pool                          :: PgPool
  , logger                        :: TimedFastLogger
  , eventSink                     :: TChan WSEvent
  , bgWorkersCounter              :: IORef Int
  , gracefulShudownActivated      :: IORef Bool
  , shutdownSem                   :: MVar ()
  , projectName                   :: ProjectName
  , baseDomain                    :: Domain
  , namespace                     :: Namespace
  , archiveRetention              :: ArchiveRetention
  , creationCommand               :: Command
  , updateCommand                 :: Command
  , deletionCommand               :: Command
  , checkingCommand               :: Command
  , cleanupCommand                :: Command
  , archiveCheckingCommand        :: Command }

data DeploymentException
  = DeploymentFailed Int
  | ThereIsAcriveDeployment
  deriving (Show)

instance Exception DeploymentException

data DeploymentListType
  = AllDeployments
  | ArchivedOnlyDeployments
  deriving (Show)

data FullInfoListType
  = FullInfoForAll
  | FullInfoOnlyForOne DeploymentName
  deriving (Show)

runDMS :: IO ()
runDMS = do
  logger' <- newLogger
  logInfo logger' "started"
  bgWorkersC <- newIORef 0
  gracefulShudownAct <- newIORef False
  shutdownS <- newEmptyMVar
  void $ do
    let
      termHandler = terminationHandler bgWorkersC gracefulShudownAct shutdownS
    installShutdownHandler logger' [sigTERM] termHandler
  opts <- parseArgs
  let
    a ?! e = a >>= maybe (die e) pure
    getEnvOrDie eName = lookupEnv eName ?! (eName <> " is not set")
  projName <- coerce . pack <$> getEnvOrDie "PROJECT_NAME"
  domain <- coerce . pack <$> getEnvOrDie "BASE_DOMAIN"
  ns <- coerce . pack <$> getEnvOrDie "NAMESPACE"
  archRetention <- coerce . read @Int <$> getEnvOrDie "ARCHIVE_RETENTION"
  creationCmd <- coerce . pack <$> getEnvOrDie "CREATION_COMMAND"
  updateCmd <- coerce . pack <$> getEnvOrDie "UPDATE_COMMAND"
  deletionCmd <- coerce . pack <$> getEnvOrDie "DELETION_COMMAND"
  checkingCmd <- coerce . pack <$> getEnvOrDie "CHECKING_COMMAND"
  cleanupCmd <- coerce . pack <$> getEnvOrDie "CLEANUP_COMMAND"
  archiveCheckingCmd <- coerce . pack <$> getEnvOrDie "ARCHIVE_CHECKING_COMMAND"
  pgPool <- initConnectionPool
    (unDBConnectionString $ dmsDB opts) (unDBPoolSize $ dmsDBPoolSize opts)
  channel <- liftIO . atomically $ newBroadcastTChan
  let
    appSt        =
      AppState
        pgPool
        logger'
        channel
        bgWorkersC
        gracefulShudownAct
        shutdownS
        projName
        domain
        ns
        archRetention
        creationCmd
        updateCmd
        deletionCmd
        checkingCmd
        cleanupCmd
        archiveCheckingCmd
    app'         = app appSt
    powerApp'    = powerApp appSt
    wsApp'       = wsApp channel
    serverPort   = dmsPort opts
    uiServerPort = unServerPort $ dmsUIPort opts
    wsServerPort = unServerPort $ dmsWSPort opts
    tlsOpts      =
      createTLSOpts (dmsTLSCertPath opts) (dmsTLSKeyPath opts)
        (dmsTLSStorePath opts) serverPort
    warpOpts     = setPort (unServerPort serverPort) defaultSettings
    in
      (run uiServerPort app')
      `race_` (runTLS tlsOpts warpOpts powerApp')
      `race_` (run wsServerPort wsApp')
      `race_` (runStatusUpdater appSt)
      `race_` (runShutdownHandler appSt)

initConnectionPool :: ByteString -> Int -> IO PgPool
initConnectionPool dbConnStr =
  createPool (connectPostgreSQL dbConnStr) close 1 30

nt :: AppState -> AppM a -> Handler a
nt s x = runReaderT x s

app :: AppState -> Application
app s = serve api $ hoistServer api (nt s) server
  where
    api = Proxy @API

server :: ServerT API AppM
server =
  ( listH :<|> createH :<|> deleteH :<|> updateH
    :<|> infoH :<|> fullInfoH :<|> statusH :<|> cleanupH :<|> restoreH
  ) :<|> getActionInfoH :<|> pingH :<|> cleanArchiveH :<|> projectNameH

powerApp :: AppState -> Application
powerApp s = serve api $ hoistServer api (nt s) powerServer
  where
    api = Proxy @PowerAPI

powerServer :: ServerT PowerAPI AppM
powerServer =
  ( powerListH :<|> createH :<|> deleteH :<|> updateH
    :<|> powerInfoH :<|> powerFullInfoH :<|> statusH :<|> cleanupH :<|> restoreH
  ) :<|> getActionInfoH :<|> pingH :<|> cleanArchiveH :<|> projectNameH

wsApp :: TChan WSEvent -> Application
wsApp channel = serve api $ wsServer channel
  where
    api = Proxy @WebSocketAPI

wsServer :: TChan WSEvent -> Server WebSocketAPI
wsServer = eventS

eventS :: MonadIO m => TChan WSEvent -> ConduitT () Value m ()
eventS channel = do
  dupChannel <- liftIO . atomically $ dupTChan channel
  forever $ do
    event <- liftIO . atomically . readTChan $ dupChannel
    yield . toJSON $ event

listH :: AppM [DeploymentFullInfo]
listH = hideSecretsInFullInfos <$> getFullInfo FullInfoForAll

powerListH :: AppM [DeploymentFullInfo]
powerListH = getFullInfo FullInfoForAll

fullInfoH :: DeploymentName -> AppM DeploymentFullInfo
fullInfoH dName = do
  fullInfoList <- hideSecretsInFullInfos <$>
    getFullInfo (FullInfoOnlyForOne dName)
  case fullInfoList of
    fullInfo : _ -> pure fullInfo
    []           -> throwError err404
      { errBody = validationError ["Name not found"] [] }

powerFullInfoH :: DeploymentName -> AppM DeploymentFullInfo
powerFullInfoH dName = do
  fullInfoList <- getFullInfo $ FullInfoOnlyForOne dName
  case fullInfoList of
    fullInfo : _ -> pure fullInfo
    []           -> throwError err404
      { errBody = validationError ["Name not found"] [] }

hideSecretsInFullInfos :: [DeploymentFullInfo] -> [DeploymentFullInfo]
hideSecretsInFullInfos dFullInfos =  do
  dFullInfos <&> \(DeploymentFullInfo dep s a ct ut u) ->
    let
      hideSecrets (Deployment n t appOvs stOvs) =
        Deployment n t (hideS appOvs) (hideS stOvs)
      hideS o                                   =
        coerce o <&> \(Override k v vis) ->
          let
            v' = case vis of
              Private -> "*"
              Public  -> v
          in coerce $ Override k v' vis
    in DeploymentFullInfo (hideSecrets dep) s a ct ut u

getFullInfo :: FullInfoListType -> AppM [DeploymentFullInfo]
getFullInfo listType = do
  AppState {pool = p, logger = l, baseDomain = d} <- ask
  deployments <- liftIO $ withResource p $ \conn -> do
    rows <- case listType of
      FullInfoForAll             -> query_ conn qAll
      FullInfoOnlyForOne (dName) -> query conn qOne (Only dName)
    for rows $ \(n, t, a, ct, ut, st) -> do
      (appOvs, stOvs) <- selectOverrides conn n
      pure $ do
        let dep = (Deployment n t appOvs stOvs)
        DeploymentFullInfo dep (read st) a ct ut (depUrls d n)
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
    -- FIXME: move to b2b-utils
    depUrls domain dName =
      [ ("app", appUrl domain dName)
      , ("kibana", "kibana." <> appUrl domain dName)
      , ("tasker", "tasker." <> appUrl domain dName)]
    appUrl domain dName  = coerce dName <> "." <> coerce domain

createH :: Deployment -> AppM CommandResponse
createH dep = do
  failIfGracefulShudownActivated
  unless (isNameValid $ name dep) $ do
    let
      badNameText = "Staging name length should be longer than 2 characters \
      \and under 17 characters and begin with a letter."
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
  failIfImageNotFound $ tag dep
  res :: Either SqlError [Only Int] <- liftIO . try $ createDep pgPool dep
  dId <- case res of
    Right ((Only depId) : _)                                ->
      pure . DeploymentId $ depId
    Right []                                                ->
      throwError err404
        { errBody = validationError ["Name not found"] [] }
    Left (SqlError code _ _ _ _) | code == unique_violation ->
      throwError err400
        { errBody = validationError ["Deployment already exists"] [] }
    Left (SqlError _ _ _ _ _)                               ->
      throwError err409 { errBody = appError "Some database error" }
  liftIO . withResource pgPool $ \conn ->
    upsertNewOverrides conn dId (appOverrides dep) (stagingOverrides dep)
  liftIO . runBgWorker st $ do
    sendReloadEvent st
    (ec, out, err) <- createDeployment dep st
    t2 <- now
    let
      arch   = ArchivedFlag False
      elTime = elapsedTime t2 t1
    createDeploymentLog pgPool dep "create" ec arch elTime out err
    sendReloadEvent st
    handleExitCode ec
  pure Success

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
        ++ stagingOverridesToArgs (stagingOverrides dep)
    cmd  = coerce $ creationCommand st

  liftIO $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    log $ "deployment created, deployment: " <> (pack . show $ dep)
    pure (ec, out, err)

applicationOverrideToArg :: ApplicationOverride -> [Text]
applicationOverrideToArg o = ["--app-env-override", overrideToArg . coerce $ o]

applicationOverridesToArgs :: ApplicationOverrides -> [Text]
applicationOverridesToArgs ovs = concat [applicationOverrideToArg o | o <- ovs ]

stagingOverrideToArg :: StagingOverride -> [Text]
stagingOverrideToArg o = ["--staging-override", overrideToArg . coerce $ o]

stagingOverridesToArgs :: StagingOverrides -> [Text]
stagingOverridesToArgs ovs = concat [stagingOverrideToArg o | o <- ovs]

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
      (appOvs, stOvs) <- selectLogOverrides conn (ActionId ai)
      pure $ DeploymentLog (ActionId ai) a t appOvs stOvs ec (Duration d) ts

selectDeployment
  :: PgPool
  -> DeploymentName
  -> DeploymentListType
  -> AppM Deployment
selectDeployment pgPool dName lType = do
  let
    q AllDeployments          =
      "SELECT name, tag FROM deployments \
      \WHERE name = ?"
    q ArchivedOnlyDeployments =
      "SELECT name, tag FROM deployments \
      \WHERE name = ? AND archived = 't'"

  deps <- liftIO . withResource pgPool $ \conn -> do
    retrieved <- query conn (q lType) (Only dName)
    for retrieved $ \(n, t) -> do
      (appOvs, stOvs) <- selectOverrides conn n
      pure $ Deployment n t appOvs stOvs
  case deps of
    [dep] -> pure dep
    []    -> throwError err404
      { errBody = validationError ["Name not found"] [] }
    _     -> throwError err406
      { errBody = validationError ["More than one name found"] [] }

deleteH :: DeploymentName -> AppM CommandResponse
deleteH dName = do
  failIfGracefulShudownActivated
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
    cmd     = coerce $ deletionCommand st
    arch    = ArchivedFlag True
  dep <- selectDeployment pgPool dName AllDeployments
  liftIO . runBgWorker st $ do
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    void $ archiveDeployment pgPool dName
    sendReloadEvent st
    t2 <- now
    let elTime = elapsedTime t2 t1
    void $ createDeploymentLog pgPool dep "delete" ec arch elTime out err
    log $ "deployment deleted, name: " <> coerce dName
    sendReloadEvent st
    handleExitCode ec
  pure Success

archiveDeployment :: PgPool -> DeploymentName -> IO Int64
archiveDeployment p dName = withResource p $ \conn -> do
  let
    q =
      "UPDATE deployments \
      \SET archived = 't', archived_at = now(), \
        \status = ?, status_updated_at = now() \
      \WHERE name = ?"
  execute conn q (show DeletePending, dName)

updateH :: DeploymentName -> DeploymentUpdate -> AppM CommandResponse
updateH dName dUpdate = do
  failIfGracefulShudownActivated
  t1 <- liftIO $ now
  st <- ask
  let
    DeploymentUpdate
      { newTag = dTag
      , newAppOverrides = newAppOvs
      , oldAppOverrides = oldAppOvs
      , newStagingOverrides = newStOvs
      , oldStagingOverrides = oldStOvs } = dUpdate
    pgPool = pool st
    log  = logInfo (logger st)

  dId <- selectDeploymentId pgPool dName
  failIfImageNotFound dTag
  liftIO . runBgWorker st $ do
    (appOvs, stOvs) <- withResource pgPool $ \conn ->
      withTransaction conn $ do
        deleteOldOverrides conn dId oldAppOvs oldStOvs
        upsertNewOverrides conn dId newAppOvs newStOvs
        void $ updateDeployment conn dName dTag
        selectOverrides conn dName
    sendReloadEvent st
    let
      args =
        [ "--project-name", coerce $ projectName st
        , "--base-domain", coerce $ baseDomain st
        , "--namespace", coerce $ namespace st
        , "--name", coerce $ dName
        , "--tag", coerce $ dTag
        ] ++ applicationOverridesToArgs appOvs
          ++ stagingOverridesToArgs stOvs
      cmd  = coerce $ updateCommand st
    log $ "call " <> unwords (cmd : args)
    (ec, out, err) <- runCommand (unpack cmd) (unpack <$> args)
    log $ "deployment updated, name: "
      <> coerce dName <> ", tag: " <> coerce dTag
    void $ do
      t2 <- now
      let
        dep    = Deployment dName dTag appOvs stOvs
        arch   = ArchivedFlag False
        elTime = elapsedTime t2 t1
      createDeploymentLog pgPool dep "update" ec arch elTime out err
    sendReloadEvent st
    handleExitCode ec
  return Success

selectOverrides
  :: Connection
  -> DeploymentName
  -> IO (ApplicationOverrides, StagingOverrides)
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
    toOverrides (k, v, s, vis) (appOvs, stOvs) =
      pure $ case parseScope s of
        App     ->
          (ApplicationOverride (Override k v $ parseVis vis) : appOvs, stOvs)
        Staging ->
          (appOvs, StagingOverride (Override k v $ parseVis vis) : stOvs)
  rows <- query conn q (Only dName)
  foldrM toOverrides ([], []) rows

selectLogOverrides
  :: Connection
  -> ActionId
  -> IO (ApplicationOverrides, StagingOverrides)
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
    toOverrides (k, v, s, vis) (appOvs, stOvs) =
      pure $ case parseScope s of
        App     ->
          (ApplicationOverride (Override k v $ parseVis vis) : appOvs, stOvs)
        Staging ->
          (appOvs, StagingOverride (Override k v $ parseVis vis) : stOvs)
  rows <- query conn q (Only . unActionId $ aId)
  foldrM toOverrides ([], []) rows

deleteOldOverrides
  :: Connection
  -> DeploymentId
  -> ApplicationOverrides
  -> StagingOverrides
  -> IO ()
deleteOldOverrides conn dId appOvs stOvs = do
  let
    q    =
      "DELETE FROM deployment_overrides \
      \WHERE deployment_id = ? AND key = ? AND scope = ?"
    dId' = unDeploymentId dId
  void $ for appOvs $ \o -> do
    let
      oKey   = overrideKey . unApplicationOverride $ o
      oScope = show App
    execute conn q (dId', oKey, oScope)
  void $ for stOvs $ \o -> do
    let
      oKey   = overrideKey . unStagingOverride $ o
      oScope = show Staging
    execute conn q (dId', oKey, oScope)

selectDeploymentId :: PgPool -> DeploymentName -> AppM DeploymentId
selectDeploymentId pgPool dName = do
  dIds :: [(Only Int)] <- liftIO $ withResource pgPool $ \conn ->
    query conn "SELECT id FROM deployments WHERE name = ?" (Only dName)
  case dIds of
    [(Only dId)] -> pure . DeploymentId $ dId
    []    -> throwError err404
      { errBody = validationError ["Name not found"] [] }
    _     -> throwError err406
      { errBody = validationError ["More than one name found"] [] }

upsertNewOverrides
  :: Connection
  -> DeploymentId
  -> ApplicationOverrides
  -> StagingOverrides
  -> IO ()
upsertNewOverrides conn dId appOvs stOvs  = do
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
      oScope = show App
      oVis   = show . overrideVisibility . unApplicationOverride $ o
    execute conn q (oKey, oValue, dId', oScope, oVis, oValue, oVis)
  void $ for stOvs $ \o -> do
    let
      oKey   = overrideKey . unStagingOverride $ o
      oValue = overrideValue . unStagingOverride $ o
      oScope = show Staging
      oVis   = show . overrideVisibility . unStagingOverride $ o
    execute conn q (oKey, oValue, dId', oScope, oVis, oValue, oVis)

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
  execute conn q (dTag, show UpdatePending, dName)

infoH :: DeploymentName -> AppM [DeploymentInfo]
infoH dName = do
  st <- ask
  dInfo <- getInfo dName
  liftIO . logInfo (logger st) $
    "get deployment info: " <> (pack . show $ dInfo)
  pure [hideSecretsInInfo dInfo]

powerInfoH :: DeploymentName -> AppM [DeploymentInfo]
powerInfoH dName = do
  st <- ask
  dInfo <- getInfo dName
  liftIO . logInfo (logger st) $
    "get deployment info: " <> (pack . show $ dInfo)
  pure [dInfo]

hideSecretsInInfo :: DeploymentInfo -> DeploymentInfo
hideSecretsInInfo (DeploymentInfo dep dLogs) =
  let
    dep'    =
      let (Deployment n t ao so) = dep
      in Deployment n t (hideS ao) (hideS so)
    dLogs'  = dLogs <&> \(DeploymentLog ai a t ao so ec d ct) ->
      DeploymentLog ai a t (hideS ao) (hideS so) ec d ct
    hideS o = coerce o <&> \(Override k v vis) ->
      let
        v' = case vis of
          Private -> "*"
          Public  -> v
      in coerce $ Override k v' vis
  in DeploymentInfo dep' dLogs'

getInfo :: DeploymentName -> AppM DeploymentInfo
getInfo dName = do
  st <- ask
  let pgPool = pool st
  dep <- selectDeployment pgPool dName AllDeployments
  dId <- selectDeploymentId pgPool dName
  liftIO $ do
    depLogs <- selectDeploymentLogs pgPool dId
    let depInfo = DeploymentInfo dep $ reverse depLogs
    pure depInfo

pingH :: AppM NoContent
pingH = do
  pgPool <- pool <$> ask
  _ :: [Only Int] <- liftIO $ withResource pgPool $ \conn ->
    query_ conn "SELECT 1"
  pure NoContent

projectNameH :: AppM ProjectName
projectNameH = projectName <$> ask

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
  void $ selectDeployment pgPool dName AllDeployments
  liftIO $ log $ "call " <> unwords (coerce cmd : args)
  ec <- liftIO $ runCommandWithoutPipes (unpack $ coerce cmd) (unpack <$> args)
  pure . CurrentDeploymentStatus $
    case ec of
      ExitSuccess -> Ok
      _           -> Error

cleanupH :: DeploymentName -> AppM CommandResponse
cleanupH dName = do
  failIfGracefulShudownActivated
  st <- ask
  liftIO . runBgWorker st $ cleanupDeployment dName st
  pure Success

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
  void $ deleteDeployment pgPool dName
  log $ "deployment destroyed, name: " <> coerce dName
  sendReloadEvent st
  handleExitCode ec

deleteDeploymentLogOverrides :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogOverrides p n = withResource p $ \conn -> execute
  conn
  "DELETE FROM deployment_log_overrides WHERE deployment_log_id in ( \
    \SELECT id FROM deployment_logs WHERE deployment_id in ( \
      \SELECT id FROM deployments where name = ? \
    \) \
  \)"
  (Only n)

deleteDeploymentLogs :: PgPool -> DeploymentName -> IO Int64
deleteDeploymentLogs p n = withResource p $ \conn -> execute
  conn
  "DELETE FROM deployment_logs WHERE deployment_id in\
  \  (SELECT id FROM deployments where name = ?)"
  (Only n)

deleteDeployment :: PgPool -> DeploymentName -> IO Int64
deleteDeployment p n = withResource p $ \conn ->
  execute conn "DELETE FROM deployments WHERE name = ?" (Only n)

cleanArchiveH :: AppM CommandResponse
cleanArchiveH = do
  failIfGracefulShudownActivated
  st <- ask
  let
    pgPool  = pool st
    archRetention = unArchiveRetention . archiveRetention $ st
    q =
      "SELECT name FROM deployments \
      \WHERE archived = 't' AND archived_at + interval '?' second < now()"
  retrieved :: [Only DeploymentName] <- liftIO $
    withResource pgPool $ \conn -> query conn q (Only archRetention)
  void . liftIO $ for retrieved $ \(Only dName) ->
    runBgWorker st $ cleanupDeployment dName st
  pure Success

restoreH :: DeploymentName -> AppM CommandResponse
restoreH dName = do
  failIfGracefulShudownActivated
  t1 <- liftIO $ now
  st <- ask
  let pgPool  = pool st
  dep <- selectDeployment pgPool dName ArchivedOnlyDeployments
  failIfImageNotFound $ tag dep
  liftIO . runBgWorker st $ do
    (ec, out, err) <- createDeployment dep st
    let
      q =
        "UPDATE deployments \
        \SET archived = 'f', archived_at = null, status = 'CreatePending' \
        \WHERE name = ?"
    void $ withResource pgPool $ \conn -> execute conn q (Only dName)
    sendReloadEvent st
    t2 <- now
    let
      arch   = ArchivedFlag False
      elTime = elapsedTime t2 t1
    createDeploymentLog pgPool dep "restore" ec arch elTime out err
    sendReloadEvent st
    handleExitCode ec
  pure Success

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
    _              ->
      throwError err400 { errBody = appError "Action not found" }

runCommand :: FilePath -> [String] -> IO (ExitCode, Stdout, Stderr)
runCommand cmd args = do
  let proc' c a = setStdout createPipe . setStderr createPipe $ proc c a
  withProcessWait (proc' cmd args) $ \p -> do
    out <- hGetContents . getStdout $ p
    err <- hGetContents . getStderr $ p
    ec <- waitExitCode p
    pure (ec, Stdout out, Stderr err)

runCommandWithoutPipes :: FilePath -> [String] -> IO ExitCode
runCommandWithoutPipes cmd args = do
  withProcessWait (proc cmd args) $ \p -> do
    ec <- waitExitCode p
    pure ec

handleExitCode :: ExitCode -> IO ()
handleExitCode ExitSuccess     = return ()
handleExitCode (ExitFailure c) = throwIO $ DeploymentFailed c

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
    (Deployment dName dTag appOvs stOvs) = dep
    exitCode'                            =
      case ec of
        ExitSuccess         -> 0
        ExitFailure errCode -> errCode
    arch'                                = unArchivedFlag arch
    dur'                                 = unDuration dur
    out'                                 = unStdout out
    err'                                 = unStderr err
    qInsertLog                           =
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
          oScope      = show App
          oVis        = show . overrideVisibility . unApplicationOverride $ o
        execute conn qInsertLogOverride (oKey, oValue, aId, oScope, oVis)
      void $ for stOvs $ \o -> do
        let
          [Only aId]  = aIds
          oKey        = overrideKey . unStagingOverride $ o
          oValue      = overrideValue . unStagingOverride $ o
          oScope      = show Staging
          oVis        = show . overrideVisibility . unStagingOverride $ o
        execute conn qInsertLogOverride (oKey, oValue, aId, oScope, oVis)

failIfImageNotFound :: DeploymentTag -> AppM ()
failIfImageNotFound dTag = do
  foundTag <- liftIO . findImageTag $ dTag
  case foundTag of
    Just _  -> pure ()
    Nothing ->
      throwError err400 { errBody = validationError [] ["Tag not found"] }

appError :: Text -> BSL.ByteString
appError = encode . AppError

validationError :: [Text] -> [Text] -> BSL.ByteString
validationError nameErrors tagErrors =
  encode $ ValidationError nameErrors tagErrors

sendReloadEvent :: AppState -> IO ()
sendReloadEvent state =
  atomically $ writeTChan (eventSink state) FrontendPleaseUpdateEverything

elapsedTime :: Time -> Time -> Duration
elapsedTime t1 t2 =
  Duration . fromIntegral . (`div` 1000000) . abs $ getTime t2 - getTime t1

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
      \WHERE name = ?"
    updateCheckedAt    =
      "UPDATE deployments SET checked_at = now() WHERE name = ?"

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
        cmd DeletePending = unpack . coerce $ archiveCheckingCommand state
        cmd _             = unpack . coerce $ checkingCommand state
      ec <- runCommandWithoutPipes (cmd dStatus) args
      pure (dName, newStatus ec dStatus ts, ts)
    void $
      for (zip checkList checkResult) $ \((dName, oldSt, _), (_, newSt, _)) ->
        withResource pgPool $ \conn ->
          if oldSt == newSt
            then execute conn updateCheckedAt (Only dName)
            else execute conn updateStatus (show newSt, dName)
    if checkList == checkResult
      then pure ()
      else sendReloadEvent state
    threadDelay 5000000

updatePeriod :: Timestamp
updatePeriod = Timestamp 600

newStatus :: ExitCode -> DeploymentStatus -> Timestamp -> DeploymentStatus
newStatus ExitSuccess     DeletePending _                      = Archived
newStatus ExitSuccess     _             _                      = Running
newStatus (ExitFailure _) Running       _                      = Failure
newStatus (ExitFailure _) CreatePending ts | ts > updatePeriod = Failure
newStatus (ExitFailure _) UpdatePending ts | ts > updatePeriod = Failure
newStatus (ExitFailure _) DeletePending _                      = DeletePending
newStatus (ExitFailure _) oldStatus     _                      = oldStatus

failIfGracefulShudownActivated :: AppM ()
failIfGracefulShudownActivated = do
  gracefulShudownAct <- gracefulShudownActivated <$> ask
  gracefulShudown <- liftIO . readIORef $ gracefulShudownAct
  if gracefulShudown
    then throwError err405 { errBody = appError "Graceful shutdown activated" }
    else pure ()

terminationHandler :: IORef Int -> IORef Bool -> MVar () -> IO ()
terminationHandler bgWorkersC gracefulShudownAct shutdownS = do
  atomicWriteIORef gracefulShudownAct True
  c <- readIORef bgWorkersC
  if c == 0
    then putMVar shutdownS ()
    else pure ()

runShutdownHandler :: AppState -> IO ()
runShutdownHandler = takeMVar . shutdownSem

runBgWorker :: AppState -> IO () -> IO ()
runBgWorker state act = void $ forkFinally act' cleanup
  where
    act' =
      atomicModifyIORef' (bgWorkersCounter state) (\c -> (c + 1, c)) >> act
    cleanup _ = do
      c <- atomicModifyIORef' (bgWorkersCounter state) (\c -> (c - 1, c - 1))
      gracefulShudown <- readIORef (gracefulShudownActivated state)
      if gracefulShudown && c == 0
        then putMVar (shutdownSem state) ()
        else pure ()
