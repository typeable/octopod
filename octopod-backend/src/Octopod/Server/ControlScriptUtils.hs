-- |
--Module      : Octopod.Server.ControlScriptUtils
--Description : Control script utils.
--
--This module contains control script utils.
module Octopod.Server.ControlScriptUtils
  ( infoCommandArgs,
    createCommandArgs,
    updateCommandArgs,
    archiveCommandArgs,
    unarchiveCommandArgs,
    cleanupCommandArgs,
    notificationCommandArgs,
    runCommand,
    runCommandArgs,
    runCommandArgs',
    checkCommandArgs,
    archiveCheckArgs,
    configCheckCommandArgs,

    -- * overrides
    defaultDeploymentOverridesArgs,
    deploymentOverrideKeys,
    defaultApplicationOverridesArgs,
    applicationOverrideKeys,

    -- * Helpers
    fullConfigArgs,
    overridesArgs,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as TL
import Data.Coerce
import Data.Fixed
import Data.Generics.Product.Typed
import qualified Data.Map.Ordered.Strict as MO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time
import Octopod.Server.Logging
import System.Exit
import System.Process (terminateProcess)
import System.Process.Typed
import Types

type GenericDeploymentCommandArgs m r =
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  FullConfig ->
  Deployment ->
  m ControlScriptArgs

genericDeploymentCommandArgs :: GenericDeploymentCommandArgs m r
genericDeploymentCommandArgs cfg dep = do
  (Namespace namespace) <- asks getTyped
  (ProjectName projectName) <- asks getTyped
  (Domain domain) <- asks getTyped
  return $
    ControlScriptArgs
      [ "--project-name"
      , T.unpack . coerce $ projectName
      , "--base-domain"
      , T.unpack . coerce $ domain
      , "--namespace"
      , T.unpack . coerce $ namespace
      , "--name"
      , T.unpack . coerce $ dep ^. #name
      ]
      <> fullConfigArgs cfg

type GenericDeploymentCommandArgsNoConfig m r =
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  m ControlScriptArgs

genericDeploymentCommandArgsNoConfig :: GenericDeploymentCommandArgsNoConfig m r
genericDeploymentCommandArgsNoConfig = do
  (Namespace namespace) <- asks getTyped
  (ProjectName projectName) <- asks getTyped
  (Domain domain) <- asks getTyped
  return $
    ControlScriptArgs
      [ "--project-name"
      , T.unpack . coerce $ projectName
      , "--base-domain"
      , T.unpack . coerce $ domain
      , "--namespace"
      , T.unpack . coerce $ namespace
      ]

infoCommandArgs :: GenericDeploymentCommandArgs m r
infoCommandArgs = genericDeploymentCommandArgs

unarchiveCommandArgs :: GenericDeploymentCommandArgs m r
unarchiveCommandArgs = genericDeploymentCommandArgs

createCommandArgs :: GenericDeploymentCommandArgs m r
createCommandArgs = genericDeploymentCommandArgs

updateCommandArgs :: GenericDeploymentCommandArgs m r
updateCommandArgs = genericDeploymentCommandArgs

archiveCommandArgs :: GenericDeploymentCommandArgs m r
archiveCommandArgs = genericDeploymentCommandArgs

cleanupCommandArgs :: GenericDeploymentCommandArgs m r
cleanupCommandArgs = genericDeploymentCommandArgs

checkCommandArgs :: GenericDeploymentCommandArgs m r
checkCommandArgs = genericDeploymentCommandArgs

archiveCheckArgs :: GenericDeploymentCommandArgs m r
archiveCheckArgs = genericDeploymentCommandArgs

configCheckCommandArgs :: GenericDeploymentCommandArgs m r
configCheckCommandArgs = genericDeploymentCommandArgs

defaultDeploymentOverridesArgs :: GenericDeploymentCommandArgsNoConfig m r
defaultDeploymentOverridesArgs = genericDeploymentCommandArgsNoConfig

deploymentOverrideKeys :: GenericDeploymentCommandArgsNoConfig m r
deploymentOverrideKeys = genericDeploymentCommandArgsNoConfig

defaultApplicationOverridesArgs ::
  Config 'DeploymentLevel ->
  GenericDeploymentCommandArgsNoConfig m r
defaultApplicationOverridesArgs cfg =
  (overridesArgs cfg <>) <$> genericDeploymentCommandArgsNoConfig

applicationOverrideKeys ::
  Config 'DeploymentLevel ->
  GenericDeploymentCommandArgsNoConfig m r
applicationOverrideKeys cfg =
  (overridesArgs cfg <>) <$> genericDeploymentCommandArgsNoConfig

notificationCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  DeploymentName ->
  -- | Previous status
  DeploymentStatus ->
  -- | New status
  DeploymentStatus ->
  m ControlScriptArgs
notificationCommandArgs dName old new = do
  (Namespace namespace) <- asks getTyped
  (ProjectName projectName) <- asks getTyped
  (Domain domain) <- asks getTyped
  return $
    ControlScriptArgs
      [ "--project-name"
      , T.unpack projectName
      , "--base-domain"
      , T.unpack domain
      , "--namespace"
      , T.unpack namespace
      , "--name"
      , T.unpack . coerce $ dName
      , "--old-status"
      , T.unpack $ deploymentStatusToText old
      , "--new-status"
      , T.unpack $ deploymentStatusToText new
      ]

runCommandArgs ::
  ( KatipContext m
  , MonadBase IO m
  , MonadReader r m
  , HasType ControlScriptTimeout r
  ) =>
  (r -> Command) ->
  ControlScriptArgs ->
  m (ExitCode, Stdout, Stderr, Duration)
runCommandArgs f args = do
  cmd <- asks f
  runCommandArgs' cmd args

runCommandArgs' ::
  ( KatipContext m
  , MonadBase IO m
  , MonadReader r m
  , HasType ControlScriptTimeout r
  ) =>
  Command ->
  ControlScriptArgs ->
  m (ExitCode, Stdout, Stderr, Duration)
runCommandArgs' (Command cmd) (ControlScriptArgs args) = do
  let logText = T.unwords (cmd : fmap T.pack args)
  logLocM DebugS $ "Calling control script: " <> logStr logText
  t1 <- liftBase getCurrentTime
  (ec, out, err) <- runCommand (T.unpack cmd) args
  t2 <- liftBase getCurrentTime
  logLocM DebugS $ "Control script " <> logStr logText <> " exited with: " <> show' ec
  let elTime = elapsedTime t2 t1
  return (ec, out, err, elTime)

-- | Returns time delta between 2 timestamps.
elapsedTime :: UTCTime -> UTCTime -> Duration
elapsedTime t1 t2 = Duration . calendarTimeTime $ Prelude.abs $ t2 `diffUTCTime` t1

-- | Helper to run command with pipes.
runCommand ::
  ( MonadBase IO m
  , MonadReader r m
  , HasType ControlScriptTimeout r
  ) =>
  FilePath ->
  [String] ->
  m (ExitCode, Stdout, Stderr)
runCommand cmd args = do
  ControlScriptTimeout timeout <- asks getTyped
  let pc =
        proc cmd args
          & setStdout byteStringOutput
          & setStderr byteStringOutput
  (ec, out, err) <- liftBase $
    withProcessTerm pc $ \p -> do
      void . forkIO $ do
        threadDelayMicro $ realToFrac timeout
        terminateProcess $ unsafeProcessHandle p
      atomically $
        (,,)
          <$> waitExitCodeSTM p
          <*> getStdout p
          <*> getStderr p
  pure
    ( ec
    , Stdout . T.decodeUtf8 . TL.toStrict $ out
    , Stderr . T.decodeUtf8 . TL.toStrict $ err
    )
  where
    threadDelayMicro :: Micro -> IO ()
    threadDelayMicro (MkFixed i) = threadDelay (fromInteger i)

fullConfigArgs :: FullConfig -> ControlScriptArgs
fullConfigArgs cfg = overridesArgs (appConfig cfg) <> overridesArgs (depConfig cfg)

overridesArgs :: forall l. KnownOverrideLevel l => Config l -> ControlScriptArgs
overridesArgs (Config cc) =
  ControlScriptArgs
    . concatMap (\(T.unpack -> k, T.unpack -> v) -> [argumentName, k <> "=" <> v])
    . MO.assocs
    $ cc
  where
    argumentName = case knownOverrideLevel @l of
      ApplicationLevel -> "--application-config"
      DeploymentLevel -> "--deployment-config"
