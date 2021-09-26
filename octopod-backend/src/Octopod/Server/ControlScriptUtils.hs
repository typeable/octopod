-- |
--Module      : Octopod.Server.ControlScriptUtils
--Description : Control script utils.
--
--This module contains control script utils.
module Octopod.Server.ControlScriptUtils
  ( infoCommandArgs,
    archiveCommandArgs,
    unarchiveCommandArgs,
    cleanupCommandArgs,
    notificationCommandArgs,
    runCommand,
    runCommandWithoutPipes,
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

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as TL
import Data.Coerce
import Data.Generics.Product.Typed
import qualified Data.Map.Ordered.Strict as MO
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Octopod.Server.Logger
import System.Exit
import System.Log.FastLogger
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

unarchiveCommandArgs :: GenericDeploymentCommandArgs m r
unarchiveCommandArgs = genericDeploymentCommandArgs

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

archiveCommandArgs :: GenericDeploymentCommandArgs m r
archiveCommandArgs = genericDeploymentCommandArgs

cleanupCommandArgs :: GenericDeploymentCommandArgs m r
cleanupCommandArgs = genericDeploymentCommandArgs

archiveCheckArgs :: GenericDeploymentCommandArgs m r
archiveCheckArgs = genericDeploymentCommandArgs

checkCommandArgs :: GenericDeploymentCommandArgs m r
checkCommandArgs = genericDeploymentCommandArgs

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
  (MonadReader r m, MonadBase IO m, HasType TimedFastLogger r) =>
  (r -> Command) ->
  ControlScriptArgs ->
  m (ExitCode, Stdout, Stderr)
runCommandArgs f args = do
  cmd <- asks f
  runCommandArgs' cmd args

runCommandArgs' ::
  (MonadBase IO m, HasType TimedFastLogger r, MonadReader r m) =>
  Command ->
  ControlScriptArgs ->
  m (ExitCode, Stdout, Stderr)
runCommandArgs' (Command cmd) (ControlScriptArgs args) = do
  logger <- asks (getTyped @TimedFastLogger)
  let logText = T.unwords (cmd : fmap T.pack args)
  liftBase $ logInfo logger $ "calling: " <> logText
  res@(ec, _, _) <- runCommand (T.unpack cmd) args
  liftBase $
    logInfo logger $
      "calling `" <> logText <> "` exited with: " <> show' ec
  return res

-- | Helper to run command with pipes.
runCommand :: MonadBase IO m => FilePath -> [String] -> m (ExitCode, Stdout, Stderr)
runCommand cmd args = do
  (ec, out, err) <- liftBase . readProcess $ proc cmd args
  pure
    ( ec
    , Stdout . T.decodeUtf8 . TL.toStrict $ out
    , Stderr . T.decodeUtf8 . TL.toStrict $ err
    )

-- | Helper to run command without pipes.
runCommandWithoutPipes :: FilePath -> [String] -> IO ExitCode
runCommandWithoutPipes cmd args =
  withProcessWait (proc cmd args) waitExitCode

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
      ApplicationLevel -> "--app-env-override"
      DeploymentLevel -> "--deployment-override"
