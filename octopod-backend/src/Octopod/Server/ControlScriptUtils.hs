-- |
--Module      : Octopod.Server.ControlScriptUtils
--Description : Control script utils.
--
--This module contains control script utils.
module Octopod.Server.ControlScriptUtils
  ( infoCommandArgs,
    notificationCommandArgs,
    runCommand,
    runCommandWithoutPipes,
    runCommandArgs,
    runCommandArgs',
    checkCommandArgs,
    archiveCheckArgs,
    tagCheckCommandArgs,

    -- * Helpers
    applicationOverrideToArg,
    applicationOverridesToArgs,
    deploymentOverrideToArg,
    deploymentOverridesToArgs,
  )
where

import Control.Monad.Base
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as TL
import Data.Coerce
import Data.Generics.Product.Typed
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Octopod.Server.Logger
import System.Exit
import System.Log.FastLogger
import System.Process.Typed
import Types

-- | Creates command arguments for the 'info' deployment control script.
infoCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  Deployment ->
  m ControlScriptArgs
infoCommandArgs dep = do
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
      , T.unpack . coerce $ name dep
      , "--tag"
      , T.unpack . coerce $ tag dep
      ]
      <> getApplicationOverrideArgs dep
      <> getDeploymentOverrideArgs dep

notificationCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  DeploymentName ->
  DeploymentTag ->
  -- | Previous status
  DeploymentStatus ->
  -- | New status
  DeploymentStatus ->
  m ControlScriptArgs
notificationCommandArgs dName dTag old new = do
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
      , "--tag"
      , T.unpack . coerce $ dTag
      , "--old-status"
      , T.unpack $ deploymentStatusText old
      , "--new-status"
      , T.unpack $ deploymentStatusText new
      ]

checkCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  Deployment ->
  m ControlScriptArgs
checkCommandArgs dep = do
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
      , T.unpack . coerce $ name dep
      , "--tag"
      , T.unpack . coerce $ tag dep
      ]
      <> getApplicationOverrideArgs dep
      <> getDeploymentOverrideArgs dep

tagCheckCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  Deployment ->
  m ControlScriptArgs
tagCheckCommandArgs dep = do
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
      , T.unpack . coerce $ name dep
      , "--tag"
      , T.unpack . coerce $ tag dep
      ]
      <> getApplicationOverrideArgs dep
      <> getDeploymentOverrideArgs dep

archiveCheckArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  DeploymentName ->
  m ControlScriptArgs
archiveCheckArgs dName = do
  (ProjectName projectName) <- asks getTyped
  (Domain domain) <- asks getTyped
  (Namespace namespace) <- asks getTyped
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

-- | Converts an application-level override list to command arguments.
applicationOverrideToArg :: ApplicationOverride -> [Text]
applicationOverrideToArg o = ["--app-env-override", overrideToArg . coerce $ o]

-- | Helper to convert an application-level override to command arguments.
applicationOverridesToArgs :: ApplicationOverrides -> [Text]
applicationOverridesToArgs ovs = concat [applicationOverrideToArg o | o <- ovs]

getApplicationOverrideArgs :: Deployment -> ControlScriptArgs
getApplicationOverrideArgs =
  ControlScriptArgs . map T.unpack . applicationOverridesToArgs . appOverrides

-- | Converts a deployment-level override list to command arguments.
deploymentOverrideToArg :: DeploymentOverride -> [Text]
deploymentOverrideToArg o =
  ["--deployment-override", overrideToArg . coerce $ o]

-- | Helper to convert a deployment-level override to command arguments.
deploymentOverridesToArgs :: DeploymentOverrides -> [Text]
deploymentOverridesToArgs ovs = concat [deploymentOverrideToArg o | o <- ovs]

getDeploymentOverrideArgs :: Deployment -> ControlScriptArgs
getDeploymentOverrideArgs =
  ControlScriptArgs . map T.unpack . deploymentOverridesToArgs . deploymentOverrides
