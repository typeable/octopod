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

-- | Creates command arguments for the 'info' deployment control script.
infoCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  FullDefaultConfig ->
  Deployment ->
  m ControlScriptArgs
infoCommandArgs dCfg dep = do
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
      <> fullConfigArgs dCfg dep

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
      , T.unpack $ deploymentStatusToText old
      , "--new-status"
      , T.unpack $ deploymentStatusToText new
      ]

checkCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  FullDefaultConfig ->
  Deployment ->
  m ControlScriptArgs
checkCommandArgs dCfg dep = do
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
      <> fullConfigArgs dCfg dep

tagCheckCommandArgs ::
  ( MonadReader r m
  , HasType Namespace r
  , HasType ProjectName r
  , HasType Domain r
  ) =>
  FullDefaultConfig ->
  Deployment ->
  m ControlScriptArgs
tagCheckCommandArgs dCfg dep = do
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
      <> fullConfigArgs dCfg dep

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

fullConfigArgs :: FullDefaultConfig -> Deployment -> ControlScriptArgs
fullConfigArgs defCfg dep =
  overridesArgs (applyOverrides (dep ^. #appOverrides) (appDefaultConfig defCfg))
    <> overridesArgs (applyOverrides (dep ^. #deploymentOverrides) (depDefaultConfig defCfg))

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
