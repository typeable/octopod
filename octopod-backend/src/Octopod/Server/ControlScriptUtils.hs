{-|
Module      : Octopod.Server.ControlScriptUtils
Description : Control script utils.

This module contains control script utils.
-}


module Octopod.Server.ControlScriptUtils
  ( infoCommandArgs
  , notificationCommandArgs
  , runCommand
  , runCommandWithoutPipes
  , runCommandArgs
  , runCommandArgs'
  , checkCommandArgs
  ) where


import           Control.Monad.Base
import           Control.Monad.Reader
import qualified Data.ByteString.Lazy as TL
import           Data.Coerce
import           Data.Generics.Product.Typed
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Octopod.Server.Logger
import           System.Exit
import           System.Log.FastLogger
import           System.Process.Typed
import           Types


-- | Creates command arguments for the 'info' deployment control script.
infoCommandArgs
  :: ProjectName
  -> Domain
  -> Namespace
  -> DeploymentName
  -> ControlScriptArgs
infoCommandArgs pName domain ns dName =
  ControlScriptArgs
    [ "--project-name", T.unpack . coerce $ pName
    , "--base-domain", T.unpack . coerce $ domain
    , "--namespace", T.unpack . coerce $ ns
    , "--name", T.unpack . coerce $ dName ]

notificationCommandArgs
  ::
    ( MonadReader r m
    , HasType Namespace r
    , HasType ProjectName r
    , HasType Domain r
    )
  => DeploymentName
  -> DeploymentTag
  -> DeploymentStatus
  -- ^ Previous status
  -> DeploymentStatus
  -- ^ New status
  -> m ControlScriptArgs
notificationCommandArgs dName dTag old new = do
  (Namespace namespace) <- asks getTyped
  (ProjectName projectName) <- asks getTyped
  (Domain domain) <- asks getTyped
  return $ ControlScriptArgs
    [ "--project-name", T.unpack projectName
    , "--base-domain", T.unpack domain
    , "--namespace", T.unpack namespace
    , "--name", T.unpack . coerce $ dName
    , "--tag", T.unpack . coerce $ dTag
    , "--old-status", T.unpack $ deploymentStatusText old
    , "--new-status", T.unpack $ deploymentStatusText new
    ]

checkCommandArgs
  ::
    ( MonadReader r m
    , HasType Namespace r
    )
  => DeploymentName
  -> DeploymentTag
  -> m ControlScriptArgs
checkCommandArgs dName dTag  = do
  (Namespace namespace) <- asks getTyped
  return $ ControlScriptArgs
    [ "--namespace", T.unpack namespace
    , "--name", T.unpack . coerce $ dName
    , "--tag", T.unpack . unDeploymentTag $ dTag
    ]

runCommandArgs
  :: (MonadReader r m, MonadBase IO m, HasType TimedFastLogger r)
  => (r -> Command) -> ControlScriptArgs -> m (ExitCode, Stdout, Stderr)
runCommandArgs f args = do
  cmd <- asks f
  runCommandArgs' cmd args

runCommandArgs'
  :: (MonadBase IO m, HasType TimedFastLogger r, MonadReader r m)
  => Command -> ControlScriptArgs -> m (ExitCode, Stdout, Stderr)
runCommandArgs' (Command cmd) (ControlScriptArgs args) = do
  logger <- asks (getTyped @TimedFastLogger)
  let logText = T.unwords (cmd : fmap T.pack args)
  liftBase $ logInfo logger $ "calling: " <> logText
  res@(ec, _, _) <- runCommand (T.unpack cmd) args
  liftBase $ logInfo logger
    $ "calling `" <> logText <> "` exited with: " <> show' ec
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
