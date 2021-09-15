module Octopod.Server.Posix (installShutdownHandler) where

import Control.Monad
import Control.Monad.Base (liftBase)
import Data.Text
import System.Log.FastLogger
import System.Posix.Signals

import Octopod.Server.Logger

-- | Installs the given shutdown handler for the specified signals.
installShutdownHandler ::
  TimedFastLogger ->
  [Signal] ->
  IO () ->
  IO [Handler]
installShutdownHandler logger signals action =
  forM signals $ \signal -> installHandler signal (handler signal) Nothing
  where
    handler signal = Catch $ do
      logInfo logger $ "Shutdown initiated by signal " <> (pack . show $ signal)
      liftBase action
