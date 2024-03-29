module Octopod.Server.Posix (installShutdownHandler) where

import Control.Monad.Trans.Control
import Data.Aeson
import Data.Traversable
import Octopod.Server.Logging
import System.Posix.Signals

newtype SigContext = SigContext Signal

instance ToObject SigContext where
  toObject (SigContext signal) = "signal" .= show signal

instance LogItem SigContext where
  payloadKeys _ _ = AllKeys

-- | Installs the given shutdown handler for the specified signals.
installShutdownHandler ::
  (KatipContext m, MonadBaseControl IO m, StM m () ~ ()) =>
  [Signal] ->
  m () ->
  m [Handler]
installShutdownHandler signals action =
  for signals $ \signal -> liftBaseWith $ \run ->
    installHandler signal (Catch $ run (handler signal)) Nothing
  where
    handler signal = katipAddNamespace "signal handler" $
      katipAddContext (SigContext signal) $ do
        logLocM InfoS "Shutdown initiated by signal"
        action
