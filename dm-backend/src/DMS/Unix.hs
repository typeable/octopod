module DMS.Unix (installShutdownHandler) where


import Control.Monad
import Control.Monad.Base (liftBase)
import Data.Text
import System.Log.FastLogger
import System.Posix.Signals


import DMS.Logger


installShutdownHandler
  :: TimedFastLogger
  -> [Signal]
  -> IO ()
  -> IO [Handler]
installShutdownHandler logger signals action =
  forM signals $ \signal -> installHandler signal (handler signal) Nothing
  where
    handler signal = Catch $ do
      logInfo logger $ "Shutdown initiated by signal " <> (pack . show $ signal)
      liftBase action
