-- |
--Module      : Octopod.Server.Logging
--Description : Octopod Server logging setup
module Octopod.Server.Logging
  ( runLog,
    show',
    logTM,
    logStr,
    katipAddContext,
    katipAddNamespace,
    Severity(..),
    KatipContext,
    KatipContextT,
    LogItem(..),
    ToObject(..),
    PayloadSelection(..),
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Control
import Katip
import System.IO
import Types hiding (stdout)


makeLogEnv :: ProjectName -> Severity -> IO LogEnv
makeLogEnv projname sev = do
  scribe <- mkHandleScribeWithFormatter bracketFormat ColorIfTerminal stdout (permitItem sev) V2
  env <- initLogEnv "octopod" $ Environment $ uProjectName projname
  env' <- registerScribe "stdout" scribe defaultScribeSettings env
  pure env'

destroyLogEnv :: LogEnv -> IO ()
destroyLogEnv = void . closeScribes

runLog :: MonadBaseControl IO m => ProjectName -> Severity -> KatipContextT m a -> m a
runLog projname sev act = liftBaseWith (\run -> bracket (makeLogEnv projname sev) destroyLogEnv $
  \env -> run $ runKatipContextT env () mempty act) >>= restoreM

show' :: Show a => a -> LogStr
show' = logStr . show
