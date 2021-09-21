-- |
--Module      : Octopod.Server.Logging
--Description : Octopod Server logging setup
module Octopod.Server.Logging
  ( runLog,
    LogConfig(..),
    show',
    logLocM,
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
import Control.Lens hiding (scribe)
import Control.Monad
import Control.Monad.Trans.Control
import Data.List (intersperse)
import Data.Text.Lazy.Builder
import GHC.Generics
import Katip
import Katip.Core
import Katip.Scribes.Handle
import System.IO
import Types hiding (stdout)

data LogConfig = LogConfig
  { project :: ProjectName
  , debug :: Bool
  , minimal :: Bool
  }
  deriving stock (Generic)

makeLogEnv :: LogConfig -> IO LogEnv
makeLogEnv conf = do
  scribe <- mkHandleScribeWithFormatter formatter ColorIfTerminal stdout (permitItem severity) V3
  env <- initLogEnv "octopod" $ Environment $ uProjectName $ conf ^. #project
  env' <- registerScribe "stdout" scribe defaultScribeSettings env
  pure env'
  where
    severity = if conf ^. #debug then DebugS else InfoS
    formatter :: LogItem a => ItemFormatter a
    formatter = if not $ conf ^. #minimal then bracketFormat else
      \_color _verb item ->
        mconcat (fromText <$> intercalateNs (_itemNamespace item)) <> fromText "(" <>
        fromText (renderSeverity $ _itemSeverity item) <> fromText "): " <>
        mconcat (intersperse (fromText " ") $
          map brackets (getKeys V0 $ _itemPayload item) ++ [unLogStr (_itemMessage item)])


destroyLogEnv :: LogEnv -> IO ()
destroyLogEnv = void . closeScribes

runLog :: MonadBaseControl IO m => LogConfig -> KatipContextT m a -> m a
runLog conf act = liftBaseWith (\run -> bracket (makeLogEnv conf) destroyLogEnv $
  \env -> run $ runKatipContextT env () mempty act) >>= restoreM

show' :: Show a => a -> LogStr
show' = logStr . show
