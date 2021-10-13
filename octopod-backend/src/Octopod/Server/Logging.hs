-- |
--Module      : Octopod.Server.Logging
--Description : Octopod Server logging setup
module Octopod.Server.Logging
  ( runLog,
    LogConfig (..),
    show',
    logLocM,
    logStr,
    katipAddContext,
    katipAddNamespace,
    Severity (..),
    KatipContext,
    KatipContextT,
    LogItem (..),
    ToObject (..),
    PayloadSelection (..),
    FilePayload (..),
  )
where

import qualified Control.Exception.Lifted as L
import Control.Lens hiding (scribe, (.=))
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Lazy.Builder
import GHC.Generics
import Katip
import Katip.Format.Time
import Katip.Scribes.Handle
import System.IO
import Types hiding (stdout)

data LogConfig = LogConfig
  { project :: ProjectName
  , debug :: Bool
  , prodLogs :: Bool
  }
  deriving stock (Generic)

newtype SingleLineify a = SingleLineify a
  deriving newtype (LogItem)

instance ToObject a => ToObject (SingleLineify a) where
  toObject (SingleLineify x) = fmap go $ toObject x
    where
      go (Object obj) = Object $ fmap go obj
      go (Array v) = Array $ fmap go v
      go (String s) = String $ T.concat $ escape s
      go value = value
      escape s = case T.break (`elem` ("\a\b\f\n\r\t\v\\" :: String)) s of
        (xs, ys) ->
          xs : case T.uncons ys of
            Nothing -> []
            Just (c, zs) -> case c of
              '\a' -> "\\a" : escape zs
              '\b' -> "\\b" : escape zs
              '\f' -> "\\f" : escape zs
              '\n' -> "\\n" : escape zs
              '\r' -> "\\r" : escape zs
              '\t' -> "\\t" : escape zs
              '\v' -> "\\v" : escape zs
              '\\' -> "\\\\" : escape zs
              _ -> escape ys

makeLogEnv :: LogConfig -> IO LogEnv
makeLogEnv conf = do
  scribe <- mkHandleScribeWithFormatter formatter ColorIfTerminal stdout (permitItem severity) verbosity
  env <- initLogEnv "octopod" $ Environment $ uProjectName $ conf ^. #project
  env' <- registerScribe "stdout" scribe defaultScribeSettings env
  pure env'
  where
    verbosity = if conf ^. #prodLogs then V3 else V0
    severity = if conf ^. #debug then DebugS else InfoS
    formatter :: LogItem a => ItemFormatter a
    formatter =
      if conf ^. #prodLogs
        then \color verb item -> bracketFormat color verb item {_itemPayload = SingleLineify $ _itemPayload item}
        else \color verb item ->
          brackets (fromText $ formatAsLogTime $ _itemTime item)
            <> brackets (fromText $ colorBySeverity color (_itemSeverity item) $ renderSeverity $ _itemSeverity item)
            <> mconcat (map brackets $ getKeys verb $ _itemPayload item)
            <> " "
            <> unLogStr (_itemMessage item)
            <> mconcat (("\n" <>) <$> renderFiles (toObject $ _itemPayload item))
    renderFiles :: Object -> [Builder]
    renderFiles obj =
      HM.toList obj >>= \(k, v) -> case (T.stripPrefix "file-" k, v) of
        (Just fn, String text) | not $ T.null text -> pure $ fromText fn <> ":\n" <> fromText (tabulate text)
        _ -> []
    tabulate = ("\t" <>) . T.replace "\n" "\n\t"

destroyLogEnv :: LogEnv -> IO ()
destroyLogEnv = void . closeScribes

runLog :: MonadBaseControl IO m => LogConfig -> KatipContextT m a -> m a
runLog conf act = L.bracket (liftBase $ makeLogEnv conf) (liftBase . destroyLogEnv) $
  \env -> runKatipContextT env () mempty act

show' :: Show a => a -> LogStr
show' = logStr . show

data FilePayload = FilePayload
  { filename :: Text
  , contents :: ByteString
  }
  deriving stock (Generic)

instance ToObject FilePayload where
  toObject fp = ("file-" <> fp ^. #filename) .= T.decodeUtf8 (fp ^. #contents)

instance LogItem FilePayload where
  payloadKeys V3 _ = AllKeys
  payloadKeys _ _ = SomeKeys []
