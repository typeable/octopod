module DMS.Logger where

import Data.ByteString
import Data.Text
import System.Log.FastLogger

newLogger :: IO TimedFastLogger
newLogger = do
  timeCache <- newTimeCache "%Y-%m-%d %T%z"
  (logger, _) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
  pure logger

logInfo :: TimedFastLogger -> Text -> IO ()
logInfo l = logWithSeverity l "INFO"

logWarning :: TimedFastLogger -> Text -> IO ()
logWarning l = logWithSeverity l "WARN"

logWithSeverity :: ToLogStr msg => TimedFastLogger -> ByteString -> msg -> IO ()
logWithSeverity l severity msg = l $ \ft -> metadata ft <> message
  where
    metadata :: ByteString -> LogStr
    metadata ft = foldMap toLogStr ["[", ft, " ", severity, "] "]
    message     = toLogStr msg <> toLogStr ("\n" :: ByteString)
