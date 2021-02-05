{-|
Module      : Octopod.Server.Logger
Description : Octopod Server logger.
-}


module Octopod.Server.Logger where

import           Data.ByteString
import           Data.Text
import qualified Data.Text as T
import           System.Log.FastLogger

-- | Creates new logger.
newLogger :: IO TimedFastLogger
newLogger = do
  timeCache <- newTimeCache "%Y-%m-%d %T%z"
  (logger, _) <- newTimedFastLogger timeCache (LogStdout defaultBufSize)
  pure logger

-- | Logs a message with the 'INFO' severity.
logInfo :: TimedFastLogger -> Text -> IO ()
logInfo l = logWithSeverity l "INFO"

-- | Logs a message with the 'WARN' severity.
logWarning :: TimedFastLogger -> Text -> IO ()
logWarning l = logWithSeverity l "WARN"

-- | Logs a message with the specified severity.
logWithSeverity :: ToLogStr msg => TimedFastLogger -> ByteString -> msg -> IO ()
logWithSeverity l severity msg = l $ \ft -> metadata ft <> message
  where
    metadata :: ByteString -> LogStr
    metadata ft = foldMap toLogStr ["[", ft, " ", severity, "] "]
    message     = toLogStr msg <> toLogStr ("\n" :: ByteString)

show' :: Show a => a -> Text
show' = T.pack . show
