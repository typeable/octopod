module DMS.Args where


import Data.ByteString (ByteString)
import Data.Coerce
import Options.Generic


import Types


data Args = Args
  { port :: Int
  , db :: ByteString
  , dbPoolSize :: Int
  , tlsCertPath :: ByteString
  , tlsKeyPath :: ByteString
  , tlsStorePath :: ByteString
  }
  deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

data DMSOpts = DMSOpts
  { dmsPort :: ServerPort
  , dmsDB :: DBConnectionString
  , dmsDBPoolSize :: DBPoolSize
  , dmsTLSCertPath :: TLSCertPath
  , dmsTLSKeyPath :: TLSKeyPath
  , dmsTLSStorePath :: TLSStorePath
  }
  deriving (Show)

parseArgs :: IO DMSOpts
parseArgs = do
  args <- getRecord "DMS"
  pure $ DMSOpts
    (coerce $ port args)
    (coerce $ db args)
    (coerce $ dbPoolSize args)
    (coerce $ tlsCertPath args)
    (coerce $ tlsKeyPath args)
    (coerce $ tlsStorePath args)
