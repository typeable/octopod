{-|
Module      : Octopod.Server.Args
Description : Octopod Server arguments parser utils.
-}

module Octopod.Server.Args where


import           Data.ByteString (ByteString)
import           Data.Coerce
import           Options.Generic


import           Types


-- | Octopod Server arguments definition.
data Args = Args
  { port :: Int
  -- ^ port for octo CLI
  , uiPort :: Int
  -- ^ port for Web UI (HTTP)
  , wsPort :: Int
  -- ^ port for Web UI (WS)
  , db :: ByteString
  -- ^ database connection string
  , dbPoolSize :: Int
  -- ^ database pool size
  , tlsCertPath :: ByteString
  -- ^ TLS certificate file path
  , tlsKeyPath :: ByteString
  -- ^ TLS key file path
  , tlsStorePath :: ByteString
  -- ^ TLS store path
  } deriving (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

-- | Parsed Octopod Server arguments definition.
data OctopodOpts = OctopodOpts
  { octopodPort :: ServerPort
  -- ^ port for Octopod Server
  , octopodUIPort :: ServerPort
  -- ^ port for UI (HTTP)
  , octopodWSPort :: ServerPort
  -- ^ port for UI (WS)
  , octopodDB :: DBConnectionString
  -- ^ database connection string
  , octopodDBPoolSize :: DBPoolSize
  -- ^ database pool size
  , octopodTLSCertPath :: TLSCertPath
  -- ^ TLS certificate file path
  , octopodTLSKeyPath :: TLSKeyPath
  -- ^ TLS key file path
  , octopodTLSStorePath :: TLSStorePath
  -- ^ TLS store path
  } deriving (Show)

-- | Parses Octopod Server arguments.
parseArgs :: IO OctopodOpts
parseArgs = do
  args <- getRecord "Octopod.Server"
  pure $ OctopodOpts
    (coerce $ port args)
    (coerce $ uiPort args)
    (coerce $ wsPort args)
    (coerce $ db args)
    (coerce $ dbPoolSize args)
    (coerce $ tlsCertPath args)
    (coerce $ tlsKeyPath args)
    (coerce $ tlsStorePath args)
