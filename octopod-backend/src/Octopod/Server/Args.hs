-- |
--Module      : Octopod.Server.Args
--Description : Octopod Server arguments parser utils.
module Octopod.Server.Args
  ( Args (..),
    OctopodOpts (..),
    parseArgs,
  )
where

import Data.ByteString (ByteString)
import Data.Coerce
import Options.Generic

import Types

-- | Octopod Server arguments definition.
data Args = Args
  { -- | port for octo CLI
    port :: Int
  , -- | port for Web UI (HTTP)
    uiPort :: Int
  , -- | port for Web UI (WS)
    wsPort :: Int
  , -- | database connection string
    db :: ByteString
  , -- | database pool size
    dbPoolSize :: Int
  , -- | TLS certificate file path
    tlsCertPath :: ByteString
  , -- | TLS key file path
    tlsKeyPath :: ByteString
  , -- | TLS store path
    tlsStorePath :: ByteString
  }
  deriving stock (Generic, Show)

instance ParseRecord Args where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

-- | Parsed Octopod Server arguments definition.
data OctopodOpts = OctopodOpts
  { -- | port for Octopod Server
    octopodPort :: ServerPort
  , -- | port for UI (HTTP)
    octopodUIPort :: ServerPort
  , -- | port for UI (WS)
    octopodWSPort :: ServerPort
  , -- | database connection string
    octopodDB :: DBConnectionString
  , -- | database pool size
    octopodDBPoolSize :: DBPoolSize
  , -- | TLS certificate file path
    octopodTLSCertPath :: TLSCertPath
  , -- | TLS key file path
    octopodTLSKeyPath :: TLSKeyPath
  , -- | TLS store path
    octopodTLSStorePath :: TLSStorePath
  }
  deriving stock (Show)

-- | Parses Octopod Server arguments.
parseArgs :: IO OctopodOpts
parseArgs = do
  args <- getRecord "Octopod.Server"
  pure $
    OctopodOpts
      (coerce $ port args)
      (coerce $ uiPort args)
      (coerce $ wsPort args)
      (coerce $ db args)
      (coerce $ dbPoolSize args)
      (coerce $ tlsCertPath args)
      (coerce $ tlsKeyPath args)
      (coerce $ tlsStorePath args)
