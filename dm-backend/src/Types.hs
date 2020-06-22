module Types
  ( module Common.Types
  , concatPair
  , concatPairWithAppEnv
  , formatEnvPairs
  , parseEnvs
  , ServerPort(..)
  , DBPoolSize(..)
  , DBConnectionString(..)
  , TLSCertPath(..)
  , TLSKeyPath(..)
  , TLSStorePath(..)
  , ProjectName(..)
  , Domain(..)
  , Namespace(..)
  , Command(..)
  , ArchiveRetention(..)) where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Text as T
import Data.Traversable

import Common.Types

-- | Format for command arguments
concatPair :: EnvPair -> Text
concatPair (k, v) = k <> "=" <> v

-- | Format for b2b-helm-tool / helm
concatPairWithAppEnv :: EnvPair -> Text
concatPairWithAppEnv pair = "b2b-app.env." <> concatPair pair

-- | Format for output
formatEnvPairs :: EnvPairs -> Text
formatEnvPairs = T.unlines . fmap concatPair

parseEnvs :: [Text] -> IO [(Text, Text)]
parseEnvs texts =
  for texts $ \t ->
    case T.findIndex (== '=') t of
      Just i  -> pure $ bimap strip (T.tail . strip) $ T.splitAt i t
      Nothing -> error $
        "Malformed environment key-value pair " <> T.unpack t <>
        ", should be similar to FOO=bar"

newtype ServerPort = ServerPort { unServerPort :: Int }
  deriving (Show)

newtype DBPoolSize = DBPoolSize { unDBPoolSize :: Int }
  deriving (Show)

newtype DBConnectionString = DbConnectionString
  { unDBConnectionString :: ByteString }
  deriving (Show)

newtype TLSCertPath = TLSCertPath { unTLSCertPath :: ByteString }
  deriving (Show)

newtype TLSKeyPath = TLSKeyPath { unTLSKeyPath :: ByteString }
  deriving (Show)

newtype TLSStorePath = TLSStore { unTLSStorePath :: ByteString }
  deriving (Show)

newtype Domain = Domain { unDomain :: Text }
  deriving (Show)

newtype Namespace = Namespace { unNamespace :: Text }
  deriving (Show)

newtype ArchiveRetention = ArchiveRetention { unArchiveRetention :: Int }
  deriving (Show)

newtype Command = Command { unCommand :: Text }
  deriving (Show)
