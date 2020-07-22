module Types
  ( module Common.Types
  , overrideToArg
  , formatOverrides
  , parseSetApplicationOverrides
  , parseSetStagingOverrides
  , parseUnsetApplicationOverrides
  , parseUnsetStagingOverrides
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
import Data.Coerce
import Data.Text as T
import Data.Traversable

import Common.Types

overrideToArg :: Override -> Text
overrideToArg (Override k v _) = k <> "=" <> v

formatOverride :: Override -> Text
formatOverride o@(Override _ _ vis) =
  overrideToArg o <> " (" <> pack (show vis) <> ")"

-- | Format for output
formatOverrides :: Overrides -> Text
formatOverrides = T.unlines . fmap formatOverride

parseSetApplicationOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [ApplicationOverride]
parseSetApplicationOverrides visibility texts =
  coerce <$> parseSetOverrides visibility texts

parseSetStagingOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [StagingOverride]
parseSetStagingOverrides visibility texts =
  coerce <$> parseSetOverrides visibility texts

parseSetOverrides :: OverrideVisibility -> [Text] -> IO [Override]
parseSetOverrides visibility texts =
  for texts $ \t ->
    case T.findIndex (== '=') t of
      Just i  -> do
        let
          (key, value) =  bimap strip (T.tail . strip) $ T.splitAt i t
        pure $ Override key value visibility
      Nothing -> error $
        "Malformed override key-value pair " <> T.unpack t <>
        ", should be similar to FOO=bar"

parseUnsetApplicationOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [ApplicationOverride]
parseUnsetApplicationOverrides visibility texts =
  coerce <$> parseUnsetOverrides visibility texts

parseUnsetStagingOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [StagingOverride]
parseUnsetStagingOverrides visibility texts =
  coerce <$> parseUnsetOverrides visibility texts

parseUnsetOverrides :: OverrideVisibility -> [Text] -> IO [Override]
parseUnsetOverrides visibility texts =
  for texts $ \key ->
    pure $ Override key "" visibility

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
