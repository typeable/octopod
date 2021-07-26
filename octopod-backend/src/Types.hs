-- |
--Module      : Types
--Description : Backend types.
--
--This module contains backend types.
module Types
  ( module Common.Types,
    parseDeploymentMetadata,
    ServerPort (..),
    DBPoolSize (..),
    DBConnectionString (..),
    ProjectName (..),
    Domain (..),
    Namespace (..),
    Command (..),
    ArchiveRetention (..),
    Timeout (..),
    ControlScriptArgs (..),
  )
where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Text as T
import Data.Traversable

import Common.Types

-- | Parses deployment metadata.
parseDeploymentMetadata :: [Text] -> IO [DeploymentMetadata]
parseDeploymentMetadata texts =
  for texts $ \t ->
    case T.findIndex (== ',') t of
      Just i -> do
        let (key, value) = bimap strip (T.tail . strip) $ T.splitAt i t
        pure $ DeploymentMetadata key value
      Nothing ->
        error $
          "Malformed metadata key-value pair " <> T.unpack t
            <> ", should be similar to foo,bar"

-- | Server port.
newtype ServerPort = ServerPort {unServerPort :: Int}
  deriving stock (Show)

-- | Database pool size.
newtype DBPoolSize = DBPoolSize {unDBPoolSize :: Int}
  deriving stock (Show)

-- | Database connection string.
newtype DBConnectionString = DbConnectionString
  {unDBConnectionString :: ByteString}
  deriving stock (Show)

newtype Domain = Domain {unDomain :: Text}
  deriving stock (Show)

-- | A Kubernetes namespace.
newtype Namespace = Namespace {unNamespace :: Text}
  deriving stock (Show)

-- | Archive retention.
newtype ArchiveRetention = ArchiveRetention {unArchiveRetention :: Int}
  deriving stock (Show)

-- | Timeout.
newtype Timeout = Timeout {unTimeout :: Int}
  deriving stock (Show)

-- | Path to a deployment control script.
newtype Command = Command {unCommand :: Text}
  deriving stock (Show)

-- | Control script arguments.
newtype ControlScriptArgs = ControlScriptArgs
  {unControlScriptArgs :: [String]}
  deriving stock (Show)
