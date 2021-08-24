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

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Csv
import Data.Foldable
import Data.Text as T
import Data.Text.Encoding as T

import Common.Types
import Data.Time

-- | Parses deployment metadata.
parseDeploymentMetadata :: Text -> IO DeploymentMetadata
parseDeploymentMetadata text = case decode NoHeader $ BS.fromStrict $ T.encodeUtf8 text of
  Left err -> error $ "Could not parse metadata: " <> err
  Right rows -> pure $ DeploymentMetadata $ toList rows

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
newtype ArchiveRetention = ArchiveRetention {unArchiveRetention :: NominalDiffTime}
  deriving stock (Show)

-- | Timeout.
newtype Timeout = Timeout {unTimeout :: CalendarDiffTime}
  deriving stock (Show)

-- | Path to a deployment control script.
newtype Command = Command {unCommand :: Text}
  deriving stock (Show)

-- | Control script arguments.
newtype ControlScriptArgs = ControlScriptArgs
  {unControlScriptArgs :: [String]}
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)
