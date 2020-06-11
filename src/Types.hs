module Types where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.String
import Data.Text as T
import Data.Traversable
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Deriving.Aeson.Stock
import Web.HttpApiData


type EnvPair = (Text, Text)

type EnvPairs = [EnvPair]

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
      -- ^ T.splitAt returns pair with not empty Texts, e.g. ("X", "=Y")
      Nothing -> error $
        "Malformed environment key-value pair " <> T.unpack t <>
        ", should be similar to FOO=bar"

newtype DeploymentName = DeploymentName { unDeploymentName :: Text }
  deriving
    ( Show, Read, FromJSON, ToJSON, FromField, ToField
    , ToHttpApiData, FromHttpApiData)

newtype DeploymentTag = DeploymentTag { unDeploymentTag :: Text }
  deriving
    ( Show, FromJSON, ToJSON, FromField, ToField
    , ToHttpApiData, FromHttpApiData)

newtype Action = Action { unAction :: Text }
  deriving (Show, FromJSON, ToJSON, FromField, ToField, IsString)

newtype ArchivedFlag = ArchivedFlag { unArchivedFlag :: Bool }
  deriving (Show, FromJSON, ToJSON, FromField, ToField)

data Deployment = Deployment
  { name :: DeploymentName
  , tag  :: DeploymentTag
  , envs :: EnvPairs
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake Deployment

data DeploymentLog = DeploymentLog
  { action         :: Action
  , deploymentTag  :: DeploymentTag
  , deploymentEnvs :: EnvPairs
  , exitCode       :: Int
  , createdAt      :: Int
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentLog

data DeploymentInfo = DeploymentInfo
  { deployment :: Deployment
  , logs       :: [DeploymentLog]
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentInfo

data DeploymentFullInfo = DeploymentFullInfo
  { deployment :: Deployment
  , archived   :: Bool
  , createdAt  :: Int
  , updatedAt  :: Int
  , urls       :: [(Text, Text)]
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentFullInfo

data DeploymentUpdate = DeploymentUpdate
  { newTag :: DeploymentTag
  , newEnvs :: Maybe EnvPairs
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentUpdate

data Status
  = Ok
  | Error
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake Status

newtype DeploymentStatus = DeploymentStatus { status :: Status }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentStatus

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

newtype ProjectName = ProjectName { unProjectName :: Text }
  deriving (Show)

newtype Domain = Domain { unDomain :: Text }
  deriving (Show)

newtype Namespace = Namespace { unNamespace :: Text }
  deriving (Show)

newtype ArchiveRetention = ArchiveRetention { unArchiveRetention :: Int }
  deriving (Show)

newtype Command = Command { unCommand :: Text }
  deriving (Show)