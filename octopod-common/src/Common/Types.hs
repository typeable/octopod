{-|
Module      : Common.Types
Description : Common types for backend and frontend.

This module contains common types between the backend and the frontend.
-}


module Common.Types where

import           Data.Bifunctor
import           Data.ByteString (ByteString)
import           Data.Coerce
import           Data.String
import           Data.Text as T
import           Data.Traversable
import           Deriving.Aeson.Stock
import           GHC.Records
import           Web.HttpApiData

-- | Deployment override.
data Override = Override
  { overrideKey :: Text
  , overrideValue :: Text
  , overrideVisibility :: OverrideVisibility
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake Override

type Overrides = [Override]

-- | Deployment override scope.
data OverrideScope
  = ApplicationScope
  | DeploymentScope
  deriving (Generic, Show, Read, Eq)
  deriving (FromJSON, ToJSON) via Snake OverrideScope

-- | Deployment override visibility.
data OverrideVisibility
  = Private
  | Public
  deriving (Generic, Show, Read, Eq)
  deriving (FromJSON, ToJSON) via Snake OverrideVisibility

-- | Deployment application-level override.
newtype ApplicationOverride =
  ApplicationOverride { unApplicationOverride :: Override }
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Deployment application-level overrides.
type ApplicationOverrides = [ApplicationOverride]

-- | Deployment-level override.
newtype DeploymentOverride = DeploymentOverride
  { unDeploymentOverride :: Override }
  deriving (Show, Eq, FromJSON, ToJSON)

-- | Deployment-level overrides.
type DeploymentOverrides = [DeploymentOverride]

newtype DeploymentId = DeploymentId { unDeploymentId :: Int }
  deriving (Show)

newtype DeploymentName = DeploymentName { unDeploymentName :: Text }
  deriving
    (Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq, Ord)

newtype DeploymentTag = DeploymentTag { unDeploymentTag :: Text }
  deriving
    (Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq)

newtype Action = Action { unAction :: Text }
  deriving (Show, FromJSON, ToJSON, IsString)

newtype ArchivedFlag = ArchivedFlag { unArchivedFlag :: Bool }
  deriving (Show, FromJSON, ToJSON)

newtype Duration = Duration { unDuration :: Int }
  deriving (Show, FromJSON, ToJSON)

newtype Timestamp = Timestamp { unTimestamp :: Int }
  deriving (Show, Eq, Ord, FromJSON, ToJSON)

newtype ProjectName = ProjectName { uProjectName :: Text }
  deriving (Show, FromJSON, ToJSON)

deploymentStatusText :: DeploymentStatus -> Text
deploymentStatusText Running = "Running"
deploymentStatusText (Failure GenericFailure) = "GenericFailure"
deploymentStatusText (Failure TagMismatch) = "TagMismatch"
deploymentStatusText (Failure PartialAvailability) = "PartialAvailability"
deploymentStatusText CreatePending = "CreatePending"
deploymentStatusText UpdatePending = "UpdatePending"
deploymentStatusText ArchivePending = "ArchivePending"
deploymentStatusText Archived = "Archived"

data DeploymentStatus
  = Running
  | Failure FailureType
  | CreatePending
  | UpdatePending
  | ArchivePending
  | Archived
  deriving (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentStatus

data FailureType
  = GenericFailure
  | TagMismatch
  | PartialAvailability
  deriving (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake FailureType

data PreciseDeploymentStatus
  = DeploymentPending { recordedStatus :: DeploymentStatus }
  -- ^ The deployment is currently being processed by the server
  | DeploymentNotPending { recordedStatus :: DeploymentStatus }
  deriving (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake PreciseDeploymentStatus

archivedStatuses :: [DeploymentStatus]
archivedStatuses = [ArchivePending, Archived]

isArchivedStatus :: DeploymentStatus -> Bool
isArchivedStatus = (`elem` archivedStatuses)

data Deployment = Deployment
  { name :: DeploymentName
  , tag :: DeploymentTag
  , appOverrides :: ApplicationOverrides
  , deploymentOverrides :: DeploymentOverrides
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake Deployment

data DeploymentLog = DeploymentLog
  { actionId :: ActionId
  , action :: Action
  , deploymentTag :: DeploymentTag
  , deploymentAppOverrides :: ApplicationOverrides
  , deploymentDepOverrides :: DeploymentOverrides
  , exitCode :: Int
  , duration :: Duration
  , createdAt :: Int
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentLog

data DeploymentMetadata = DeploymentMetadata
  { deploymentMetadataKey :: Text -- ^ The name of the link
  , deploymentMetadataValue :: Text -- ^ The URL
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentMetadata

data DeploymentInfo = DeploymentInfo
  { deployment :: Deployment
  , metadata :: [DeploymentMetadata]
  , logs :: [DeploymentLog]
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentInfo

data DeploymentFullInfo = DeploymentFullInfo
  { deployment :: Deployment
  , status :: PreciseDeploymentStatus
  , metadata :: [DeploymentMetadata]
  , createdAt :: Int
  , updatedAt :: Int
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentFullInfo

isDeploymentArchived :: DeploymentFullInfo -> Bool
isDeploymentArchived = isArchivedStatus . recordedStatus . getField @"status"

data DeploymentUpdate = DeploymentUpdate
  { newTag :: DeploymentTag
  , newAppOverrides :: ApplicationOverrides
  , oldAppOverrides :: ApplicationOverrides
  , newDeploymentOverrides :: DeploymentOverrides
  , oldDeploymentOverrides :: DeploymentOverrides
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentUpdate

data CurrentStatus
  = Ok
  | Error
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake CurrentStatus

newtype CurrentDeploymentStatus =
  CurrentDeploymentStatus { status :: CurrentStatus }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake CurrentDeploymentStatus

data CommandResponse
  = Success
  | ValidationError
    { nameField :: [Text]
    , tagField :: [Text]
    }
  | AppError
    { errorMessage :: Text }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake CommandResponse

data WSEvent = FrontendPleaseUpdateEverything
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake WSEvent

newtype ActionId = ActionId { unActionId :: Int }
  deriving
    (Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq)

newtype Stdout = Stdout { unStdout :: Text }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake Stdout

newtype Stderr = Stderr { unStderr :: Text }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake Stderr

data ActionInfo = ActionInfo
  { stdout :: Text
  , stderr :: Text
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake ActionInfo

-- | TLS certificate path.
newtype TLSCertPath = TLSCertPath { unTLSCertPath :: ByteString }
  deriving (Show)

-- | TLS key path.
newtype TLSKeyPath = TLSKeyPath { unTLSKeyPath :: ByteString }
  deriving (Show)

-- | Parses setting application-level overrides.
parseSetApplicationOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [ApplicationOverride]
parseSetApplicationOverrides visibility texts =
  coerce <$> parseSetOverrides visibility texts

-- | Parses setting deployment-level overrides.
parseSetDeploymentOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [DeploymentOverride]
parseSetDeploymentOverrides visibility texts =
  coerce <$> parseSetOverrides visibility texts

-- | Parses setting overrides.
parseSetOverrides :: OverrideVisibility -> [Text] -> IO [Override]
parseSetOverrides visibility texts =
  for texts $ \t ->
    case T.findIndex (== '=') t of
      Just i -> do
        let
          (key, value) = bimap strip (T.tail . strip) $ T.splitAt i t
        pure $ Override key value visibility
      Nothing -> error $
        "Malformed override key-value pair " <> T.unpack t <>
        ", should be similar to FOO=bar"

-- | Parses unsetting application-level overrides.
parseUnsetApplicationOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [ApplicationOverride]
parseUnsetApplicationOverrides visibility texts =
  coerce <$> parseUnsetOverrides visibility texts

-- | Parses unsetting deployment-level overrides.
parseUnsetDeploymentOverrides
  :: OverrideVisibility
  -> [Text]
  -> IO [DeploymentOverride]
parseUnsetDeploymentOverrides visibility texts =
  coerce <$> parseUnsetOverrides visibility texts

-- | Parses unsetting overrides.
parseUnsetOverrides :: OverrideVisibility -> [Text] -> IO [Override]
parseUnsetOverrides visibility texts =
  for texts $ \key ->
    pure $ Override key "" visibility

-- | Creates pretty-printed text from override.
formatOverride :: Override -> Text
formatOverride o@(Override _ _ vis) =
  overrideToArg o <> case vis of
    Private -> " (" <> pack (show vis) <> ")"
    Public -> mempty

-- | Creates pretty-printed texts from overrides.
formatOverrides :: Overrides -> Text
formatOverrides = T.unlines . fmap formatOverride

-- | Creates a CLI argument from an override.
overrideToArg :: Override -> Text
overrideToArg (Override k v _) = k <> "=" <> v
