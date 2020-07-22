module Common.Types where

import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.String
import Data.Text as T
import Data.Traversable
import Deriving.Aeson.Stock
import Web.HttpApiData


type EnvPair = (Text, Text)

type EnvPairs = [EnvPair]

data Override = Override
  { overrideKey           :: Text
  , overrideValue         :: Text
  , overrideVisibility    :: OverrideVisibility
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake Override

type Overrides = [Override]

data OverrideScope
  = App
  | Staging
  deriving (Generic, Show, Read, Eq)
  deriving (FromJSON, ToJSON) via Snake OverrideScope

data OverrideVisibility
  = Private
  | Public
  deriving (Generic, Show, Read, Eq)
  deriving (FromJSON, ToJSON) via Snake OverrideVisibility

newtype ApplicationOverride =
  ApplicationOverride { unApplicationOverride :: Override }
  deriving (Show, Eq, FromJSON, ToJSON)

type ApplicationOverrides = [ApplicationOverride]

newtype StagingOverride = StagingOverride { unStagingOverride :: Override }
  deriving (Show, Eq, FromJSON, ToJSON)

type StagingOverrides = [StagingOverride]

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

data DeploymentStatus
  = Running
  | Failure
  | CreatePending
  | UpdatePending
  | DeletePending
  deriving (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentStatus

data Deployment = Deployment
  { name             :: DeploymentName
  , tag              :: DeploymentTag
  , appOverrides     :: ApplicationOverrides
  , stagingOverrides :: StagingOverrides
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake Deployment

data DeploymentLog = DeploymentLog
  { actionId                   :: ActionId
  , action                     :: Action
  , deploymentTag              :: DeploymentTag
  , deploymentAppOverrides     :: ApplicationOverrides
  , deploymentStagingOverrides :: StagingOverrides
  , exitCode                   :: Int
  , duration                   :: Duration
  , createdAt                  :: Int
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
  , status     :: DeploymentStatus
  , archived   :: Bool
  , createdAt  :: Int
  , updatedAt  :: Int
  , urls       :: [(Text, Text)]
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentFullInfo

data DeploymentUpdate = DeploymentUpdate
  { newTag              :: DeploymentTag
  , newAppOverrides     :: ApplicationOverrides
  , oldAppOverrides     :: ApplicationOverrides
  , newStagingOverrides :: StagingOverrides
  , oldStagingOverrides :: StagingOverrides
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
    , tagField :: [Text] }
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
  , stderr :: Text }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake ActionInfo
