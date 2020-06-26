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

newtype DeploymentName = DeploymentName { unDeploymentName :: Text }
  deriving
    (Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq)

newtype DeploymentTag = DeploymentTag { unDeploymentTag :: Text }
  deriving
    ( Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq)

newtype Action = Action { unAction :: Text }
  deriving (Show, FromJSON, ToJSON, IsString)

newtype ArchivedFlag = ArchivedFlag { unArchivedFlag :: Bool }
  deriving (Show, FromJSON, ToJSON)

newtype Duration = Duration { unDuration :: Int }
  deriving (Show, FromJSON, ToJSON)

newtype ProjectName = ProjectName { uProjectName :: Text }
  deriving (Show, FromJSON, ToJSON)

data Deployment = Deployment
  { name :: DeploymentName
  , tag  :: DeploymentTag
  , envs :: EnvPairs
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake Deployment

data DeploymentLog = DeploymentLog
  { action         :: Action
  , deploymentTag  :: DeploymentTag
  , deploymentEnvs :: EnvPairs
  , exitCode       :: Int
  , duration       :: Duration
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
  deriving (Generic, Show, Eq)
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
