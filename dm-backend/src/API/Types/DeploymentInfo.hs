module API.Types.DeploymentInfo where

import Data.Text (Text)
import Deriving.Aeson.Stock
import Options.Generic

import API.Types.Deployment

newtype Action = Action { unAction :: Text }
  deriving (Show, FromJSON, ToJSON)

newtype DeploymentTag = DeploymentTag { unAction :: Text }
  deriving (Show, FromJSON, ToJSON)

data DeploymentLog = DeploymentLog
  { action         :: Action
  , deploymentTag  :: DeploymentTag
  , deploymentEnvs :: [Text] -- FIXME: badly typed, can't be validated
  , exitCode       :: Int
  , createdAt      :: Int
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentLog

data DeploymentInfo = DeploymentInfo
  { deployment :: Deployment
  , logs       :: [DeploymentLog]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentInfo
