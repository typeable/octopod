module API.Types.Deployment where

import Data.Text (Text)
import Deriving.Aeson.Stock
import Options.Generic


newtype DeploymentName = DeploymentName { unDeploymentName :: Text }
  deriving (Show, FromJSON, ToJSON)

newtype DeploymentTag = DeploymentTag { unDeploymentTag :: Text }
  deriving (Show, FromJSON, ToJSON)

data Deployment = Deployment
  { name :: DeploymentName
  , tag  :: DeploymentTag
  , envs :: [Text] -- FIXME: mostly untyped
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake Deployment
