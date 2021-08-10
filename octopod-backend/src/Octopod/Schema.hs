module Octopod.Schema
  ( DeploymentSchema (..),
    deploymentSchema,
    extractDeployment,
    extractDeploymentFullInfo,
    DeploymentLogSchema (..),
    deploymentLogSchema,
    extractDeploymentLog,
  )
where

import Control.Lens
import Data.Generics.Product
import Data.Int
import Data.Time
import GHC.Generics (Generic)
import Rel8
import Types

deploymentSchema :: TableSchema (DeploymentSchema Name)
deploymentSchema =
  TableSchema
    { name = "deployments"
    , schema = Nothing
    , columns =
        DeploymentSchema
          { id_ = "id"
          , name = "name"
          , tag = "tag"
          , appOverrides = "app_overrides"
          , deploymentOverrides = "deployment_overrides"
          , createdAt = "created_at"
          , updatedAt = "updated_at"
          , archivedAt = "archived_at"
          , status = "status"
          , statusUpdatedAt = "status_updated_at"
          , checkedAt = "checked_at"
          , metadata = "links"
          }
    }

data DeploymentSchema f = DeploymentSchema
  { id_ :: Column f DeploymentId
  , name :: Column f DeploymentName
  , tag :: Column f DeploymentTag
  , appOverrides :: Column f (Overrides 'ApplicationLevel)
  , deploymentOverrides :: Column f (Overrides 'DeploymentLevel)
  , createdAt :: Column f UTCTime
  , updatedAt :: Column f UTCTime
  , archivedAt :: Column f (Maybe UTCTime)
  , status :: Column f DeploymentStatus
  , statusUpdatedAt :: Column f UTCTime
  , checkedAt :: Column f UTCTime
  , metadata :: Column f DeploymentMetadata
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

extractDeployment :: DeploymentSchema f -> Deployment' f
extractDeployment = upcast

extractDeploymentFullInfo ::
  -- | Is the deployment locked?
  Bool ->
  DeploymentSchema Result ->
  DeploymentFullInfo
extractDeploymentFullInfo locked d =
  DeploymentFullInfo
    { deployment = extractDeployment d
    , status =
        if locked
          then DeploymentPending $ d ^. #status
          else DeploymentNotPending $ d ^. #status
    , metadata = d ^. #metadata
    , createdAt = d ^. #createdAt
    , updatedAt = d ^. #updatedAt
    }

data DeploymentLogSchema f = DeploymentLogSchema
  { actionId :: Column f ActionId
  , deploymentId :: Column f DeploymentId
  , action :: Column f Action
  , deploymentTag :: Column f DeploymentTag
  , exitCode :: Column f Int64
  , createdAt :: Column f UTCTime
  , archived :: Column f Bool
  , duration :: Column f Duration
  , stdout :: Column f Stdout
  , stderr :: Column f Stderr
  , deploymentAppOverrides :: Column f (Overrides 'ApplicationLevel)
  , deploymentDepOverrides :: Column f (Overrides 'DeploymentLevel)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deploymentLogSchema :: TableSchema (DeploymentLogSchema Name)
deploymentLogSchema =
  TableSchema
    { name = "deployment_logs"
    , schema = Nothing
    , columns =
        DeploymentLogSchema
          { actionId = "id"
          , deploymentId = "deployment_id"
          , action = "action"
          , deploymentTag = "tag"
          , exitCode = "exit_code"
          , createdAt = "created_at"
          , archived = "archived"
          , duration = "duration"
          , stdout = "stdout"
          , stderr = "stderr"
          , deploymentAppOverrides = "app_overrides"
          , deploymentDepOverrides = "deployment_overrides"
          }
    }

extractDeploymentLog :: DeploymentLogSchema f -> DeploymentLog' f
extractDeploymentLog = upcast
