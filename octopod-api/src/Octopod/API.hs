-- |
--Module      : Octopod.API
--Description : Backend endpoints.
--
--This module contains backend endpoints.
module Octopod.API
  ( module Octopod.API.WebSocket,
    API,
  )
where

import Servant.API

import Common.Types
import Data.Text (Text)
import Octopod.API.WebSocket

type CaptureName = Capture "name" DeploymentName

type ListEndpoint = Get '[JSON] [DeploymentFullInfo]
type CreateEndpoint =
  ReqBody '[JSON] Deployment :> Post '[JSON] CommandResponse
type ArchiveEndpoint c = c :> Delete '[JSON] CommandResponse
type UpdateEndpoint c =
  c :> ReqBody '[JSON] DeploymentUpdate :> Put '[JSON] CommandResponse
type InfoEndpoint c = c :> "info" :> Get '[JSON] [DeploymentInfo]
type FullInfoEndpoint c = c :> "full_info" :> Get '[JSON] DeploymentFullInfo
type StatusEndpoint c = c :> "status" :> Get '[JSON] CurrentDeploymentStatus
type RestoreEndpoint c =
  c :> "restore" :> Patch '[JSON] CommandResponse

type DefaultDeploymentOverrideKeys =
  "deployment_override_keys" :> Get '[JSON] [Text]
type DefaultDeploymentOverrides =
  "deployment_overrides" :> Get '[JSON] (DefaultConfig 'DeploymentLevel)
type DefaultApplicationOverrideKeys =
  "application_override_keys"
    :> ReqBody '[JSON] (Config 'DeploymentLevel)
    :> Post '[JSON] [Text]
type DefaultApplicationOverrides =
  "application_overrides"
    :> ReqBody '[JSON] (Config 'DeploymentLevel)
    :> Post '[JSON] (DefaultConfig 'ApplicationLevel)

type PingEndpoint = "ping" :> Get '[PlainText] Text
type ProjectNameEndpoint =
  "project_name" :> Get '[JSON] ProjectName

type DeploymentAPI' c =
  "api" :> "v1"
    :> ( DefaultDeploymentOverrideKeys
          :<|> DefaultDeploymentOverrides
          :<|> DefaultApplicationOverrideKeys
          :<|> DefaultApplicationOverrides
          :<|> ( "deployments"
                  :> ( ListEndpoint
                        -- endpoint to get deployment list
                        :<|> CreateEndpoint
                        -- endpoint to create a new deployment
                        :<|> ArchiveEndpoint c
                        -- endpoint to archive existing deployment
                        :<|> UpdateEndpoint c
                        -- endpoint to update exists deployment
                        :<|> InfoEndpoint c
                        -- endpoint to get deployment info
                        :<|> FullInfoEndpoint c
                        -- endpoint to get deployment full info
                        :<|> StatusEndpoint c
                        -- endpoint to get deployment status
                        :<|> RestoreEndpoint c
                        -- endpoint to restore deployment
                     )
                  :<|> PingEndpoint
                  -- endpoint to liveness probe
                  :<|> ProjectNameEndpoint
                  -- endpoint to get project name
               )
       )

-- | API for frontend
type API = DeploymentAPI' CaptureName
