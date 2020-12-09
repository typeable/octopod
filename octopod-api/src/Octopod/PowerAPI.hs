{-|
Module      : Octopod.PowerAPI
Description : Backend endpoints.

This module contains backend endpoints.
-}


module Octopod.PowerAPI
  ( module Octopod.API.WebSocket
  , PowerAPI
  ) where

import           Servant.API

import           Common.Types
import           Octopod.API.WebSocket

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
type CleanupEndpoint c =
  c :> "cleanup" :> Delete '[JSON] CommandResponse
type RestoreEndpoint c =
  c :> "restore" :> Patch '[JSON] CommandResponse

type GetActionInfoEndpoint =
  "log" :> Capture "action_id" ActionId :> Get '[JSON] ActionInfo

type CleanArchiveEndpoint =
  "clean_archive" :> Delete '[JSON] CommandResponse

type DeploymentAPI' c =
  "api" :> "v1" :>
    ( "deployments" :>
      (    ListEndpoint
      -- endpoint to get deployment list
      :<|> CreateEndpoint
      -- endpoint to create a new deployment
      :<|> ArchiveEndpoint c
      -- endpoint to archive an existing deployment
      :<|> UpdateEndpoint c
      -- endpoint to update an existing deployment
      :<|> InfoEndpoint c
      -- endpoint to get deployment info
      :<|> FullInfoEndpoint c
      -- endpoint to get full deployment info
      :<|> StatusEndpoint c
      -- endpoint to get deployment status
      :<|> CleanupEndpoint c
      -- endpoint to clean up resources of an archived deployment
      :<|> RestoreEndpoint c
      -- endpoint to restore an archived deployment
      )
    :<|> GetActionInfoEndpoint
    -- endpoint to get action logs
    :<|> CleanArchiveEndpoint
    -- endpoint to clean up resources of all archived deployments
    -- according to the archive retention policy
    )

-- | API for the octo CLI
type PowerAPI = DeploymentAPI' CaptureName
