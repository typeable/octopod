module Common.API where

import Servant.API

import Common.Types


type CaptureName = Capture "name" DeploymentName

type ListEndpoint = Get '[JSON] [DeploymentFullInfo]
type CreateEndpoint =
  ReqBody '[JSON] Deployment :> Post '[JSON] CommandResponse
type GetEndpoint c = c :> Get '[JSON] Deployment
type EditEndpoint c =
  c :> ReqBody '[JSON] EnvPairs :> Patch '[JSON] CommandResponse
type DeleteEndpoint c = c :> Delete '[JSON] CommandResponse
type UpdateEndpoint c =
  c :> ReqBody '[JSON] DeploymentUpdate :> Put '[JSON] CommandResponse
type InfoEndpoint c = c :> "info" :> Get '[JSON] [DeploymentInfo]
type StatusEndpoint c = c :> "status" :> Get '[JSON] CurrentDeploymentStatus
type CleanupEndpoint c =
  c :> "cleanup" :> Delete '[JSON] CommandResponse
type RestoreEndpoint c =
  c :> "restore" :> Patch '[JSON] CommandResponse

type GetActionInfoEndpoint =
  "log" :> Capture "action_id" ActionId :> Get '[JSON] ActionInfo

type PingEndpoint =
  "ping" :> GetNoContent '[PlainText] NoContent
type CleanArchiveEndpoint =
  "clean_archive" :> Delete '[JSON] CommandResponse
type ProjectNameEndpoint =
  "project_name" :> Get '[JSON] ProjectName

-- FIXME: Text as a return type for many endpoints
type DeploymentAPI' c =
  "api" :> "v1" :>
    ( "deployments" :>
      (    ListEndpoint
      :<|> CreateEndpoint
      :<|> GetEndpoint c
      :<|> EditEndpoint c
      :<|> DeleteEndpoint c
      :<|> UpdateEndpoint c
      :<|> InfoEndpoint c
      :<|> StatusEndpoint c
      :<|> CleanupEndpoint c
      :<|> RestoreEndpoint c
      )
    :<|> GetActionInfoEndpoint
    :<|> PingEndpoint
    :<|> CleanArchiveEndpoint
    :<|> ProjectNameEndpoint
    )

type API = DeploymentAPI' CaptureName
