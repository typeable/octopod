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
type StatusEndpoint c = c :> "status" :> Get '[JSON] DeploymentStatus
type CleanupEndpoint c =
  c :> "cleanup" :> Delete '[JSON] CommandResponse
type RestoreEndpoint c =
  c :> "restore" :> Patch '[JSON] CommandResponse

type PingEndpoint =
  "ping" :> GetNoContent '[PlainText] NoContent
type CleanArchiveEndpoint =
  "clean_archive" :> Delete '[JSON] CommandResponse

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
    :<|> PingEndpoint
    :<|> CleanArchiveEndpoint
    )

type API = DeploymentAPI' CaptureName
