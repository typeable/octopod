module API where

import Servant.API

import Types


type CaptureName = Capture "name" DeploymentName

type ListEndpoint = Get '[JSON] [DeploymentFullInfo]
type CreateEndpoint =
  ReqBody '[JSON] Deployment :> PostNoContent '[PlainText] NoContent
type GetEndpoint c = c :> Get '[JSON] Deployment
type EditEndpoint c =
  c :> ReqBody '[JSON] EnvPairs :> PatchNoContent '[PlainText] NoContent
type DeleteEndpoint c = c :> DeleteNoContent '[PlainText] NoContent
type UpdateEndpoint c =
  c :> ReqBody '[JSON] DeploymentUpdate :> PutNoContent '[PlainText] NoContent
type InfoEndpoint c = c :> "info" :> Get '[JSON] [DeploymentInfo]
type StatusEndpoint c = c :> "status" :> Get '[JSON] DeploymentStatus
type CleanupEndpoint c =
  c :> "cleanup" :> DeleteNoContent '[PlainText] NoContent

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
      )
    :<|> "ping" :> GetNoContent '[PlainText] NoContent
    )

type API = DeploymentAPI' CaptureName
