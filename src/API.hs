module API where

import Servant.API

import Types


type CaptureName = Capture "name" DeploymentName

type ListEndpoint = Get '[JSON] [DeploymentName]
type CreateEndpoint =
  ReqBody '[JSON] Deployment :> PostNoContent '[PlainText] NoContent
type GetEndpoint c = c :> Get '[JSON] Deployment
type EditEndpoint c =
  c :> ReqBody '[JSON] EnvPairs :> PatchNoContent '[PlainText] NoContent
type DestroyEndpoint c = c :> DeleteNoContent '[PlainText] NoContent
type UpdateEndpoint c =
  c :> Capture "tag" DeploymentTag :> PutNoContent '[PlainText] NoContent
type InfoEndpoint c = c :> "info" :> Get '[JSON] [DeploymentInfo]
type StatusEndpoint c = c :> "status" :> Get '[JSON] DeploymentStatus

-- FIXME: Text as a return type for many endpoints
type DeploymentAPI' c =
  "api" :> "v1" :>
    ( "deployments" :>
      (    ListEndpoint
      :<|> CreateEndpoint
      :<|> GetEndpoint c
      :<|> EditEndpoint c
      :<|> DestroyEndpoint c
      :<|> UpdateEndpoint c
      :<|> InfoEndpoint c
      :<|> StatusEndpoint c
      )
    :<|> "ping" :> GetNoContent '[PlainText] NoContent
    )

type API = DeploymentAPI' CaptureName
