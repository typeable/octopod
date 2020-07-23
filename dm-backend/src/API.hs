module API where

import Data.Aeson (Value (..))
import Servant.API
import Servant.API.WebSocketConduit


import Common.API (ApiWSPath)
import Common.Types


type CaptureName = Capture "name" DeploymentName

type ListEndpoint = Get '[JSON] [DeploymentFullInfo]
type CreateEndpoint =
  ReqBody '[JSON] Deployment :> Post '[JSON] CommandResponse
type DeleteEndpoint c = c :> Delete '[JSON] CommandResponse
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
      :<|> DeleteEndpoint c
      :<|> UpdateEndpoint c
      :<|> InfoEndpoint c
      :<|> FullInfoEndpoint c
      :<|> StatusEndpoint c
      :<|> CleanupEndpoint c
      :<|> RestoreEndpoint c
      )
    :<|> GetActionInfoEndpoint
    :<|> PingEndpoint
    :<|> CleanArchiveEndpoint
    :<|> ProjectNameEndpoint
    )

type PowerAPI = DeploymentAPI' CaptureName

type WebSocketAPI = ApiWSPath :> WebSocketSource Value
