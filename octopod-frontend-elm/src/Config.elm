module Config exposing (..)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)


type alias Config =
  { appUrl : String
  , wsUrl : String
  , appAuth : String
  , k8sDashboardUrlTemplate : String
  }

configDecoder : Decoder Config
configDecoder =
  Decode.succeed Config
    |> required "app_url" string
    |> required "ws_url" string
    |> required "app_auth" string
    |> required "kubernetes_dashboard_url_template" string
