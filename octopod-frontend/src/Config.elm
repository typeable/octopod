module Config exposing (..)

import Browser.Navigation exposing (Key)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)
import Time exposing (Zone, utc)


type AppUrl
    = AppUrl String


unwrapAppUrl : AppUrl -> String
unwrapAppUrl (AppUrl appUrl) =
    appUrl


type AppAuth
    = AppAuth String


unwrapAppAuth : AppAuth -> String
unwrapAppAuth (AppAuth appAuth) =
    appAuth


type alias Config =
    { appUrl : AppUrl
    , wsUrl : String
    , appAuth : AppAuth
    , k8sDashboardUrlTemplate : String
    }


configDecoder : Decoder Config
configDecoder =
    Decode.succeed Config
        |> required "app_url" (Decode.map AppUrl string)
        |> required "ws_url" string
        |> required "app_auth" (Decode.map AppAuth string)
        |> required "kubernetes_dashboard_url_template" string


emptyConfig : Config
emptyConfig =
    Config (AppUrl "") "" (AppAuth "") ""


type alias Settings =
    { navKey : Key
    , projectName : String
    , zone : Zone
    }


emptySettings : Key -> Settings
emptySettings key =
    { navKey = key
    , projectName = ""
    , zone = utc
    }


setProjectName : Settings -> String -> Settings
setProjectName settings projectName =
    { settings | projectName = projectName }


setZone : Settings -> Zone -> Settings
setZone settings zone =
    { settings | zone = zone }
