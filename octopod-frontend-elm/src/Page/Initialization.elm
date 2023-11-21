module Page.Initialization exposing (..)

import Browser.Navigation as Nav
import Config exposing (Config)
import Url
import Http
import Html exposing (..)
import Route
import Html.Attributes exposing (class)

type Model
  = Model Nav.Key Url.Url Config
  | ConfigLoading Nav.Key Url.Url
  | FatalError Nav.Key


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
  ( ConfigLoading key url
  , getConfig )

getNavKey : Model -> Nav.Key
getNavKey model =
  case model of
    Model key _ _ -> key
    ConfigLoading key _ -> key
    FatalError key -> key


type Msg
  = ConfigResponse (Result Http.Error Config)

getConfig : Cmd Msg
getConfig =
  Http.get
    { url = "http://localhost:3100/config.json"
    , expect = Http.expectJson ConfigResponse Config.configDecoder
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case (msg, model) of
    (ConfigResponse result, ConfigLoading key url)  ->
      case result of
        Ok config ->
          case Route.fromUrl url of
            Nothing -> (Model key url config, Route.replaceUrl (getNavKey model) Route.Deployments )
            Just route -> (Model key url config, Route.replaceUrl (getNavKey model) route )
        Err _ ->
          (FatalError key, Cmd.none)
    (_, _) -> (FatalError (getNavKey model), Cmd.none)

view : Model -> Html Msg
view model =
  case model of
    Model _ _ config ->
      div []
        [ h2 [] [ text "Got cofig" ]
        , text config.appAuth
        ]
    ConfigLoading _ _ ->     div
        [ class "no-page"
        ]
        [ div
            [ class "no-page__inner"
            ]
            [ div
                [ class "loading loading--enlarged loading--alternate"
                ]
                [ text "Loading..." ]
            ]
        ]

    FatalError _ -> div [] [text "fatal error..."]
