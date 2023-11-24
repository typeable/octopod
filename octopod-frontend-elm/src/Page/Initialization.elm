port module Page.Initialization exposing (..)

import Api
import Api.Endpoint as Endpoint exposing (configJson)
import Browser.Navigation as Nav
import Config exposing (Config, Settings, configDecoder, emptyConfig, emptySettings, setProjectName, setZone)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (bClass, divClass)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Route
import Task
import Time
import Url


type alias Model =
    { settings : Settings
    , url : Url.Url
    , config : WebData Config
    , projectName : WebData String
    }


port getWebsocketAddress : String -> Cmd msg


init : Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init url key =
    ( { settings = emptySettings key
      , url = url
      , config = RemoteData.Loading
      , projectName = RemoteData.Loading
      }
    , Task.perform AdjustTimeZone Time.here
    )


getNavKey : Model -> Nav.Key
getNavKey model =
    model.settings.navKey


getConfig : Model -> Config
getConfig model =
    case model.config of
        Success cfg ->
            cfg

        _ ->
            emptyConfig


getSettings : Model -> Settings
getSettings model =
    model.settings


type Msg
    = ConfigResponse (WebData Config)
    | ProjectNameResponse (WebData String)
    | AdjustTimeZone Time.Zone


reqConfig : Cmd Msg
reqConfig =
    Endpoint.request
        { method = "GET"
        , url = configJson
        , expect = Http.expectJson (RemoteData.fromResult >> ConfigResponse) configDecoder
        , headers = []
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


reqProjectName : Config -> Cmd Msg
reqProjectName cfg =
    Api.get cfg Endpoint.projectName Decode.string (RemoteData.fromResult >> ProjectNameResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ConfigResponse webData ->
            case webData of
                Success config ->
                    ( { model | config = Success config }
                    , Cmd.batch
                        [ reqProjectName config
                        , getWebsocketAddress (config.wsUrl ++ "/event")
                        ]
                    )

                a ->
                    ( { model | config = a }, Cmd.none )

        ProjectNameResponse webData ->
            case webData of
                Success projectName ->
                    case Route.fromUrl model.url of
                        Nothing ->
                            ( { model | settings = setProjectName model.settings projectName, projectName = Success projectName }
                            , Route.replaceUrl model.settings.navKey Route.Deployments
                            )

                        Just route ->
                            ( { model | settings = setProjectName model.settings projectName, projectName = Success projectName }
                            , Route.replaceUrl model.settings.navKey route
                            )

                a ->
                    ( { model | projectName = a }, Cmd.none )

        AdjustTimeZone zone ->
            ( { model | settings = setZone model.settings zone }
            , reqConfig
            )


view : Model -> { title : String, content : Html Msg }
view model =
    case ( model.config, model.projectName ) of
        ( Failure _, _ ) ->
            { title = "Octopod", content = failureView }

        ( _, Failure _ ) ->
            { title = "Octopod", content = failureView }

        ( Loading, _ ) ->
            { title = "Octopod", content = loadingView }

        ( _, Loading ) ->
            { title = "Octopod", content = loadingView }

        _ ->
            { title = "Octopod", content = div [] [] }


failureView : Html Msg
failureView =
    divClass "no-page"
        [ divClass "no-page__inner"
            [ divClass "null null--data"
                [ bClass "null__heading"
                    [ text "Cannot retrieve the data" ]
                , divClass "null__message"
                    [ text "Try to reload page" ]
                ]
            ]
        ]


loadingView : Html Msg
loadingView =
    divClass "no-page"
        [ divClass "no-page__inner"
            [ divClass "loading loading--enlarged loading--alternate"
                [ text "Loading..."
                ]
            ]
        ]
