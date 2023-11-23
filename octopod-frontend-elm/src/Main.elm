module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Config exposing (Config, Settings, emptySettings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode exposing (Decoder, field, int, map4, string)
import Page
import Page.Deployments as Deployments
import Page.Initialization as Initialization
import Route exposing (Route)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Model
    = Initialization Initialization.Model
    | Deployments Deployments.Model


getNavKey : Model -> Nav.Key
getNavKey model =
    case model of
        Initialization subModel ->
            Initialization.getNavKey subModel

        Deployments subModel ->
            Deployments.getNavKey subModel


getConfig : Model -> Config
getConfig model =
    case model of
        Initialization subModel ->
            Initialization.getConfig subModel

        Deployments subModel ->
            Deployments.getConfig subModel


getSettings : Model -> Settings
getSettings model =
    case model of
        Initialization subModel ->
            Initialization.getSettings subModel

        Deployments subModel ->
            Deployments.getSettings subModel


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    Initialization.init url key
        |> updateWith Initialization InitializationMsg



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | InitializationMsg Initialization.Msg
    | DeploymentsMsg Deployments.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl (getNavKey model) (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( InitializationMsg subMsg, Initialization initialization ) ->
            Initialization.update subMsg initialization
                |> updateWith Initialization InitializationMsg

        ( DeploymentsMsg subMsg, Deployments deployments ) ->
            Deployments.update subMsg deployments
                |> updateWith Deployments DeploymentsMsg

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            Deployments.init (getSettings model) (getConfig model)
                |> updateWith Deployments DeploymentsMsg

        Just Route.Deployments ->
            Deployments.init (getSettings model) (getConfig model)
                |> updateWith Deployments DeploymentsMsg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view (getSettings model).projectName page config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Initialization subModel ->
            viewPage Page.Initialization InitializationMsg (Initialization.view subModel)

        Deployments subModel ->
            viewPage Page.Deployments DeploymentsMsg (Deployments.view subModel)
