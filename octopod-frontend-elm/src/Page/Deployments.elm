port module Page.Deployments exposing (..)

import Api
import Api.Endpoint exposing (..)
import Browser.Navigation as Nav
import Config exposing (Config, Settings)
import Deployments exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (onInput)
import Page.Deployments.Table as Table
import RemoteData exposing (RemoteData(..), WebData)


type alias Model =
    { settings : Settings
    , config : Config
    , deployments : WebData Deployments
    , activeTable : Table.Model
    , archivedTable : Table.Model
    , search : String
    , showArchived : Bool
    }


type Sort
    = Asc Column
    | Desc Column


type Column
    = Name
    | Updated
    | Created


init : Settings -> Config -> ( Model, Cmd Msg )
init settings config =
    ( { settings = settings
      , config = config
      , deployments = Loading
      , activeTable = Table.init config settings.zone Table.ActiveTable
      , archivedTable = Table.init config settings.zone Table.ArchivedTable
      , search = ""
      , showArchived = False
      }
    , reqConfig config
    )


getNavKey : Model -> Nav.Key
getNavKey model =
    model.settings.navKey


getConfig : Model -> Config
getConfig model =
    model.config


getSettings : Model -> Settings
getSettings model =
    model.settings


type Msg
    = DeploymentsResponse (WebData Deployments)
    | SearchInput String
    | ActiveTableMsg Table.Msg
    | ArchivedTableMsg Table.Msg
    | ToggleArchived
    | WSUpdate String


port messageReceiver : (String -> msg) -> Sub msg


reqConfig : Config -> Cmd Msg
reqConfig cfg =
    Api.get cfg deployments deploymentsDecoder (RemoteData.fromResult >> DeploymentsResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    let
        updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
        updateWith toModel toMsg ( subModel, subCmd ) =
            ( toModel subModel
            , Cmd.map toMsg subCmd
            )
    in
    case cmd of
        DeploymentsResponse deployments ->
            ( { model | deployments = deployments }, Cmd.none )

        SearchInput search ->
            ( { model | search = search }, Cmd.none )

        ActiveTableMsg subMsg ->
            Table.update subMsg model.activeTable
                |> updateWith (\table -> { model | activeTable = table }) ActiveTableMsg

        ArchivedTableMsg subMsg ->
            Table.update subMsg model.archivedTable
                |> updateWith (\table -> { model | archivedTable = table }) ArchivedTableMsg

        ToggleArchived ->
            ( { model | showArchived = not model.showArchived }, Cmd.none )

        WSUpdate _ ->
            ( model, reqConfig model.config )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ActiveTableMsg (Table.subscriptions model.activeTable)
        , Sub.map ArchivedTableMsg (Table.subscriptions model.archivedTable)
        , messageReceiver WSUpdate
        ]


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Deployments"
    , content = pageView model
    }


pageView : Model -> Html Msg
pageView model =
    let
        activeDeploymets =
            RemoteData.map (List.filter (isDeploymentArchived >> not)) model.deployments

        archivedDeploymets =
            RemoteData.map (List.filter isDeploymentArchived) model.deployments

        archivedCount =
            RemoteData.unwrap 0 List.length archivedDeploymets
    in
    pageWrapper
        [ pageHeaderView model
        , pageBodyWrapper <|
            dataPrimaryView model activeDeploymets
                :: (if archivedCount == 0 then
                        []

                    else
                        [ toggleButtonView model
                        , dataArchivedView model archivedDeploymets
                        ]
                   )
        ]


pageWrapper : List (Html Msg) -> Html Msg
pageWrapper body =
    divClass "page" [ divClass "page__wrap container" body ]


pageHeaderTimeUpdateView : Model -> Html Msg
pageHeaderTimeUpdateView _ =
    divClass "page__note" <| [ text "Updated 5 mins ago" ]


pageHeaderSearchView : Model -> Html Msg
pageHeaderSearchView _ =
    divClass "page__action page__action--search input input--search input--has-clear-type"
        [ input [ Attr.class "input__widget", Attr.type_ "text", Attr.placeholder "Search for deployments", onInput SearchInput ]
            []
        ]


pageHeaderView : Model -> Html Msg
pageHeaderView model =
    divClass "page__head"
        [ h1Class "page__heading title" <| [ text "All deployments" ]
        , pageHeaderTimeUpdateView model
        , pageHeaderSearchView model
        , Html.a [ Attr.class "page__action button button--add popup-handler" ] [ text "New deployment" ]
        ]


pageBodyWrapper : List (Html Msg) -> Html Msg
pageBodyWrapper body =
    divClass "page__body" [ divClass "body" body ]


dataPrimaryView : Model -> WebData Deployments -> Html Msg
dataPrimaryView model activeDeploymets =
    divClass "data__primary"
        [ Html.map ActiveTableMsg (Table.view model.activeTable activeDeploymets model.search) ]


toggleButtonView : Model -> Html Msg
toggleButtonView model =
    if model.showArchived then
        buttonClass "data__show-archive expander expander--stand-alone expander--open"
            ToggleArchived
            [ text <| "Hide Archived deployments" ]

    else
        buttonClass "data__show-archive expander expander--stand-alone"
            ToggleArchived
            [ text <| "Show Archived deployments" ]


dataArchivedView : Model -> WebData Deployments -> Html Msg
dataArchivedView model archivedDeploymets =
    let
        dataClass =
            if model.showArchived then
                "data__archive data__archive--open"

            else
                "data__archive"
    in
    divClass dataClass
        [ Html.map ArchivedTableMsg (Table.view model.archivedTable archivedDeploymets model.search) ]
