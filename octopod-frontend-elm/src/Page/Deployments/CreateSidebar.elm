module Page.Deployments.CreateSidebar exposing (..)

import Api
import Api.Endpoint exposing (..)
import Config exposing (Config)
import Deployments exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Deployments.Overrides as Overrides
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Time exposing (Month(..))
import Tree exposing (Tree, getLabel, mergeTrees, mergeTreesByWithSort, mergeTreesWithSort)


overridesToDict : List (List String) -> Dict Int Overrides.OverrideData
overridesToDict raw =
    let
        f x =
            case x of
                [ p, v ] ->
                    Just ( p, v )

                _ ->
                    Nothing

        g i ( p, v ) =
            ( i, Overrides.OverrideData p v )
    in
    Dict.fromList <| List.indexedMap g <| List.filterMap f raw


dictToOverrides : Dict Int Overrides.OverrideData -> List (List String)
dictToOverrides =
    let
        f ( _, b ) =
            [ b.path, b.value ]
    in
    Dict.toList
        >> List.map f


type alias Model =
    { appOverrides : Overrides.Model
    , deploymentOverrides : Overrides.Model
    , name : String
    , visibility : Bool
    , config : Config
    }


show : Model -> Model
show model =
    { model | visibility = True }


hide : Model -> Model
hide model =
    { model | visibility = False }


init : Config -> Bool -> Model
init config visibility =
    { appOverrides = Overrides.init "App configuration"
    , deploymentOverrides = Overrides.init "Deployment configuration"
    , name = ""
    , visibility = visibility
    , config = config
    }


overridesToTrees : List (List String) -> List (Tree ( String, Maybe OverrideValue ))
overridesToTrees respRaw =
    let
        f x =
            case x of
                [ p, v ] ->
                    Just (Tree.pathToTree (String.split "." p) ( v, Nothing ))

                _ ->
                    Nothing
    in
    mergeTrees (List.filterMap f respRaw)


type Msg
    = DeploymentOverrideKeysResponse (WebData (List String))
    | DeploymentOverridesResponse (WebData (List (List String)))
    | AppOverrideKeysResponse (WebData (List String))
    | AppOverridesResponse (WebData (List (List String)))
    | Close
    | Save
    | NameInput String
    | AppOverridesMsg Overrides.Msg
    | DeploymentOverridesMsg Overrides.Msg


reqDeploymentOverrideKeys : Config -> Cmd Msg
reqDeploymentOverrideKeys config =
    Api.get config
        deploymentOverrideKeys
        (Decode.list Decode.string)
        (RemoteData.fromResult >> DeploymentOverrideKeysResponse)


reqDeploymentOverrides : Config -> Cmd Msg
reqDeploymentOverrides config =
    Api.get config
        deploymentOverrides
        (Decode.list (Decode.list Decode.string))
        (RemoteData.fromResult >> DeploymentOverridesResponse)


reqAppOverrideKeys : Config -> List (List String) -> Cmd Msg
reqAppOverrideKeys config body =
    Api.post config
        appOverrideKeys
        (Http.jsonBody (Encode.list (Encode.list Encode.string) body))
        (Decode.list Decode.string)
        (RemoteData.fromResult >> AppOverrideKeysResponse)


reqAppOverrides : Config -> List (List String) -> Cmd Msg
reqAppOverrides config body =
    Api.post config
        appOverrides
        (Http.jsonBody (Encode.list (Encode.list Encode.string) body))
        (Decode.list (Decode.list Decode.string))
        (RemoteData.fromResult >> AppOverridesResponse)


initReqs : Config -> Cmd Msg
initReqs config =
    Cmd.batch
        [ reqDeploymentOverrideKeys config
        , reqDeploymentOverrides config
        ]


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
        DeploymentOverrideKeysResponse keys ->
            ( { model | deploymentOverrides = Overrides.setKeys keys model.deploymentOverrides }, Cmd.none )

        DeploymentOverridesResponse overrides ->
            ( { model
                | deploymentOverrides = Overrides.setDefaultOverrides (RemoteData.map overridesToDict overrides) model.deploymentOverrides
              }
            , Cmd.batch
                [ reqAppOverrideKeys model.config (RemoteData.withDefault [] overrides)
                , reqAppOverrides model.config (RemoteData.withDefault [] overrides)
                ]
            )

        AppOverrideKeysResponse keys ->
            ( { model | appOverrides = Overrides.setKeys keys model.appOverrides }, Cmd.none )

        AppOverridesResponse overrides ->
            ( { model
                | appOverrides = Overrides.setDefaultOverrides (RemoteData.map overridesToDict overrides) model.appOverrides
              }
            , Cmd.none
            )

        Close ->
            ( { model | visibility = False }, Cmd.none )

        Save ->
            ( model, Cmd.none )

        NameInput name ->
            ( { model | name = name }, Cmd.none )

        AppOverridesMsg subMsg ->
            Overrides.update subMsg model.appOverrides
                |> updateWith (\appOverrides -> { model | appOverrides = appOverrides }) AppOverridesMsg

        DeploymentOverridesMsg subMsg ->
            let
                ( model_, cmd_ ) =
                    Overrides.update subMsg model.deploymentOverrides
                        |> updateWith (\deploymentOverrides -> { model | deploymentOverrides = deploymentOverrides }) DeploymentOverridesMsg
            in
            ( { model_ | appOverrides = Overrides.init "App configuration" }
            , Cmd.batch
                [ reqAppOverrideKeys model_.config ((Overrides.getEditOverrides >> dictToOverrides) model_.deploymentOverrides)
                , reqAppOverrides model_.config ((Overrides.getEditOverrides >> dictToOverrides) model_.deploymentOverrides)
                , cmd_
                ]
            )


view : Model -> List (Html Msg)
view model =
    if model.visibility then
        [ divClass "popup popup--visible"
            [ Html.div [ Attr.attribute "aria-hidden" "true", Attr.class "popup__overlay" ] []
            , divClass "popup__body"
                [ sidebarHeader
                , sidebarContent model
                ]
            ]
        ]

    else
        []


sidebarHeader : Html Msg
sidebarHeader =
    divClass "popup__head"
        [ buttonClass "popup__close"
            Close
            []
        , h2Class "popup__project"
            [ text "Create new deployment" ]
        , divClass "popup__operations"
            [ buttonClass "button button--save popup__action"
                Close
                [ text "Save" ]
            ]
        , divClass "popup__menu drop drop--actions"
            []
        ]


sidebarContent : Model -> Html Msg
sidebarContent model =
    divClass "popup__content"
        [ divClass "deployment"
            [ nameSection model
            , Html.map DeploymentOverridesMsg (Overrides.view model.deploymentOverrides)
            , Html.map AppOverridesMsg (Overrides.view model.appOverrides)
            ]
        ]


nameSection : Model -> Html Msg
nameSection _ =
    divClass "deployment__section"
        [ h3Class "deployment__sub-heading" [ text "Name" ]
        , divClass "deployment__widget"
            [ divClass "input"
                [ input [ Attr.class "input__widget tag", Attr.type_ "text", Attr.placeholder "Name", onInput NameInput ]
                    []
                ]
            ]
        ]
