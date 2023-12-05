module Page.Sidebar.CreateUpdate exposing (..)

import Api
import Api.Endpoint exposing (..)
import Config exposing (Config)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (onInput)
import Html.Overrides as Overrides
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import RemoteData exposing (RemoteData(..))
import Time exposing (Month(..))
import Types.Deployment exposing (..)
import Types.Override exposing (..)
import Types.OverrideWithDefault exposing (OverrideWithDefault, defaultOverrideEncoder, defaultOverridesDecode)


type alias Model =
    { appOverrides : Overrides.Model
    , deploymentOverrides : Overrides.Model
    , name : DeploymentName
    , visibility : Bool
    , config : Config
    , saveResp : Api.WebData ()
    , nameEdited : Bool
    , deployment : Maybe Deployment
    }


init : Config -> Bool -> Model
init config visibility =
    { appOverrides = Overrides.init "App configuration" Overrides.Write
    , deploymentOverrides = Overrides.init "Deployment configuration" Overrides.Write
    , name = DeploymentName ""
    , visibility = visibility
    , config = config
    , saveResp = NotAsked
    , nameEdited = False
    , deployment = Nothing
    }


initWithDeployment : Config -> Bool -> Deployment -> Model
initWithDeployment config visibility deployment =
    { appOverrides = Overrides.init "App configuration" Overrides.Write
    , deploymentOverrides = Overrides.init "Deployment configuration" Overrides.Write
    , name = deployment.deployment.name
    , visibility = visibility
    , config = config
    , saveResp = NotAsked
    , nameEdited = False
    , deployment = Just deployment
    }


type Msg
    = DeploymentOverrideKeysResponse (Api.WebData (List OverrideName))
    | DeploymentOverridesResponse (Api.WebData (List OverrideWithDefault))
    | AppOverrideKeysResponse (Api.WebData (List OverrideName))
    | AppOverridesResponse (Api.WebData (List OverrideWithDefault))
    | Close
    | Save
    | NameInput String
    | AppOverridesMsg Overrides.Msg
    | DeploymentOverridesMsg Overrides.Msg
    | SaveDeploymentResponse (Api.WebData ())


reqDeploymentOverrideKeys : Config -> Cmd Msg
reqDeploymentOverrideKeys config =
    Api.get config
        deploymentOverrideKeys
        (Decode.list overrideNameDecoder)
        (RemoteData.fromResult >> DeploymentOverrideKeysResponse)


reqDeploymentOverrides : Config -> Cmd Msg
reqDeploymentOverrides config =
    Api.get config
        deploymentOverrides
        (Decode.list defaultOverridesDecode)
        (RemoteData.fromResult >> DeploymentOverridesResponse)


reqAppOverrideKeys : Config -> List OverrideWithDefault -> Cmd Msg
reqAppOverrideKeys config body =
    Api.post config
        appOverrideKeys
        (Http.jsonBody (Encode.list defaultOverrideEncoder body))
        (Decode.list overrideNameDecoder)
        (RemoteData.fromResult >> AppOverrideKeysResponse)


reqAppOverrides : Config -> List OverrideWithDefault -> Cmd Msg
reqAppOverrides config body =
    Api.post config
        appOverrides
        (Http.jsonBody (Encode.list defaultOverrideEncoder body))
        (Decode.list defaultOverridesDecode)
        (RemoteData.fromResult >> AppOverridesResponse)


reqSaveDeployment : Config -> Info -> Cmd Msg
reqSaveDeployment config body =
    Api.post config
        saveDeployment
        (Http.jsonBody (infoEncode body))
        (Decode.succeed ())
        (RemoteData.fromResult >> SaveDeploymentResponse)


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
                | deploymentOverrides =
                    case model.deployment of
                        Just deployment ->
                            Overrides.setDefaultAndLoadedOverrides overrides (Success deployment.deployment.deploymentOverrides) model.deploymentOverrides

                        Nothing ->
                            Overrides.setOverrideWithDefaults overrides model.deploymentOverrides
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
                | appOverrides =
                    case model.deployment of
                        Just deployment ->
                            Overrides.setDefaultAndLoadedOverrides overrides (Success deployment.deployment.appOverrides) model.appOverrides

                        Nothing ->
                            Overrides.setOverrideWithDefaults overrides model.appOverrides
              }
            , Cmd.none
            )

        Close ->
            ( { model | visibility = False }, Cmd.none )

        NameInput name ->
            ( { model | name = DeploymentName name, nameEdited = True }, Cmd.none )

        AppOverridesMsg subMsg ->
            Overrides.update subMsg model.appOverrides
                |> updateWith (\appOverrides -> { model | appOverrides = appOverrides }) AppOverridesMsg

        DeploymentOverridesMsg subMsg ->
            let
                ( model_, cmd_ ) =
                    Overrides.update subMsg model.deploymentOverrides
                        |> updateWith (\deploymentOverrides -> { model | deploymentOverrides = deploymentOverrides }) DeploymentOverridesMsg
            in
            if Overrides.dataChanged subMsg then
                ( { model_ | appOverrides = Overrides.init "App configuration" Overrides.Write }
                , Cmd.batch
                    [ reqAppOverrideKeys model_.config (Overrides.getFullOverrides model_.deploymentOverrides)
                    , reqAppOverrides model_.config (Overrides.getFullOverrides model_.deploymentOverrides)
                    , cmd_
                    ]
                )

            else
                ( model_, cmd_ )

        Save ->
            let
                deploymentOverrides =
                    Overrides.getEditedOverrides model.deploymentOverrides

                appOverrides =
                    Overrides.getEditedOverrides model.appOverrides

                info =
                    { name = model.name
                    , deploymentOverrides = deploymentOverrides
                    , appOverrides = appOverrides
                    }
            in
            ( { model | saveResp = Loading }, reqSaveDeployment model.config info )

        SaveDeploymentResponse resp ->
            ( { model | saveResp = resp }, Cmd.none )


view : Model -> List (Html Msg)
view model =
    if model.visibility then
        [ divClass "popup popup--visible"
            [ Html.div [ Attr.attribute "aria-hidden" "true", Attr.class "popup__overlay" ] []
            , divClass "popup__body"
                [ sidebarHeader model
                , sidebarContent model
                ]
            ]
        ]

    else
        []


sidebarHeader : Model -> Html Msg
sidebarHeader model =
    let
        button =
            case ( model.saveResp, hasEmptyValues model ) of
                ( Loading, _ ) ->
                    Html.button
                        [ Attr.class "popup__action button button--save-loading"
                        , Attr.disabled True
                        ]
                        [ text "Save" ]

                ( Success _, _ ) ->
                    div [] []

                ( _, True ) ->
                    Html.button
                        [ Attr.class "button--disabled button button--save popup__action"
                        , Attr.disabled True
                        ]
                        [ text "Save" ]

                ( _, False ) ->
                    buttonClass "button button--save popup__action" Save [ text "Save" ]
    in
    divClass "popup__head"
        [ buttonClass "popup__close"
            Close
            []
        , h2Class "popup__project"
            [ text "Create new deployment" ]
        , divClass "popup__operations"
            [ button
            ]
        , divClass "popup__menu drop drop--actions"
            []
        ]


sidebarContent : Model -> Html Msg
sidebarContent model =
    let
        errorText =
            case model.saveResp of
                RemoteData.Failure (Api.BadStatus _ msg) ->
                    Just msg

                RemoteData.Failure _ ->
                    Just "Something went wrong"

                _ ->
                    Nothing

        errorViewer msg =
            [ divClass "deployment__output notification notification--danger"
                [ text msg ]
            ]

        errorView =
            errorText |> Maybe.map errorViewer |> Maybe.withDefault []
    in
    divClass "popup__content"
        [ divClass "deployment"
            (errorView
                ++ [ nameSection model
                   , Html.map DeploymentOverridesMsg (Overrides.view model.deploymentOverrides)
                   , Html.map AppOverridesMsg (Overrides.view model.appOverrides)
                   ]
            )
        ]


nameSection : Model -> Html Msg
nameSection model =
    let
        nameError =
            model.nameEdited && hasEmptyName model

        inputClass =
            if nameError then
                "input input--error"

            else
                "input"

        errorMessage =
            if nameError then
                [ divClass "input__output"
                    [ text "Deployment name length should be longer than 2 characters and under 17 characters and begin with a letter." ]
                ]

            else
                []
    in
    divClass "deployment__section"
        [ h3Class "deployment__sub-heading" [ text "Name" ]
        , divClass "deployment__widget"
            (divClass inputClass
                [ input
                    [ Attr.class "input__widget tag"
                    , Attr.type_ "text"
                    , Attr.placeholder "Name"
                    , Attr.value (unDeploymentName model.name)
                    , onInput NameInput
                    ]
                    []
                ]
                :: errorMessage
            )
        ]


hasEmptyName : Model -> Bool
hasEmptyName model =
    let
        nameLength =
            String.length (unDeploymentName model.name)
    in
    nameLength < 2 || nameLength > 17


hasEmptyValues : Model -> Bool
hasEmptyValues model =
    Overrides.hasEmptyValues model.appOverrides
        || Overrides.hasEmptyValues model.deploymentOverrides
        || hasEmptyName model
