module Page.Sidebar.CreateUpdate exposing (..)

import Api
import Api.Endpoint exposing (..)
import Config exposing (Config)
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


type Mode
    = Create
    | Update


type alias Model =
    { appOverrides : Api.WebData Overrides.Model
    , deploymentOverrides : Api.WebData Overrides.Model
    , name : DeploymentName
    , visibility : Bool
    , config : Config
    , saveResp : Api.WebData ()
    , nameEdited : Bool
    , mode : Mode
    , deployment : Api.WebData Deployment
    , appKeys : Api.WebData (List OverrideName)
    , appDefaults : Api.WebData (List OverrideWithDefault)
    , deploymentKeys : Api.WebData (List OverrideName)
    , deploymentDefaults : Api.WebData (List OverrideWithDefault)
    }


init : Config -> Mode -> Bool -> Model
init config mode visibility =
    { appOverrides = Loading
    , deploymentOverrides = Loading
    , name = DeploymentName ""
    , visibility = visibility
    , config = config
    , saveResp = NotAsked
    , nameEdited = False
    , deployment = NotAsked
    , mode = mode
    , appKeys = Loading
    , appDefaults = Loading
    , deploymentKeys = Loading
    , deploymentDefaults = Loading
    }


initWithDeploymentName : Config -> Mode -> Bool -> DeploymentName -> Model
initWithDeploymentName config mode visibility deploymentName =
    { appOverrides = Loading
    , deploymentOverrides = Loading
    , name = deploymentName
    , visibility = visibility
    , config = config
    , saveResp = NotAsked
    , nameEdited = False
    , deployment = Loading
    , mode = mode
    , appKeys = Loading
    , appDefaults = Loading
    , deploymentKeys = Loading
    , deploymentDefaults = Loading
    }


type Msg
    = DeploymentOverrideKeysResponse (Api.WebData (List OverrideName))
    | DeploymentOverridesResponse (Api.WebData (List OverrideWithDefault))
    | AppOverrideKeysResponse (Api.WebData (List OverrideName))
    | AppOverridesResponse (Api.WebData (List OverrideWithDefault))
    | DeploymentFullInfoResponse (Api.WebData Deployment)
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


reqDeployment : DeploymentName -> Config -> Cmd Msg
reqDeployment deploymentName cfg =
    Api.get cfg (deploymentFullInfo deploymentName) deploymentDecoder (RemoteData.fromResult >> DeploymentFullInfoResponse)


initCreate : Config -> Cmd Msg
initCreate config =
    Cmd.batch
        [ reqDeploymentOverrideKeys config
        , reqDeploymentOverrides config
        ]


initUpdate : Config -> DeploymentName -> Cmd Msg
initUpdate config deploymentName =
    reqDeployment deploymentName config


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    let
        updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
        updateWith toModel toMsg ( subModel, subCmd ) =
            ( toModel subModel
            , Cmd.map toMsg subCmd
            )

        initOverrides name edits defaults keys =
            Overrides.init defaults edits keys Overrides.Write name

        ( appEdits, deployEdits ) =
            case model.deployment of
                Success deployment ->
                    ( deployment.deployment.appOverrides, deployment.deployment.deploymentOverrides )

                _ ->
                    ( [], [] )
    in
    case cmd of
        DeploymentOverrideKeysResponse keys ->
            ( { model
                | deploymentKeys = keys
                , deploymentOverrides =
                    RemoteData.map2
                        (initOverrides "Deployment configuration" deployEdits)
                        model.deploymentDefaults
                        keys
              }
            , Cmd.none
            )

        DeploymentOverridesResponse overrides ->
            ( { model
                | deploymentDefaults = overrides
                , deploymentOverrides =
                    RemoteData.map2
                        (initOverrides "Deployment configuration" deployEdits)
                        overrides
                        model.deploymentKeys
              }
            , case overrides of
                Success defaults ->
                    Cmd.batch
                        [ reqAppOverrideKeys model.config defaults
                        , reqAppOverrides model.config defaults
                        ]

                _ ->
                    Cmd.none
            )

        AppOverrideKeysResponse keys ->
            ( { model
                | appKeys = keys
                , appOverrides =
                    RemoteData.map2
                        (initOverrides "App configuration" appEdits)
                        model.appDefaults
                        keys
              }
            , Cmd.none
            )

        AppOverridesResponse overrides ->
            ( { model
                | appDefaults = overrides
                , appOverrides =
                    RemoteData.map2
                        (initOverrides "App configuration" appEdits)
                        overrides
                        model.appKeys
              }
            , Cmd.none
            )

        Close ->
            ( { model | visibility = False }, Cmd.none )

        NameInput name ->
            ( { model | name = DeploymentName name, nameEdited = True }, Cmd.none )

        AppOverridesMsg subMsg ->
            case model.appOverrides of
                Success appOverrides ->
                    Overrides.update subMsg appOverrides
                        |> updateWith (\updated -> { model | appOverrides = Success updated }) AppOverridesMsg

                _ ->
                    ( model, Cmd.none )

        DeploymentOverridesMsg subMsg ->
            case model.deploymentOverrides of
                Success deploymentOverrides ->
                    let
                        ( model_, cmd_ ) =
                            Overrides.update subMsg deploymentOverrides
                                |> updateWith
                                    (\deploymentOverrides_ ->
                                        { model | deploymentOverrides = Success deploymentOverrides_ }
                                    )
                                    DeploymentOverridesMsg
                    in
                    case ( Overrides.dataChanged subMsg, model_.deploymentOverrides ) of
                        ( True, Success deploymentOverrides_ ) ->
                            ( { model_ | appOverrides = Loading }
                            , Cmd.batch
                                [ reqAppOverrideKeys model_.config (Overrides.getFullOverrides deploymentOverrides_)
                                , reqAppOverrides model_.config (Overrides.getFullOverrides deploymentOverrides_)
                                , cmd_
                                ]
                            )

                        _ ->
                            ( model_, cmd_ )

                _ ->
                    ( model, Cmd.none )

        Save ->
            case ( model.deploymentOverrides, model.appOverrides ) of
                ( Success deploymentOverrides, Success appOverrides ) ->
                    let
                        deploymentOverridesData =
                            Overrides.getEditedOverrides deploymentOverrides

                        appOverridesData =
                            Overrides.getEditedOverrides appOverrides

                        info =
                            { name = model.name
                            , deploymentOverrides = deploymentOverridesData
                            , appOverrides = appOverridesData
                            }
                    in
                    ( { model | saveResp = Loading }, reqSaveDeployment model.config info )

                _ ->
                    ( model, Cmd.none )

        SaveDeploymentResponse resp ->
            ( { model | saveResp = resp }, Cmd.none )

        DeploymentFullInfoResponse resp ->
            ( { model | deployment = resp }, initCreate model.config )


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
            [ text <|
                case model.mode of
                    Create ->
                        "Create new deployment"

                    Update ->
                        "Update  deployment"
            ]
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
                   , overridesView model.deploymentOverrides DeploymentOverridesMsg
                   , overridesView model.appOverrides AppOverridesMsg
                   ]
            )
        ]


overridesView : Api.WebData Overrides.Model -> (Overrides.Msg -> Msg) -> Html Msg
overridesView overrides mapper =
    Html.map mapper <|
        case overrides of
            Success x ->
                Overrides.view x

            _ ->
                Overrides.overridesSectionLoading Overrides.Write


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
    case ( model.appOverrides, model.deploymentOverrides ) of
        ( Success appOverrides, Success deploymentOverrides ) ->
            Overrides.hasEmptyValues appOverrides
                || Overrides.hasEmptyValues deploymentOverrides
                || hasEmptyName model

        _ ->
            True
