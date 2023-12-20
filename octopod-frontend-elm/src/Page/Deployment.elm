port module Page.Deployment exposing (..)

import Api
import Api.Endpoint exposing (appOverrides, deploymentFullInfo, deploymentOverrides)
import Browser.Navigation as Nav
import Config exposing (Config, Settings)
import Debounce exposing (Debounce)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Common exposing (aClassHrefExternal, aClassHrefInternal, bClass, dateView, divClass, h1Class, h3Class)
import Html.Events exposing (onClick)
import Html.Overrides as Overrides
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Sidebar.CreateUpdate as CreateSidebar
import RemoteData exposing (RemoteData(..))
import Route
import Types.Deployment as Deployments exposing (..)
import Types.OverrideWithDefault exposing (OverrideWithDefault, defaultOverrideEncoder, defaultOverridesDecode)


type alias Model =
    { settings : Settings
    , config : Config
    , deployment : Api.WebData Deployment
    , deploymentName : DeploymentName
    , debounce : Debounce ()
    , deploymentOverrides : Api.WebData Overrides.Model
    , appOverrides : Api.WebData Overrides.Model
    , sidebar : CreateSidebar.Model
    , appDefaults : Api.WebData (List OverrideWithDefault)
    , deploymentDefaults : Api.WebData (List OverrideWithDefault)
    }


init : Settings -> Config -> DeploymentName -> ( Model, Cmd Msg )
init settings config deploymentName =
    ( { settings = settings
      , config = config
      , deployment = Loading
      , deploymentName = deploymentName
      , debounce = Debounce.init
      , deploymentOverrides = Loading
      , appOverrides = Loading
      , sidebar = CreateSidebar.init config CreateSidebar.Update False
      , appDefaults = Loading
      , deploymentDefaults = Loading
      }
    , Cmd.batch
        [ reqDeployment deploymentName config
        ]
    )


reqDeployment : DeploymentName -> Config -> Cmd Msg
reqDeployment deploymentName cfg =
    Api.get cfg (deploymentFullInfo deploymentName) deploymentDecoder (RemoteData.fromResult >> DeploymentFullInfoResponse)


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
    = DeploymentFullInfoResponse (Api.WebData Deployment)
    | WSUpdate String
    | DebounceMsg Debounce.Msg
    | DeploymentOverridesResponse (Api.WebData (List OverrideWithDefault))
    | AppOverridesResponse (Api.WebData (List OverrideWithDefault))
    | AppOverridesMsg Overrides.Msg
    | DeploymentOverridesMsg Overrides.Msg
    | ShowSidebar Deployment
    | CreateSidebarMsg CreateSidebar.Msg


reqDeploymentOverrides : Config -> Cmd Msg
reqDeploymentOverrides config =
    Api.get config
        deploymentOverrides
        (Decode.list defaultOverridesDecode)
        (RemoteData.fromResult >> DeploymentOverridesResponse)


reqAppOverrides : Config -> List OverrideWithDefault -> Cmd Msg
reqAppOverrides config body =
    Api.post config
        appOverrides
        (Http.jsonBody (Encode.list defaultOverrideEncoder body))
        (Decode.list defaultOverridesDecode)
        (RemoteData.fromResult >> AppOverridesResponse)


debounceConfig : Debounce.Config Msg
debounceConfig =
    { strategy = Debounce.soon 2000
    , transform = DebounceMsg
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    let
        updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
        updateWith toModel toMsg ( subModel, subCmd ) =
            ( toModel subModel
            , Cmd.map toMsg subCmd
            )

        initOverrides name defaults edits =
            Overrides.init defaults edits [] Overrides.Read name
    in
    case cmd of
        DeploymentFullInfoResponse deployment ->
            ( { model | deployment = deployment }
            , Cmd.batch
                [ reqDeploymentOverrides model.config ]
            )

        WSUpdate _ ->
            let
                ( debounce, subCmd ) =
                    Debounce.push debounceConfig () model.debounce
            in
            ( { model | debounce = debounce }, subCmd )

        DebounceMsg msg ->
            let
                ( debounce, subCmd ) =
                    Debounce.update
                        debounceConfig
                        (Debounce.takeAll (\_ _ -> reqDeployment model.deploymentName model.config))
                        msg
                        model.debounce
            in
            ( { model | debounce = debounce }, subCmd )

        DeploymentOverridesResponse overrides ->
            ( { model
                | deploymentDefaults = overrides
                , deploymentOverrides =
                    RemoteData.map2
                        (initOverrides "Deployment configuration")
                        overrides
                        (RemoteData.map (\x -> x.deployment.deploymentOverrides) model.deployment)
              }
            , case overrides of
                Success defaults ->
                    reqAppOverrides model.config defaults

                _ ->
                    Cmd.none
            )

        AppOverridesResponse overrides ->
            ( { model
                | appDefaults = overrides
                , appOverrides =
                    RemoteData.map2
                        (initOverrides "App configuration")
                        overrides
                        (RemoteData.map (\x -> x.deployment.appOverrides) model.deployment)
              }
            , Cmd.none
            )

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
                    Overrides.update subMsg deploymentOverrides
                        |> updateWith (\updated -> { model | deploymentOverrides = Success updated }) DeploymentOverridesMsg

                _ ->
                    ( model, Cmd.none )

        ShowSidebar deployment ->
            ( { model | sidebar = CreateSidebar.initWithDeployment model.config CreateSidebar.Update True deployment }
            , Cmd.map CreateSidebarMsg (CreateSidebar.initReqs model.config)
            )

        CreateSidebarMsg subMsg ->
            CreateSidebar.update subMsg model.sidebar
                |> updateWith (\sidebar -> { model | sidebar = sidebar }) CreateSidebarMsg


port deploymentReceiver : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ deploymentReceiver WSUpdate
        ]


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Deployments"
    , content = pageView model :: List.map (Html.map CreateSidebarMsg) (CreateSidebar.view model.sidebar)
    }


pageView : Model -> Html Msg
pageView model =
    pageWrapper
        [ backButton
        , deploymentHeaderView model
        , pageBodyView model
        ]


pageWrapper : List (Html Msg) -> Html Msg
pageWrapper body =
    divClass "page" [ divClass "page__wrap container" body ]


backButton : Html Msg
backButton =
    aClassHrefInternal "page__back dash dash--back dash--smaller" Route.Deployments [ text "All deployments" ]


deploymentHeaderView : Model -> Html Msg
deploymentHeaderView model =
    let
        buttonCommon disabled mMsg cls txt =
            Html.button
                ([ Attr.class cls
                 , Attr.disabled disabled
                 ]
                    ++ (case mMsg of
                            Just msg ->
                                [ onClick msg ]

                            _ ->
                                []
                       )
                )
                [ text txt ]

        buttonEnabled cls msg txt =
            buttonCommon False (Just msg) cls txt

        buttonDisabled cls txt =
            buttonCommon True Nothing (cls ++ " button--disabled") txt

        buttons =
            case model.deployment of
                Success deployment ->
                    if isPending deployment.status then
                        [ buttonDisabled "button button--edit page__action" "Edit deployment"
                        , buttonDisabled "button button--archive page__action button--secondary" "Move to archive"
                        , aClassHrefExternal "button button--logs page__action button--secondary"
                            (model.config.k8sDashboardUrlTemplate ++ unDeploymentName model.deploymentName)
                            [ text "Details" ]
                        ]

                    else if isDeploymentArchived deployment then
                        [ buttonDisabled " button button--restore page__action button--secondary" "Restore from archive"
                        , aClassHrefExternal "button button--logs page__action button--secondary"
                            (model.config.k8sDashboardUrlTemplate ++ unDeploymentName model.deploymentName)
                            [ text "Details" ]
                        ]

                    else
                        [ buttonEnabled "button button--edit page__action" (ShowSidebar deployment) "Edit deployment"
                        , buttonDisabled "button button--archive page__action button--secondary" "Move to archive"
                        , aClassHrefExternal "button button--logs page__action button--secondary"
                            (model.config.k8sDashboardUrlTemplate ++ unDeploymentName model.deploymentName)
                            [ text "Details" ]
                        ]

                _ ->
                    []
    in
    divClass "page__head"
        (h1Class "page__heading title" [ text (unDeploymentName model.deploymentName) ]
            :: buttons
        )


pageBodyView : Model -> Html Msg
pageBodyView model =
    divClass "page__body"
        [ case model.deployment of
            RemoteData.Success deployment ->
                deploymentView model deployment

            RemoteData.Failure _ ->
                failureDeploymentView model

            _ ->
                loadingDeploymentView model
        ]


loadingDeploymentView : Model -> Html Msg
loadingDeploymentView model =
    divClass "no-deployment"
        [ divClass "loading loading--enlarged loading--alternate"
            [ text "Loading ..." ]
        ]


failureDeploymentView : Model -> Html Msg
failureDeploymentView model =
    divClass "no-deployment"
        [ divClass "null null--data"
            [ divClass "null__content"
                [ bClass "null__heading"
                    [ text "Cannot retrieve the data"
                    , divClass "null__message" [ text "Try to reload page" ]
                    ]
                ]
            ]
        ]


deploymentView : Model -> Deployment -> Html Msg
deploymentView model deployment =
    divClass "deployment"
        [ deploymentSummaryView model deployment
        , deploymentLinksView model deployment
        , overridesView model.deploymentOverrides DeploymentOverridesMsg
        , overridesView model.appOverrides AppOverridesMsg
        ]


overridesView : Api.WebData Overrides.Model -> (Overrides.Msg -> Msg) -> Html Msg
overridesView overrides mapper =
    Html.map mapper <|
        case overrides of
            Success x ->
                Overrides.view x

            _ ->
                Overrides.overridesSectionLoading Overrides.Read


deploymentSummaryView : Model -> Deployment -> Html Msg
deploymentSummaryView model deployment =
    let
        statusView =
            case deployment.status of
                DeploymentPending _ ->
                    divClass "status status--pending" [ text "Pending..." ]

                DeploymentNotPending Running ->
                    divClass "status status--success" [ text "Running" ]

                DeploymentNotPending (Deployments.Failure _) ->
                    divClass "status status--failure" [ text "Failure" ]

                DeploymentNotPending CreatePending ->
                    divClass "loading loading--status-alike" [ text "Creating..." ]

                DeploymentNotPending UpdatePending ->
                    divClass "loading loading--status-alike" [ text "Updating..." ]

                DeploymentNotPending ArchivePending ->
                    divClass "loading loading--status-alike" [ text "Archiving..." ]

                DeploymentNotPending Archived ->
                    divClass "status status--archived" [ text "Archived" ]

                DeploymentNotPending CleanupFailed ->
                    divClass "status status--failure" [ text "Cleanup failed (contact admin)" ]
    in
    divClass "deployment__summary"
        [ divClass "deployment__stat"
            [ bClass "deployment__param" [ text "Status" ]
            , divClass "deployment__value" [ statusView ]
            ]
        , divClass "deployment__stat"
            [ bClass "deployment__param" [ text "Created" ]
            , divClass "deployment__value" [ dateView model.settings.zone deployment.createdAt ]
            ]
        , divClass "deployment__stat"
            [ bClass "deployment__param" [ text "Changed" ]
            , divClass "deployment__value" [ dateView model.settings.zone deployment.updatedAt ]
            ]
        ]


deploymentLinksView : Model -> Deployment -> Html Msg
deploymentLinksView _ deployment =
    let
        link m =
            aClassHrefExternal "listing__item external bar bar--larger" m.link [ text m.name ]
    in
    divClass "deployment__section"
        [ h3Class "deployment__sub-heading" [ text "Links" ]
        , divClass "deployment__widget"
            [ divClass "listing" <|
                List.map link deployment.metadata
            ]
        ]
