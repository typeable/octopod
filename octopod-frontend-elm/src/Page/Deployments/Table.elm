module Page.Deployments.Table exposing (..)

import Api
import Api.Endpoint as Endpoint exposing (..)
import Browser.Events
import Config exposing (Config, Settings)
import Deployments exposing (..)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickStopPropagation)
import Http
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import Route
import Set.Any as Set exposing (AnySet)
import Time exposing (Month(..), Posix, Zone, posixToMillis, toMonth)


type alias Model =
    { sort : Sort
    , openedAppOverrides : AnySet String DeploymentName
    , openedDeployemntOverrides : AnySet String DeploymentName
    , menuButton : Maybe DeploymentName
    , tableType : TableType
    , archivePopup : Maybe DeploymentName
    , config : Config
    , settings : Settings
    }


type TableType
    = ActiveTable
    | ArchivedTable


type Sort
    = Asc Column
    | Desc Column


type Column
    = Name
    | Updated
    | Created


type Msg
    = SortChanged Sort
    | OpenDeploymentOverrides DeploymentName
    | CloseDeploymentOverrides DeploymentName
    | OpenAppOverrides DeploymentName
    | CloseAppOverrides DeploymentName
    | OpenMenu DeploymentName
    | CloseMenu
    | OpenArchivePopup DeploymentName
    | CloseArchivePopup
    | DeleteDeploymentReq DeploymentName
    | DeleteDeploymentResp (Api.WebData String)
    | RestoreDeploymentReq DeploymentName
    | RestoreDeploymentResp (Api.WebData String)
    | GoToDeployment DeploymentName


init : Config -> Settings -> TableType -> Model
init config settings tableType =
    { sort = Desc Updated
    , openedAppOverrides = Set.empty unDeploymentName
    , openedDeployemntOverrides = Set.empty unDeploymentName
    , menuButton = Nothing
    , tableType = tableType
    , archivePopup = Nothing
    , config = config
    , settings = settings
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    case cmd of
        SortChanged sort ->
            ( { model | sort = sort }, Cmd.none )

        OpenDeploymentOverrides name ->
            ( { model | openedDeployemntOverrides = Set.insert name model.openedDeployemntOverrides }, Cmd.none )

        CloseDeploymentOverrides name ->
            ( { model | openedDeployemntOverrides = Set.remove name model.openedDeployemntOverrides }, Cmd.none )

        OpenAppOverrides name ->
            ( { model | openedAppOverrides = Set.insert name model.openedAppOverrides }, Cmd.none )

        CloseAppOverrides name ->
            ( { model | openedAppOverrides = Set.remove name model.openedAppOverrides }, Cmd.none )

        OpenMenu name ->
            ( { model | menuButton = Just name }, Cmd.none )

        CloseMenu ->
            ( { model | menuButton = Nothing }, Cmd.none )

        OpenArchivePopup deploymentName ->
            ( { model | archivePopup = Just deploymentName }, Cmd.none )

        CloseArchivePopup ->
            ( { model | archivePopup = Nothing }, Cmd.none )

        DeleteDeploymentReq deploymentName ->
            ( { model | archivePopup = Nothing }, reqDeleteDeployment model.config deploymentName )

        DeleteDeploymentResp _ ->
            ( model, Cmd.none )

        RestoreDeploymentReq deploymentName ->
            ( { model | menuButton = Nothing }, reqRestoreDeployment model.config deploymentName )

        RestoreDeploymentResp _ ->
            ( model, Cmd.none )

        GoToDeployment deploymentName ->
            ( model, Route.pushUrl model.settings.navKey (Route.Deployment deploymentName) )


reqDeleteDeployment : Config -> DeploymentName -> Cmd Msg
reqDeleteDeployment config deploymentName =
    Api.delete config
        (Endpoint.deleteDeployment deploymentName)
        Http.emptyBody
        Decode.string
        (RemoteData.fromResult >> DeleteDeploymentResp)


reqRestoreDeployment : Config -> DeploymentName -> Cmd Msg
reqRestoreDeployment config deploymentName =
    Api.patch config
        (Endpoint.restoreDeployment deploymentName)
        Decode.string
        (RemoteData.fromResult >> RestoreDeploymentResp)


getDropdownId : Model -> String
getDropdownId model =
    case model.tableType of
        ActiveTable ->
            "active-dropdown"

        ArchivedTable ->
            "archived-dropdown"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.menuButton of
        Just _ ->
            Browser.Events.onMouseDown (outsideTarget (getDropdownId model))

        Nothing ->
            Sub.none


outsideTarget : String -> Decode.Decoder Msg
outsideTarget dropdownId =
    Decode.field "target" (isOutsideDropdown dropdownId)
        |> Decode.andThen
            (\isOutside ->
                if isOutside then
                    Decode.succeed CloseMenu

                else
                    Decode.fail "inside dropdown"
            )


isOutsideDropdown : String -> Decode.Decoder Bool
isOutsideDropdown dropdownId =
    Decode.oneOf
        [ Decode.field "id" Decode.string
            |> Decode.andThen
                (\id ->
                    if dropdownId == id then
                        Decode.succeed False

                    else
                        Decode.fail "check parent node"
                )
        , Decode.lazy (\_ -> isOutsideDropdown dropdownId |> Decode.field "parentNode")
        , Decode.succeed True
        ]


view : Model -> Api.WebData Deployments -> String -> Html Msg
view model deployments search =
    divClass "table table--deployments table--clickable table--double-click" <|
        table []
            [ tablePrimaryHeaderView model
            , tablePrimaryBodyView model deployments search
            ]
            :: (case model.archivePopup of
                    Nothing ->
                        []

                    Just deploymentName ->
                        [ archivePopupView deploymentName ]
               )


archivePopupView : DeploymentName -> Html Msg
archivePopupView deploymentName =
    div
        [ Attr.class "classic-popup"
        , Attr.style "display" "block"
        ]
        [ divClass "classic-popup__container"
            [ divClass "classic-popup__viewport"
                [ divClass "classic-popup__slot"
                    [ divClass "dialog dialog--archive"
                        [ divClass "dialog__content"
                            [ text "Are you sure you want to archive the"
                            , br [] []
                            , text (unDeploymentName deploymentName)
                            , text " deployment?"
                            ]
                        , divClass "dialog__footer"
                            [ buttonClass "button dialog__action"
                                (DeleteDeploymentReq deploymentName)
                                [ text "Archive" ]
                            , buttonClass "button dialog__action button--secondary"
                                CloseArchivePopup
                                [ text "Cancel" ]
                            ]
                        ]
                    , buttonClass "classic-popup__close"
                        CloseArchivePopup
                        []
                    ]
                ]
            ]
        ]


tablePrimaryHeaderView : Model -> Html Msg
tablePrimaryHeaderView model =
    let
        sortTh clm sort txt =
            let
                ( cls, event ) =
                    case sort of
                        Asc sClm ->
                            if sClm == clm then
                                ( "sort sort--active sort--asc", SortChanged (Desc clm) )

                            else
                                ( "sort", SortChanged (Asc clm) )

                        Desc sClm ->
                            if sClm == clm then
                                ( "sort sort--active sort--desc", SortChanged (Asc clm) )

                            else
                                ( "sort", SortChanged (Asc clm) )
            in
            th [] [ buttonClass cls event [ text txt ] ]

        simpleTh txt =
            th [] [ text txt ]
    in
    thead []
        [ tr []
            [ sortTh Name model.sort "Name"
            , simpleTh "Links"
            , simpleTh "Deployment configuration"
            , simpleTh "App configuration"
            , sortTh Created model.sort "Created"
            , sortTh Updated model.sort "Updated"
            , th [] [ spanClass "visuallyhidden" [ text "Menu" ] ]
            ]
        ]


tablePrimaryBodyView : Model -> Api.WebData Deployments -> String -> Html Msg
tablePrimaryBodyView model deployments search =
    case deployments of
        Success d ->
            tableDeploymentsView model d search

        RemoteData.Failure _ ->
            tableFailureView

        _ ->
            tableLoadingView


tableLoadingView : Html Msg
tableLoadingView =
    tbody []
        [ trClass "no-table"
            [ td [ Attr.colspan 8 ]
                [ divClass "loading loading--enlarged loading--alternate"
                    [ text "Loading..." ]
                ]
            ]
        ]


tableFailureView : Html Msg
tableFailureView =
    tbody []
        [ trClass "no-table"
            [ td [ Attr.colspan 8 ]
                [ divClass "null null--data"
                    [ bClass "null__heading" [ text "Cannot retrieve the data" ]
                    , divClass "null__message" [ text "Try to reload page" ]
                    ]
                ]
            ]
        ]


tableDeploymentsView : Model -> Deployments -> String -> Html Msg
tableDeploymentsView model deployments search =
    let
        compareDeployments : Deployment -> Deployment -> Order
        compareDeployments a b =
            case model.sort of
                Asc Name ->
                    compare (unDeploymentName a.deployment.name) (unDeploymentName b.deployment.name)

                Desc Name ->
                    compare (unDeploymentName b.deployment.name) (unDeploymentName a.deployment.name)

                Asc Created ->
                    compare (posixToMillis a.createdAt) (posixToMillis b.createdAt)

                Desc Created ->
                    compare (posixToMillis b.createdAt) (posixToMillis a.createdAt)

                Asc Updated ->
                    compare (posixToMillis a.updatedAt) (posixToMillis b.updatedAt)

                Desc Updated ->
                    compare (posixToMillis b.updatedAt) (posixToMillis a.updatedAt)

        check deployment =
            if search == "" then
                True

            else
                String.contains
                    (String.toUpper search)
                    (String.toUpper <| unDeploymentName <| deployment.deployment.name)

        sorted =
            List.filter check (List.sortWith compareDeployments deployments)
    in
    if List.length sorted > 0 then
        tbody [] <| List.map (deploymentView model) sorted

    else
        tableFailureView


overridesView :
    (DeploymentName -> Msg)
    -> (DeploymentName -> Msg)
    -> DeploymentName
    -> AnySet String DeploymentName
    -> List Override
    -> List (Html Msg)
overridesView openCmd closeCmd dName opened overrides =
    let
        overrideView override =
            divClass "row" <|
                case override.value of
                    ValueAdded v ->
                        [ spanClass "key-default-pristine" [ text <| unOverrideName override.name ++ ": " ]
                        , Html.span
                            [ Attr.class "value-edited"
                            , Attr.attribute
                                "style"
                                "width:400px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:block;"
                            ]
                            [ text v ]
                        ]

                    ValueDeleted ->
                        [ spanClass "key-deleted" [ text <| unOverrideName override.name ++ ": " ]
                        , divClass "listing__placeholder listing__placeholder__value" []
                        ]

        visibleOverrides =
            List.take 3 overrides

        buttonClass_ cls msg body =
            Html.button
                [ Attr.class cls
                , onClickStopPropagation msg
                ]
                body
    in
    if List.length overrides == List.length visibleOverrides then
        List.map overrideView overrides

    else if Set.member dName opened then
        List.append (List.map overrideView overrides)
            [ buttonClass_ "expander listing__more expander--open" (closeCmd dName) [ text "Hide default configuration" ] ]

    else
        List.append (List.map overrideView visibleOverrides)
            [ buttonClass_ "expander listing__more" (openCmd dName) [ text "Show full configuration" ] ]


deploymentView : Model -> Deployment -> Html Msg
deploymentView model deployment =
    let
        statusView status =
            case status of
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

        name =
            [ text (unDeploymentName deployment.deployment.name)
            , statusView deployment.status
            ]

        link m =
            aClassHrefExternal "listing__item external bar" m.link [ text m.name ]

        links =
            case model.tableType of
                ActiveTable ->
                    [ divClass "listing" <| List.map link deployment.metadata ]

                ArchivedTable ->
                    [ text "..." ]

        deploymentOverrides =
            [ divClass "row" <|
                overridesView OpenDeploymentOverrides
                    CloseDeploymentOverrides
                    deployment.deployment.name
                    model.openedDeployemntOverrides
                    deployment.deployment.deploymentOverrides
            ]

        appOverrides =
            [ divClass "row" <|
                overridesView OpenAppOverrides
                    CloseAppOverrides
                    deployment.deployment.name
                    model.openedAppOverrides
                    deployment.deployment.appOverrides
            ]

        menuButtonClass status class =
            if isPending status then
                class ++ " action--disabled "

            else
                class

        buttonClass_ cls msg body =
            Html.button
                [ Attr.class cls
                , onClickStopPropagation msg
                ]
                body

        dropDown =
            div
                [ Attr.class <|
                    "drop drop--actions "
                        ++ (if model.menuButton == Just deployment.deployment.name then
                                "drop--expanded"

                            else
                                ""
                           )
                , Attr.id (getDropdownId model)
                ]
                [ buttonClass_ "drop__handler"
                    (OpenMenu deployment.deployment.name)
                    [ text "Actions" ]
                , divClass "drop__dropdown" <|
                    case model.tableType of
                        ActiveTable ->
                            [ button
                                [ Attr.class (menuButtonClass deployment.status "action action--edit")
                                , Attr.type_ "button"
                                , Attr.disabled (isPending deployment.status)
                                , onClick CloseMenu
                                ]
                                [ text "Edit" ]
                            , button
                                [ Attr.class (menuButtonClass deployment.status "action action--archive classic-popup-handler")
                                , Attr.type_ "button"
                                , Attr.disabled (isPending deployment.status)
                                , onClick (OpenArchivePopup deployment.deployment.name)
                                ]
                                [ text "Move to archive" ]
                            , aClassHrefExternal "action action--logs"
                                (model.config.k8sDashboardUrlTemplate ++ unDeploymentName deployment.deployment.name)
                                [ text "Details" ]
                            ]

                        ArchivedTable ->
                            [ buttonClass "action action--restore"
                                (RestoreDeploymentReq deployment.deployment.name)
                                [ text "Restore from archive" ]
                            ]
                ]
    in
    tr [ onClick (GoToDeployment deployment.deployment.name) ]
        [ td [] name
        , td [] links
        , td [] deploymentOverrides
        , td [] appOverrides
        , td [] [ dateView model.settings.zone deployment.createdAt ]
        , td [] [ dateView model.settings.zone deployment.updatedAt ]
        , td [] [ dropDown ]
        ]
