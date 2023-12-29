module Page.Deployment.ActionTable exposing (..)

import Api
import Api.Endpoint as Endpoint exposing (..)
import Browser.Events
import Config exposing (Config, Settings)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickStopPropagation)
import Http
import Iso8601 exposing (fromTime)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..))
import Route
import Set.Any as Set exposing (AnySet)
import Time exposing (Month(..), Posix, Zone, posixToMillis, toMonth)
import Types.Action as Action exposing (ActionId, Log, LogWrapper, unActionId, unExitCode)
import Types.Deployment as Deployments exposing (..)
import Types.Override exposing (..)


type alias Model =
    { openedAppOverrides : AnySet Int ActionId
    , openedDeployemntOverrides : AnySet Int ActionId
    }


type Msg
    = OpenDeploymentOverrides ActionId
    | CloseDeploymentOverrides ActionId
    | OpenAppOverrides ActionId
    | CloseAppOverrides ActionId


init : Model
init =
    { openedAppOverrides = Set.empty unActionId
    , openedDeployemntOverrides = Set.empty unActionId
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    case cmd of
        OpenDeploymentOverrides id ->
            ( { model | openedDeployemntOverrides = Set.insert id model.openedDeployemntOverrides }, Cmd.none )

        CloseDeploymentOverrides id ->
            ( { model | openedDeployemntOverrides = Set.remove id model.openedDeployemntOverrides }, Cmd.none )

        OpenAppOverrides id ->
            ( { model | openedAppOverrides = Set.insert id model.openedAppOverrides }, Cmd.none )

        CloseAppOverrides id ->
            ( { model | openedAppOverrides = Set.remove id model.openedAppOverrides }, Cmd.none )


view : Model -> Api.WebData (List LogWrapper) -> Html Msg
view model logs =
    divClass "table table--actions" <|
        [ table []
            [ tablePrimaryHeaderView
            , tablePrimaryBodyView model logs
            ]
        ]


tablePrimaryHeaderView : Html Msg
tablePrimaryHeaderView =
    let
        simpleTh txt =
            th [] [ text txt ]
    in
    thead []
        [ tr []
            [ simpleTh "Action Type"
            , simpleTh "Deployment configuration"
            , simpleTh "App configuration"
            , simpleTh "Exit code"
            , simpleTh "Created"
            , simpleTh "Deployment duration"
            ]
        ]


tablePrimaryBodyView : Model -> Api.WebData (List LogWrapper) -> Html Msg
tablePrimaryBodyView model deployments =
    case deployments of
        Success d ->
            tableDeploymentsView model (List.concatMap .logs d)

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


tableDeploymentsView : Model -> List Log -> Html Msg
tableDeploymentsView model logs =
    tbody [] <| List.map (logView model) logs


overridesView :
    (ActionId -> Msg)
    -> (ActionId -> Msg)
    -> ActionId
    -> AnySet Int ActionId
    -> List Override
    -> List (Html Msg)
overridesView openCmd closeCmd id opened overrides =
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

    else if Set.member id opened then
        List.append (List.map overrideView overrides)
            [ buttonClass_ "expander listing__more expander--open" (closeCmd id) [ text "Hide default configuration" ] ]

    else
        List.append (List.map overrideView visibleOverrides)
            [ buttonClass_ "expander listing__more" (openCmd id) [ text "Show full configuration" ] ]


logView : Model -> Log -> Html Msg
logView model log =
    let
        name =
            case log.action of
                Action.Create ->
                    "create"

                Action.Restore ->
                    "restore"

                Action.Update ->
                    "update"

                Action.Archive ->
                    "archive"

        deploymentOverrides =
            [ divClass "row" <|
                overridesView OpenDeploymentOverrides
                    CloseDeploymentOverrides
                    log.actionId
                    model.openedDeployemntOverrides
                    log.deploymentOverrides
            ]

        appOverrides =
            [ divClass "row" <|
                overridesView OpenAppOverrides
                    CloseAppOverrides
                    log.actionId
                    model.openedAppOverrides
                    log.appOverrides
            ]

        exitCode =
            [ text (String.fromInt <| unExitCode log.exitCode)
            ]

        duration =
            [ text <| (String.fromInt <| round log.duration.time) ++ "s"
            ]
    in
    tr []
        [ td [] [ text name ]
        , td [] deploymentOverrides
        , td [] appOverrides
        , td [] exitCode
        , td [] [ text <| fromTime log.createdAt ]
        , td [] duration
        ]
