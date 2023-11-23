module Page.Deployments exposing (..)

import Api
import Api.Endpoint exposing (deployments)
import Browser.Navigation as Nav
import Char exposing (toUpper)
import Config exposing (Config, Settings)
import Deployments exposing (Deployment, DeploymentStatus(..), Deployments, Override, OverrideValue(..), Status(..), deploymentsDecoder, isDeploymentArchived)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (aClassHrefExternal, bClass, buttonClass, divClass, h1Class, spanClass, trClass)
import Html.Events exposing (onInput)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Time exposing (Month(..), Posix, Zone, posixToMillis, toMonth)


type alias Model =
    { settings : Settings
    , config : Config
    , deployments : WebData Deployments
    , sort : Sort
    , openedAppOverrides : Set String
    , openedDeployemntOverrides : Set String
    , search : String
    , moreButton : Maybe String
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
      , sort = Asc Updated
      , openedAppOverrides = Set.empty
      , openedDeployemntOverrides = Set.empty
      , search = ""
      , moreButton = Nothing
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
    | SortChanged Sort
    | SearchInput String
    | OpenDeploymentOverrides String
    | CloseDeploymentOverrides String
    | OpenAppOverrides String
    | CloseAppOverrides String


reqConfig : Config -> Cmd Msg
reqConfig cfg =
    Api.get cfg deployments deploymentsDecoder (RemoteData.fromResult >> DeploymentsResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update cmd model =
    case cmd of
        DeploymentsResponse deployments ->
            ( { model | deployments = deployments }, Cmd.none )

        SortChanged sort ->
            ( { model | sort = sort }, Cmd.none )

        SearchInput search ->
            ( { model | search = search }, Cmd.none )

        OpenDeploymentOverrides name ->
            ( { model | openedDeployemntOverrides = Set.insert name model.openedDeployemntOverrides }, Cmd.none )

        CloseDeploymentOverrides name ->
            ( { model | openedDeployemntOverrides = Set.remove name model.openedDeployemntOverrides }, Cmd.none )

        OpenAppOverrides name ->
            ( { model | openedAppOverrides = Set.insert name model.openedAppOverrides }, Cmd.none )

        CloseAppOverrides name ->
            ( { model | openedAppOverrides = Set.remove name model.openedAppOverrides }, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Deployments"
    , content =
        pageWrapper
            [ pageHeaderView model
            , pageBodyWrapper
                [ dataPrimaryView model ]
            ]
    }


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


dataPrimaryView : Model -> Html Msg
dataPrimaryView model =
    divClass "data__primary"
        [ divClass "table table--deployments table--clickable table--double-click"
            [ tablePrimaryView model ]
        ]


tablePrimaryView : Model -> Html Msg
tablePrimaryView model =
    table []
        [ tablePrimaryHeaderView model
        , tablePrimaryBodyView model
        ]


tablePrimaryHeaderView : Model -> Html Msg
tablePrimaryHeaderView model =
    let
        sortTh : Column -> Sort -> String -> Html Msg
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

        simpleTh : String -> Html Msg
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
            ]
        ]


tablePrimaryBodyView : Model -> Html Msg
tablePrimaryBodyView model =
    case model.deployments of
        Success d ->
            tableDeploymentsView model d

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


tableDeploymentsView : Model -> Deployments -> Html Msg
tableDeploymentsView model ds =
    let
        active =
            List.filter (isDeploymentArchived >> not) ds

        cmp : Deployment -> Deployment -> Order
        cmp a b =
            case model.sort of
                Asc Name ->
                    compare a.deployment.name b.deployment.name

                Desc Name ->
                    compare b.deployment.name a.deployment.name

                Asc Created ->
                    compare (posixToMillis a.createdAt) (posixToMillis b.createdAt)

                Desc Created ->
                    compare (posixToMillis b.createdAt) (posixToMillis a.createdAt)

                Asc Updated ->
                    compare (posixToMillis a.updatedAt) (posixToMillis b.updatedAt)

                Desc Updated ->
                    compare (posixToMillis b.updatedAt) (posixToMillis a.updatedAt)

        check d =
            if model.search == "" then
                True

            else
                String.contains (String.toUpper model.search) (String.toUpper d.deployment.name)

        sorted =
            List.filter check (List.sortWith cmp active)
    in
    if List.length sorted > 0 then
        tbody [] <| List.map (deploymentView model) sorted

    else
        tableFailureView


overridesView : (String -> Msg) -> (String -> Msg) -> String -> Set String -> List Override -> List (Html Msg)
overridesView openCmd closeCmd dName opened overrides =
    let
        overrideView o =
            divClass "row" <|
                case o.value of
                    ValueAdded v ->
                        [ spanClass "key-default-pristine" [ text <| o.name ++ ": " ]
                        , Html.span
                            [ Attr.class "value-edited"
                            , Attr.attribute
                                "style"
                                "width:400px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;display:block;"
                            ]
                            [ text v ]
                        ]

                    ValueDeleted ->
                        [ spanClass "key-deleted" [ text <| o.name ++ ": " ]
                        , divClass "listing__placeholder listing__placeholder__value" []
                        ]

        visibleOverrides =
            List.take 3 overrides
    in
    if List.length overrides == List.length visibleOverrides then
        List.map overrideView overrides

    else if Set.member dName opened then
        List.append (List.map overrideView overrides)
            [ buttonClass "expander listing__more expander--open" (closeCmd dName) [ text "Hide default configuration" ] ]

    else
        List.append (List.map overrideView visibleOverrides)
            [ buttonClass "expander listing__more" (openCmd dName) [ text "Show full configuration" ] ]


dateView : Zone -> Posix -> Html Msg
dateView zone time =
    let
        toMonth month_ =
            case month_ of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        year =
            String.fromInt (Time.toYear zone time)

        month =
            toMonth (Time.toMonth zone time)

        day =
            String.padLeft 2 '0' <|
                String.fromInt (Time.toDay zone time)
    in
    text <| String.join "-" [ year, month, day ]


deploymentView : Model -> Deployment -> Html Msg
deploymentView model d =
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
            [ text d.deployment.name
            , statusView d.status
            ]

        link m =
            aClassHrefExternal "listing__item external bar" m.link [ text m.name ]

        links =
            [ divClass "listing" <| List.map link d.metadata ]

        deploymentOverrides =
            [ divClass "row" <|
                overridesView OpenDeploymentOverrides
                    CloseDeploymentOverrides
                    d.deployment.name
                    model.openedDeployemntOverrides
                    d.deployment.deploymentOverrides
            ]

        appOverrides =
            [ divClass "row" <|
                overridesView OpenAppOverrides
                    CloseAppOverrides
                    d.deployment.name
                    model.openedAppOverrides
                    d.deployment.appOverrides
            ]
    in
    tr []
        [ td [] name
        , td [] links
        , td [] deploymentOverrides
        , td [] appOverrides
        , td [] [ dateView model.settings.zone d.createdAt ]
        , td [] [ dateView model.settings.zone d.updatedAt ]
        ]
