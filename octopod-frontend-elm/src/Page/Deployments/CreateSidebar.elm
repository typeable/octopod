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
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Time exposing (Month(..))
import Tree exposing (Tree, mergeTrees, mergeTreesSortWith)


type alias OverrideData =
    { path : String
    , originalPath : String
    , defaultValue : String
    , value : String
    , deleted : Bool
    , id : Int
    }


newOverride : String -> Int -> OverrideData
newOverride p id =
    { path = p
    , originalPath = p
    , defaultValue = ""
    , value = ""
    , deleted = False
    , id = id
    }


type alias Model =
    { appOverrides : WebData (Dict Int OverrideData)
    , deploymentOverrides : WebData (Dict Int OverrideData)
    , appOverrideKeys : WebData (List String)
    , deploymentOverrideKeys : WebData (List String)
    , name : String
    , visibility : Bool
    , config : Config
    , openedPaths : Set (List String)
    , nextAppOverrideId : Int
    }


show : Model -> Model
show model =
    { model | visibility = True }


hide : Model -> Model
hide model =
    { model | visibility = False }


init : Config -> Bool -> Model
init config visibility =
    { appOverrides = Loading
    , deploymentOverrides = Loading
    , appOverrideKeys = Loading
    , deploymentOverrideKeys = Loading
    , name = ""
    , visibility = visibility
    , config = config
    , openedPaths = Set.empty
    , nextAppOverrideId = 0
    }


hasEmptyOverrides : Dict Int OverrideData -> Bool
hasEmptyOverrides overrides =
    overrides
        |> Dict.toList
        |> List.filter (\( _, x ) -> x.path == "" && x.originalPath == "")
        |> (List.isEmpty >> not)


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


overridesToDict : List (List String) -> Dict Int OverrideData
overridesToDict raw =
    let
        f x =
            case x of
                [ p, v ] ->
                    Just ( p, v )

                _ ->
                    Nothing

        g i ( p, v ) =
            ( i, OverrideData p p v "" False i )
    in
    Dict.fromList <| List.indexedMap g <| List.filterMap f raw


type Msg
    = DeploymentOverrideKeysResponse (WebData (List String))
    | DeploymentOverridesResponse (WebData (List (List String)))
    | AppOverrideKeysResponse (WebData (List String))
    | AppOverridesResponse (WebData (List (List String)))
    | Close
    | Save
    | NameInput String
    | AddAppOverride
    | DeleteAppOverride Int
    | RestoreAppOverride Int
    | EditAppOverridePath Int String
    | EditAppOverrideValue Int String


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
    case cmd of
        DeploymentOverrideKeysResponse keys ->
            ( { model | deploymentOverrideKeys = keys }, Cmd.none )

        DeploymentOverridesResponse overrides ->
            ( { model | deploymentOverrides = RemoteData.map overridesToDict overrides }
            , Cmd.batch
                [ reqAppOverrideKeys model.config (RemoteData.withDefault [] overrides)
                , reqAppOverrides model.config (RemoteData.withDefault [] overrides)
                ]
            )

        AppOverrideKeysResponse keys ->
            ( { model | appOverrideKeys = keys }, Cmd.none )

        AppOverridesResponse overrides ->
            ( { model
                | appOverrides = RemoteData.map overridesToDict overrides
                , nextAppOverrideId = RemoteData.unwrap 0 List.length overrides + 1
              }
            , Cmd.none
            )

        Close ->
            ( { model | visibility = False }, Cmd.none )

        Save ->
            ( model, Cmd.none )

        NameInput name ->
            ( { model | name = name }, Cmd.none )

        AddAppOverride ->
            Debug.log "!"
                ( { model
                    | appOverrides =
                        RemoteData.map
                            (Dict.insert model.nextAppOverrideId (newOverride "" model.nextAppOverrideId))
                            model.appOverrides
                    , nextAppOverrideId = model.nextAppOverrideId + 1
                  }
                , Cmd.none
                )

        DeleteAppOverride ix ->
            let
                deleteValue val =
                    { val | deleted = True }
            in
            ( { model | appOverrides = RemoteData.map (Dict.update ix (Maybe.map deleteValue)) model.appOverrides }
            , Cmd.none
            )

        RestoreAppOverride ix ->
            let
                deleteValue val =
                    { val | deleted = False }
            in
            ( { model | appOverrides = RemoteData.map (Dict.update ix (Maybe.map deleteValue)) model.appOverrides }
            , Cmd.none
            )

        EditAppOverrideValue ix value ->
            let
                deleteValue override =
                    { override | value = value }
            in
            ( { model | appOverrides = RemoteData.map (Dict.update ix (Maybe.map deleteValue)) model.appOverrides }
            , Cmd.none
            )

        EditAppOverridePath ix path ->
            let
                deleteValue override =
                    { override | path = path }
            in
            ( { model | appOverrides = RemoteData.map (Dict.update ix (Maybe.map deleteValue)) model.appOverrides }
            , Cmd.none
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
                -- TODO
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
            , overridesSection model
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


overridesSection : Model -> Html Msg
overridesSection model =
    divClass "deployment__section"
        [ h3Class "deployment__sub-heading" [ text "App configuration" ]
        , case ( model.appOverrides, model.appOverrideKeys ) of
            ( Success overrides, Success keys ) ->
                overridesSectionData model overrides keys

            _ ->
                overridesSectionLoading model
        ]


overridesSectionLoading : Model -> Html Msg
overridesSectionLoading _ =
    let
        rowLoader =
            divClass "editable-row loader"
                [ divClass "editable-row__placeholder" []
                , divClass "editable-row__placeholder" []
                , divClass "overrides__delete spot spot--loader" []
                ]
    in
    divClass "deployment__widget"
        [ divClass "padded"
            [ Html.button [ Attr.class "dash--disabled dash dash--add overrides__add", Attr.disabled True ] [ text "Add an override" ] ]
        , rowLoader
        , rowLoader
        , rowLoader
        ]


overridesSectionData : Model -> Dict Int OverrideData -> List String -> Html Msg
overridesSectionData model overridesDict keys =
    let
        overrideCompare a b =
            case ( a, b ) of
                ( Tree.Node x [ Tree.Leaf _ ], Tree.Node y [ Tree.Leaf _ ] ) ->
                    compare x y

                ( Tree.Node _ [ Tree.Leaf _ ], _ ) ->
                    LT

                ( _, Tree.Node _ [ Tree.Leaf _ ] ) ->
                    GT

                ( Tree.Node x _, Tree.Node y _ ) ->
                    compare x y

                _ ->
                    GT

        chose

        overridesTree =
            overridesDict
                |> Dict.toList
                |> List.map (\( _, a ) -> Tree.pathToTree (String.split "." a.originalPath) a)
                |> mergeTreesSortWith overrideCompare

        addButton =
            if hasEmptyOverrides overridesDict then
                Html.button [ Attr.class "dash--disabled dash dash--add overrides__add", Attr.disabled True ] [ text "Add an override" ]

            else
                buttonClass "dash dash--add overrides__add" AddAppOverride [ text "Add an override" ]
    in
    divClass "deployment__widget"
        [ divClass "padded"
            (addButton :: overridesLevelView model overridesTree keys)
        ]


overridesLevelView : Model -> List (Tree OverrideData) -> List String -> List (Html Msg)
overridesLevelView model overrides keys =
    List.map
        (overrideView model keys [])
        overrides


overrideEditableView : Model -> List String -> OverrideData -> Html Msg
overrideEditableView model keys overrideData =
    divClass "row"
        [ divClass "editable-row"
            [ divClass "input editable-row__key"
                [ Html.input
                    [ Attr.class "input__widget key-default-pristine"
                    , Attr.placeholder "key"
                    , Attr.spellcheck False
                    , Attr.type_ "text"
                    , Attr.value overrideData.path
                    , onInput (EditAppOverridePath overrideData.id)
                    ]
                    []
                ]
            , divClass "input editable-row__value"
                [ Html.input
                    [ Attr.class "input__widget value-pristine"
                    , Attr.placeholder "value"
                    , Attr.spellcheck False
                    , Attr.type_ "text"
                    , Attr.value
                        (if overrideData.value == "" then
                            overrideData.defaultValue

                         else
                            overrideData.value
                        )
                    , onInput (EditAppOverrideValue overrideData.id)
                    ]
                    []
                ]
            , buttonClass "editable-row__delete spot spot--cancel" (DeleteAppOverride overrideData.id) []
            ]
        ]


overrideDeletedView : Model -> OverrideData -> Html Msg
overrideDeletedView _ overrideData =
    divClass "row"
        [ divClass "editable-row"
            [ divClass "input editable-row__key input--deleted"
                [ Html.input
                    [ Attr.class "input__widget key-deleted"
                    , Attr.placeholder "key"
                    , Attr.spellcheck False
                    , Attr.type_ "text"
                    , Attr.disabled True
                    , Attr.value overrideData.path
                    ]
                    []
                ]
            , divClass "input editable-row__value input--deleted"
                [ Html.input
                    [ Attr.class "input__widget value-deleted"
                    , Attr.placeholder "key"
                    , Attr.spellcheck False
                    , Attr.type_ "text"
                    , Attr.disabled True
                    , Attr.value
                        (if overrideData.value == "" then
                            overrideData.defaultValue

                         else
                            overrideData.value
                        )
                    ]
                    []
                ]
            , buttonClass "editable-row__delete spot spot--undo" (RestoreAppOverride overrideData.id) []
            ]
        ]


overrideView : Model -> List String -> List String -> Tree OverrideData -> Html Msg
overrideView model keys partPath override =
    case override of
        Tree.Node piece [ Tree.Leaf overrideData ] ->
            if overrideData.deleted then
                overrideDeletedView model overrideData

            else
                overrideEditableView model keys overrideData

        _ ->
            div [] [ text "lol" ]
