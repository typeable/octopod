module Html.Overrides exposing
    ( Mode(..)
    , Model
    , Msg
    , dataChanged
    , getEditedOverrides
    , getFullOverrides
    , hasEmptyValues
    , init
    , setDefaultAndEditedOverrides
    , setDefaultOverrides
    , setKeys
    , update
    , view
    )

import Api
import Api.Endpoint exposing (..)
import Api.Types.DefaultOverrides exposing (DefaultOverride, DefaultOverrides)
import Api.Types.Deployment as Deployments exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (onBlur, onFocus, onInput, onMouseDown)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)
import Time exposing (Month(..))
import Tree exposing (Tree, mergeTreesWithSort)



-- type alias OverrideData =
--     { path : String
--     , value : String
--     }


type Mode
    = Read
    | Write


type alias Model =
    { defaultOverrides : Api.WebData (Dict Int DefaultOverride)
    , keys : Api.WebData (List String)
    , editedOverrides : Dict Int Override
    , nextId : Int
    , openedPaths : Set (List String)
    , autocomplete : Maybe Int
    , name : String
    , mode : Mode
    }


init : String -> Mode -> Model
init name mode =
    { defaultOverrides = Loading
    , keys = Loading
    , editedOverrides = Dict.empty
    , nextId = 0
    , openedPaths = Set.empty
    , autocomplete = Nothing
    , name = name
    , mode = mode
    }


setDefaultOverrides : Api.WebData (List DefaultOverride) -> Model -> Model
setDefaultOverrides defaultOverrides model =
    let
        emptyOverrides overrides =
            Dict.toList overrides
                |> List.filter (\( _, o ) -> o.value == "")
                |> List.map (Tuple.second >> .name >> unOverrideName >> String.split ".")
                |> List.map
                    (List.Extra.inits
                        >> List.tail
                        >> Maybe.withDefault []
                        >> List.Extra.init
                        >> Maybe.withDefault []
                        >> List.reverse
                    )
                |> List.concat
                |> Set.fromList

        indexedPair i v =
            ( i, v )

        dictOverrides =
            RemoteData.map (Dict.fromList << List.indexedMap indexedPair) defaultOverrides
    in
    { model
        | defaultOverrides = dictOverrides
        , nextId = RemoteData.unwrap 0 Dict.size dictOverrides
        , openedPaths = RemoteData.unwrap Set.empty emptyOverrides dictOverrides
    }


setDefaultAndEditedOverrides :
    Api.WebData (List DefaultOverride)
    -> Api.WebData (List Override)
    -> Model
    -> Model
setDefaultAndEditedOverrides defaults edits model_ =
    let
        model =
            setDefaultOverrides defaults model_

        nextId =
            model.nextId + RemoteData.unwrap 0 List.length edits

        editedOverrides overrides =
            List.map2 (\a b -> ( a, b )) (List.range model.nextId (model.nextId + List.length overrides)) overrides

        dictOverrides =
            RemoteData.map editedOverrides edits
                |> RemoteData.unwrap Dict.empty Dict.fromList
    in
    { model | editedOverrides = dictOverrides, nextId = nextId }


getFullOverrides : Model -> List DefaultOverride
getFullOverrides model =
    let
        f ix o acc =
            case o.value of
                ValueDeleted ->
                    acc

                ValueAdded v ->
                    DefaultOverride o.name v :: acc

        g ix o d acc =
            case o.value of
                ValueDeleted ->
                    acc

                ValueAdded v ->
                    DefaultOverride d.name v :: acc

        h ix d acc =
            d :: acc
    in
    case model.defaultOverrides of
        Success defaultOverrides ->
            Dict.merge
                f
                g
                h
                model.editedOverrides
                defaultOverrides
                []

        _ ->
            []


getEditedOverrides : Model -> List Deployments.Override
getEditedOverrides model =
    model.editedOverrides
        |> Dict.values


setKeys : Api.WebData (List String) -> Model -> Model
setKeys keys model =
    { model | keys = keys }


type Msg
    = AddOverride
    | DeleteOverride Int
    | RestoreOverride Int
    | EditOverridePath Int String
    | EditOverrideValue Int String
    | OpenPath (List String)
    | ClosePath (List String)
    | ShowAutocomplete Int
    | HideAutocomplete


dataChanged : Msg -> Bool
dataChanged msg =
    case msg of
        DeleteOverride _ ->
            True

        RestoreOverride _ ->
            True

        EditOverridePath _ _ ->
            True

        EditOverrideValue _ _ ->
            True

        _ ->
            False


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddOverride ->
            ( newOverride model, Cmd.none )

        DeleteOverride ix ->
            ( deleteOverride ix model, Cmd.none )

        RestoreOverride ix ->
            ( restoreOverride ix model, Cmd.none )

        EditOverridePath ix path ->
            ( editOverridePath ix path model, Cmd.none )

        EditOverrideValue ix value ->
            ( editOverrideValue ix value model, Cmd.none )

        OpenPath path ->
            ( { model | openedPaths = Set.insert path model.openedPaths }, Cmd.none )

        ClosePath path ->
            ( { model | openedPaths = Set.remove path model.openedPaths }, Cmd.none )

        ShowAutocomplete ix ->
            ( { model | autocomplete = Just ix }, Cmd.none )

        HideAutocomplete ->
            ( { model | autocomplete = Nothing }, Cmd.none )


newOverride : Model -> Model
newOverride model =
    { model
        | editedOverrides = Dict.insert model.nextId (Override (OverrideName "") (ValueAdded "")) model.editedOverrides
        , nextId = model.nextId + 1
    }


deleteOverride : Int -> Model -> Model
deleteOverride ix model =
    let
        delete _ =
            case RemoteData.map (Dict.get ix) model.defaultOverrides of
                Success (Just defaultOverride) ->
                    Just
                        { name = defaultOverride.name
                        , value = ValueDeleted
                        }

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix delete model.editedOverrides }


restoreOverride : Int -> Model -> Model
restoreOverride ix model =
    { model | editedOverrides = Dict.update ix (\_ -> Nothing) model.editedOverrides }


editOverridePath : Int -> String -> Model -> Model
editOverridePath ix path model =
    let
        editPath mOverride =
            case ( mOverride, RemoteData.map (Dict.get ix) model.defaultOverrides ) of
                ( Just override, Success (Just default) ) ->
                    if OverrideName path == default.name then
                        Nothing

                    else
                        Just { override | name = OverrideName path }

                ( Just override, Success Nothing ) ->
                    Just { override | name = OverrideName path }

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix editPath model.editedOverrides }


editOverrideValue : Int -> String -> Model -> Model
editOverrideValue ix value model =
    let
        editPath mOverride =
            case ( mOverride, RemoteData.map (Dict.get ix) model.defaultOverrides ) of
                ( Just override, Success (Just default) ) ->
                    if value == default.value then
                        Nothing

                    else
                        Just { override | value = ValueAdded value }

                ( Just override, Success Nothing ) ->
                    Just { override | value = ValueAdded value }

                ( Nothing, Success (Just default) ) ->
                    if value == default.value then
                        Nothing

                    else
                        Just { name = default.name, value = ValueAdded value }

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix editPath model.editedOverrides }


view : Model -> Html Msg
view model =
    divClass "deployment__section"
        [ h3Class "deployment__sub-heading" [ text model.name ]
        , case ( model.defaultOverrides, model.keys, model.mode ) of
            ( Success defaultOverrides, Success keys, Write ) ->
                overridesSectionData model defaultOverrides keys

            ( Success defaultOverrides, _, Read ) ->
                overridesSectionData model defaultOverrides []

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


overridesSectionData : Model -> Dict Int DefaultOverride -> List String -> Html Msg
overridesSectionData model defaultOverrides keys =
    let
        isEmptyOverride _ v =
            case v.value of
                ValueAdded "" ->
                    True

                _ ->
                    False

        hasEmptyOverrides overrides =
            overrides
                |> Dict.filter isEmptyOverride
                |> (Dict.isEmpty >> not)

        newOverrides =
            Dict.merge
                (\i v acc -> Dict.insert i v acc)
                (\_ _ _ acc -> acc)
                (\_ _ acc -> acc)
                model.editedOverrides
                defaultOverrides
                Dict.empty

        addButton =
            if model.mode == Write then
                [ if hasEmptyOverrides newOverrides then
                    Html.button [ Attr.class "dash--disabled dash dash--add overrides__add", Attr.disabled True ] [ text "Add an override" ]

                  else
                    buttonClass "dash dash--add overrides__add" AddOverride [ text "Add an override" ]
                ]

            else
                []
    in
    divClass "deployment__widget"
        [ divClass "padded"
            (addButton ++ newOverridesView model newOverrides keys ++ overridesLevelView model defaultOverrides keys)
        ]


newOverridesView : Model -> Dict Int Override -> List String -> List (Html Msg)
newOverridesView model overrides keys =
    let
        overridesSorted =
            overrides
                |> Dict.toList
                |> List.sortBy (\( ix, _ ) -> ix)
                |> List.reverse
    in
    List.map
        (newOverrideView model keys)
        overridesSorted


newOverrideView : Model -> List String -> ( Int, Override ) -> Html Msg
newOverrideView model keys ( ix, override ) =
    case model.mode of
        Write ->
            overrideWriteWrapper model keys ix (Just override) Nothing

        _ ->
            div [] []



-- overrideReadView
--     "key-custom-edited"
--     "value-edited"
--     model
--     ixOverride
-- overrideReadView : String -> String -> Model -> Int -> Override -> Html Msg
-- overrideReadView pathClass valueClass model ix overrideData =
--     divClass "row"
--         [ spanClass pathClass [ text (overrideData.path ++ ": ") ]
--         , spanClass valueClass [ text overrideData.value ]
--         ]


overrideValueWriteView : Model -> String -> String -> Bool -> String -> Int -> Html Msg
overrideValueWriteView model valueClass inputClass inputDisabled overrideValue ix =
    let
        valueClass_ =
            if not inputDisabled && overrideValue == "" then
                valueClass ++ " input--error"

            else
                valueClass
    in
    divClass valueClass_
        (Html.input
            [ Attr.class inputClass
            , Attr.placeholder "value"
            , Attr.spellcheck False
            , Attr.type_ "text"
            , Attr.disabled inputDisabled
            , Attr.value overrideValue
            , onInput (EditOverrideValue ix)
            ]
            []
            :: (if not inputDisabled && overrideValue == "" then
                    [ divClass "input__output" [ text "Value can not be empty" ] ]

                else
                    []
               )
        )


overridePathWriteView : Model -> String -> String -> Bool -> String -> List String -> Int -> Html Msg
overridePathWriteView model pathClass inputClass inputDisabled overrideName keys ix =
    let
        suggestions =
            List.filter (String.startsWith overrideName) keys

        suggestionItem suggestion =
            Html.li
                [ Attr.class "input__suggest"
                , onMouseDown (EditOverridePath ix suggestion)
                ]
                [ b [] [ text overrideName ]
                , text (String.dropLeft (String.length overrideName) suggestion)
                ]

        inputPath =
            Html.input
                [ Attr.class inputClass
                , Attr.placeholder "value"
                , Attr.spellcheck False
                , Attr.type_ "text"
                , Attr.value overrideName
                , Attr.disabled inputDisabled
                , onInput (EditOverridePath ix)
                , onFocus (ShowAutocomplete ix)
                , onBlur HideAutocomplete
                ]
                []

        suggestionsView =
            case model.autocomplete of
                Nothing ->
                    []

                Just ax ->
                    if ax == ix && List.length suggestions > 0 then
                        [ ulClass "input__dropdown"
                            (List.map suggestionItem suggestions)
                        ]

                    else
                        []
    in
    divClass pathClass
        (inputPath
            :: suggestionsView
        )


type alias OverrideWriteConfig =
    { nameDivClass : String
    , nameInputClass : String
    , nameInputDisabled : Bool
    , nameInput : String
    , valueDivClass : String
    , valueInputClass : String
    , valueInputDisabled : Bool
    , valueInput : String
    , buttonClass : String
    , buttonMsg : Msg
    , ix : Int
    , keys : List String
    }


newOverrideConfig keys ix name value =
    { nameDivClass = "input editable-row__key"
    , nameInputClass = "input__widget key-custom-edited"
    , nameInputDisabled = False
    , nameInput = name
    , valueDivClass = "input editable-row__value"
    , valueInputClass = "input__widget value-edited"
    , valueInputDisabled = False
    , valueInput = value
    , buttonClass = "editable-row__delete spot spot--cancel"
    , buttonMsg = DeleteOverride ix
    , ix = ix
    , keys = keys
    }


editedOverrideConfig keys ix name value =
    { nameDivClass = "input editable-row__key"
    , nameInputClass = "input__widget key-default-edited"
    , nameInputDisabled = False
    , nameInput = name
    , valueDivClass = "input editable-row__value"
    , valueInputClass = "input__widget value-edited"
    , valueInputDisabled = False
    , valueInput = value
    , buttonClass = "editable-row__delete spot spot--cancel"
    , buttonMsg = DeleteOverride ix
    , ix = ix
    , keys = keys
    }


deletedOverrideConfig ix name value =
    { nameDivClass = "input editable-row__key input--deleted"
    , nameInputClass = "input__widget key-deleted"
    , nameInputDisabled = True
    , nameInput = name
    , valueDivClass = "input editable-row__value input--deleted"
    , valueInputClass = "input__widget value-deleted"
    , valueInputDisabled = True
    , valueInput = value
    , buttonClass = "editable-row__delete spot spot--undo"
    , buttonMsg = RestoreOverride ix
    , ix = ix
    , keys = []
    }


defaultOverrideConfig keys ix name value =
    { nameDivClass = "input editable-row__key"
    , nameInputClass = "input__widget key-default-pristine"
    , nameInputDisabled = False
    , nameInput = name
    , valueDivClass = "input editable-row__value"
    , valueInputClass = "input__widget value-pristine"
    , valueInputDisabled = False
    , valueInput = value
    , buttonClass = "editable-row__delete spot spot--cancel"
    , buttonMsg = DeleteOverride ix
    , ix = ix
    , keys = keys
    }


overrideWriteView :
    Model
    -> OverrideWriteConfig
    -> Html Msg
overrideWriteView model config =
    divClass "row"
        [ divClass "editable-row"
            [ overridePathWriteView model
                config.nameDivClass
                config.nameInputClass
                config.nameInputDisabled
                config.nameInput
                config.keys
                config.ix
            , overrideValueWriteView
                model
                config.valueDivClass
                config.valueInputClass
                config.valueInputDisabled
                config.valueInput
                config.ix
            , buttonClass config.buttonClass config.buttonMsg []
            ]
        ]


overridesLevelView : Model -> Dict Int DefaultOverride -> List String -> List (Html Msg)
overridesLevelView model defaultOverrides keys =
    let
        hasEmpty t =
            case t of
                Tree.Node _ cs ->
                    List.any hasEmpty cs

                Tree.Leaf ( _, d ) ->
                    d.value == ""

        overrideCompare a b =
            case ( a, b ) of
                ( Tree.Node x [ Tree.Leaf _ ], Tree.Node y [ Tree.Leaf _ ] ) ->
                    compare x y

                ( Tree.Node _ [ Tree.Leaf _ ], _ ) ->
                    LT

                ( _, Tree.Node _ [ Tree.Leaf _ ] ) ->
                    GT

                ( Tree.Node x _, Tree.Node y _ ) ->
                    case ( hasEmpty a, hasEmpty b ) of
                        ( True, True ) ->
                            compare x y

                        ( False, False ) ->
                            compare x y

                        ( True, False ) ->
                            LT

                        ( False, True ) ->
                            GT

                _ ->
                    EQ

        overridesTree =
            defaultOverrides
                |> Dict.toList
                |> List.map (\( ix, a ) -> Tree.pathToTree (String.split "." (unOverrideName a.name)) ( ix, a ))
                |> mergeTreesWithSort overrideCompare
    in
    List.map
        (treeOverrideView model keys [])
        overridesTree



-- overrideDeletedWriteView : Model -> List String -> ( Int, DefaultOverride ) -> Html Msg
-- overrideDeletedWriteView _ _ ( ix, overrideData ) =
--     divClass "row"
--         [ divClass "editable-row"
--             [ divClass "input editable-row__key input--deleted"
--                 [ Html.input
--                     [ Attr.class "input__widget key-deleted"
--                     , Attr.placeholder "key"
--                     , Attr.spellcheck False
--                     , Attr.type_ "text"
--                     , Attr.disabled True
--                     , Attr.value (unOverrideName overrideData.name)
--                     ]
--                     []
--                 ]
--             , divClass "input editable-row__value input--deleted"
--                 [ Html.input
--                     [ Attr.class "input__widget value-deleted"
--                     , Attr.placeholder "key"
--                     , Attr.spellcheck False
--                     , Attr.type_ "text"
--                     , Attr.disabled True
--                     , Attr.value overrideData.value
--                     ]
--                     []
--                 ]
--             , buttonClass "editable-row__delete spot spot--undo" (RestoreOverride ix) []
--             ]
--         ]
-- defaultOverrideWriteView : Model -> List String -> ( Int, DefaultOverride ) -> Html Msg
-- defaultOverrideWriteView =
--     overrideWriteView
--         "input editable-row__key"
--         "input__widget key-default-pristine"
--         "input editable-row__value"
--         "input__widget value-pristine"
-- editedOverrideWriteView : Model -> List String -> ( Int, Override ) -> Html Msg
-- editedOverrideWriteView =
--     overrideWriteView
--         "input editable-row__key"
--         "input__widget key-default-edited"
--         "input editable-row__value"
--         "input__widget value-edited"
-- editedOverrideReadView : Model -> ( Int, OverrideData ) -> Html Msg
-- editedOverrideReadView =
--     overrideReadView
--         "key-default-edited"
--         "value-edited"
-- overrideDeletedReadView : Model -> ( Int, OverrideData ) -> Html Msg
-- overrideDeletedReadView =
--     overrideReadView
--         "key-deleted"
--         "value-deletes"
-- defaultOverrideReadView : Model -> ( Int, OverrideData ) -> Html Msg
-- defaultOverrideReadView =
--     overrideReadView
--         "key-default-pristine"
--         "value-pristine"
-- overrideInputWriteView :


overrideWriteWrapper : Model -> List String -> Int -> Maybe Override -> Maybe DefaultOverride -> Html Msg
overrideWriteWrapper model keys ix override default =
    case ( override, default ) of
        ( Just o, _ ) ->
            case ( o.value, default ) of
                ( ValueAdded v, Just d ) ->
                    editedOverrideConfig keys ix (unOverrideName d.name) v
                        |> overrideWriteView model

                ( ValueAdded v, Nothing ) ->
                    newOverrideConfig keys ix (unOverrideName o.name) v
                        |> overrideWriteView model

                ( ValueDeleted, Just d ) ->
                    deletedOverrideConfig ix (unOverrideName d.name) d.value
                        |> overrideWriteView model

                _ ->
                    deletedOverrideConfig ix "" ""
                        |> overrideWriteView model

        ( Nothing, Just d ) ->
            defaultOverrideConfig keys ix (unOverrideName d.name) d.value
                |> overrideWriteView model

        _ ->
            div [] []



-- divClass "row"
--     [ divClass "editable-row" inputs
--         -- [ overridePathWriteView model pathClass pathInputClass keys ( ix, override )
--         -- , overrideValueWriteView model valueClass valueInputClass keys ( ix, override )
--         -- , buttonClass "editable-row__delete spot spot--cancel" (DeleteOverride ix) []
--         -- ]
--     ]
-- overrideReadView : Model -> Int -> Maybe Override -> Maybe DefaultOverride -> Html Msg
-- overrideReadView model ix override default =
--     div [] []


treeOverrideView : Model -> List String -> List String -> Tree ( Int, DefaultOverride ) -> Html Msg
treeOverrideView model keys piecies defaultOverride =
    case defaultOverride of
        Tree.Node _ [ Tree.Leaf ( ix, default ) ] ->
            case model.mode of
                Write ->
                    overrideWriteWrapper model keys ix (Dict.get ix model.editedOverrides) (Just default)

                Read ->
                    Debug.todo "!"

        -- overrideReadView model ix (Dict.get ix model.editedOverrides) (Just default)
        -- Write ->
        --     case Dict.get ix model.editedOverrides of
        --         Just edited ->
        --             case edited.value of
        --                 ValueAdded _ ->
        --                     editedOverrideWriteView model keys ( ix, edited )
        --                 ValueDeleted ->
        --                     overrideDeletedWriteView model keys ( ix, default )
        --         Nothing ->
        --             defaultOverrideWriteView model keys ( ix, default )
        -- Read ->
        --     Debug.todo "!"
        -- case Dict.get ix model.editedOverrides of
        --     Just (Just edited) ->
        --         editedOverrideReadView model ( ix, edited )
        --     Just Nothing ->
        --         overrideDeletedReadView model ( ix, default )
        --     Nothing ->
        --         defaultOverrideReadView model ( ix, default )
        Tree.Node piece cs ->
            if Set.member (piece :: piecies) model.openedPaths then
                divClass "collapse--project collapse collapse--expanded"
                    [ buttonClass "collapse__head"
                        (ClosePath (piece :: piecies))
                        [ text piece ]
                    , divClass "collapse__inner"
                        (List.map (treeOverrideView model keys (piece :: piecies)) cs)
                    ]

            else
                divClass "collapse--project collapse"
                    [ buttonClass "collapse__head collapse--expanded"
                        (OpenPath (piece :: piecies))
                        [ text piece ]
                    , divClass "collapse__inner" []
                    ]

        Tree.Leaf _ ->
            div [] []


hasEmptyValues : Model -> Bool
hasEmptyValues model =
    getFullOverrides model
        |> List.filter (\v -> v.value == "")
        |> (List.isEmpty >> not)
