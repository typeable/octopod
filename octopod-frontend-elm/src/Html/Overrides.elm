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
import Api.Types.DefaultOverrides exposing (DefaultOverride)
import Api.Types.Deployment as Deployments exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (on, onBlur, onFocus, onInput, onMouseDown)
import Html.Extra exposing (nothing)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)
import Time exposing (Month(..))
import Tree exposing (Tree, mergeTreesWithSort)


type Mode
    = Read
    | Write


type alias Model =
    { defaultOverrides : Api.WebData (Dict Int DefaultOverride)
    , keys : Api.WebData (List String)
    , editedOverrides : Dict Int Override
    , nextId : Int
    , lastTreeId : Int
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
    , lastTreeId = 0
    , openedPaths = Set.empty
    , autocomplete = Nothing
    , name = name
    , mode = mode
    }


setDefaultOverrides : Api.WebData (List DefaultOverride) -> Model -> Model
setDefaultOverrides defaultOverrides model =
    let
        emptyOverrides overrides =
            case model.mode of
                Write ->
                    List.filter (\o -> o.value == "") overrides
                        |> List.map (.name >> unOverrideName >> String.split "." >> List.reverse)
                        |> List.map (List.tail >> Maybe.withDefault [] >> List.Extra.inits)
                        |> List.concat
                        |> Set.fromList

                Read ->
                    Set.empty

        indexedPair i v =
            ( i, v )

        dictOverrides =
            RemoteData.map (Dict.fromList << List.indexedMap indexedPair) defaultOverrides
    in
    { model
        | defaultOverrides = dictOverrides
        , nextId = RemoteData.unwrap 0 Dict.size dictOverrides
        , lastTreeId = RemoteData.unwrap 0 Dict.size dictOverrides
        , openedPaths = RemoteData.unwrap Set.empty emptyOverrides defaultOverrides
    }


setDefaultAndEditedOverrides :
    Api.WebData (List DefaultOverride)
    -> Api.WebData (List Override)
    -> Model
    -> Model
setDefaultAndEditedOverrides defaultsRemote editsRemote model =
    let
        mkDicts defaults edits =
            let
                f _ d ( ds, os ) =
                    ( Dict.insert (Dict.size ds) d ds, os )

                g _ d o ( ds, os ) =
                    ( Dict.insert (Dict.size ds) d ds, Dict.insert (Dict.size ds) o os )

                h _ o ( ds, os ) =
                    ( ds, Dict.insert (Dict.size defaults + Dict.size os) o os )
            in
            Dict.merge
                f
                g
                h
                defaults
                edits
                ( Dict.empty, Dict.empty )

        mkOverrideDict =
            List.map (\x -> ( unOverrideName x.name, x )) >> Dict.fromList

        dictsRemote =
            RemoteData.map2
                mkDicts
                (RemoteData.map mkOverrideDict defaultsRemote)
                (RemoteData.map mkOverrideDict editsRemote)

        nextId =
            RemoteData.unwrap 0 (Tuple.first >> Dict.size) dictsRemote
                + RemoteData.unwrap 0 (Tuple.second >> Dict.size) dictsRemote
    in
    { model
        | editedOverrides = RemoteData.unwrap Dict.empty Tuple.second dictsRemote
        , defaultOverrides = RemoteData.map Tuple.first dictsRemote
        , lastTreeId = nextId
        , nextId = nextId
    }


getFullOverrides : Model -> List DefaultOverride
getFullOverrides model =
    let
        f _ o acc =
            case o.value of
                ValueDeleted ->
                    acc

                ValueAdded v ->
                    DefaultOverride o.name v :: acc

        g _ o d acc =
            case o.value of
                ValueDeleted ->
                    acc

                ValueAdded v ->
                    DefaultOverride d.name v :: acc

        h _ d acc =
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
overridesSectionLoading model =
    let
        rowLoader =
            divClass "editable-row loader"
                [ divClass "editable-row__placeholder" []
                , divClass "editable-row__placeholder" []
                , divClass "overrides__delete spot spot--loader" []
                ]

        addBtn =
            case model.mode of
                Read ->
                    []

                Write ->
                    [ divClass "padded"
                        [ Html.button
                            [ Attr.class "dash--disabled dash dash--add overrides__add"
                            , Attr.disabled True
                            ]
                            [ text "Add an override" ]
                        ]
                    ]
    in
    divClass "deployment__widget"
        (addBtn
            ++ [ rowLoader
               , rowLoader
               , rowLoader
               ]
        )


type OverrideTreeType
    = Default
    | Edited
    | New


type alias OverrideTree =
    { name : String
    , value : String
    , type_ : OverrideTreeType
    }


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
                (\i v acc ->
                    if i >= model.lastTreeId then
                        Dict.insert i v acc

                    else
                        acc
                )
                (\_ _ _ acc -> acc)
                (\_ _ acc -> acc)
                model.editedOverrides
                defaultOverrides
                Dict.empty

        treeOverrides =
            Dict.merge
                (\i v acc ->
                    case ( v.value, i < model.lastTreeId ) of
                        ( ValueAdded x, True ) ->
                            Dict.insert i (OverrideTree (unOverrideName v.name) x New) acc

                        _ ->
                            acc
                )
                (\i _ d acc -> Dict.insert i (OverrideTree (unOverrideName d.name) d.value Edited) acc)
                (\i d acc -> Dict.insert i (OverrideTree (unOverrideName d.name) d.value Default) acc)
                model.editedOverrides
                defaultOverrides
                Dict.empty

        editedPaths =
            Dict.values treeOverrides
                |> List.filter (\o -> o.type_ == Edited || o.type_ == New)
                |> List.map (.name >> String.split "." >> List.reverse)
                |> List.map (List.tail >> Maybe.withDefault [] >> List.Extra.inits)
                |> List.concat
                |> Set.fromList

        addButton =
            case model.mode of
                Write ->
                    [ if hasEmptyOverrides newOverrides then
                        Html.button [ Attr.class "dash--disabled dash dash--add overrides__add", Attr.disabled True ] [ text "Add an override" ]

                      else
                        buttonClass "dash dash--add overrides__add" AddOverride [ text "Add an override" ]
                    ]

                Read ->
                    []
    in
    Debug.log (Debug.toString editedPaths) <|
        divClass "deployment__widget"
            [ divClass "padded"
                (addButton ++ newOverridesView model newOverrides keys ++ overridesLevelView model treeOverrides keys editedPaths)
            ]


newOverridesView : Model -> Dict Int Override -> List String -> List (Html Msg)
newOverridesView model overrides keys =
    let
        overridesSorted =
            overrides
                |> Dict.toList
                |> List.sortBy (\( ix, _ ) -> ix)
                |> List.reverse
                |> List.filterMap overrideValue

        overrideValue ( ix, o ) =
            case o.value of
                ValueAdded v ->
                    Just ( ix, unOverrideName o.name, v )

                ValueDeleted ->
                    Nothing

        newOverrideWriteView_ ( ix, oName, oValue ) =
            newOverrideWriteView model oName oValue ix keys
    in
    case model.mode of
        Write ->
            List.map newOverrideWriteView_ overridesSorted

        Read ->
            []


overrideValueWriteView : Model -> String -> Bool -> String -> Int -> Html Msg
overrideValueWriteView model inputClass deleted overrideValue ix =
    let
        valueClass =
            "editable-row__value input"
                ++ (if deleted then
                        " input--deleted"

                    else
                        ""
                   )

        valueClass_ =
            if not deleted && overrideValue == "" then
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
            , Attr.disabled deleted
            , Attr.value overrideValue
            , onInput (EditOverrideValue ix)
            ]
            []
            :: (if not deleted && overrideValue == "" then
                    [ divClass "input__output" [ text "Value can not be empty" ] ]

                else
                    []
               )
        )


overridePathWriteView : Model -> String -> Bool -> String -> List String -> Int -> Html Msg
overridePathWriteView model inputClass deleted overrideName keys ix =
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

        nameClass =
            "editable-row__value input"
                ++ (if deleted then
                        " input--deleted"

                    else
                        ""
                   )

        inputPath =
            Html.input
                [ Attr.class inputClass
                , Attr.placeholder "value"
                , Attr.spellcheck False
                , Attr.type_ "text"
                , Attr.value overrideName
                , Attr.disabled deleted
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
    divClass nameClass
        (inputPath
            :: suggestionsView
        )


defaultOverrideWriteView : Model -> String -> String -> Int -> List String -> Html Msg
defaultOverrideWriteView =
    overrideWriteView
        "input__widget key-default-pristine"
        "input__widget value-pristine"
        False


editedOverrideWriteView : Model -> String -> String -> Int -> List String -> Html Msg
editedOverrideWriteView =
    overrideWriteView
        "input__widget key-default-edited"
        "input__widget value-edited"
        False


newOverrideWriteView : Model -> String -> String -> Int -> List String -> Html Msg
newOverrideWriteView =
    overrideWriteView
        "input__widget key-custom-pristine"
        "input__widget value-edited"
        False


deletedOverrideWriteView : Model -> String -> String -> Int -> List String -> Html Msg
deletedOverrideWriteView =
    overrideWriteView
        "input__widget key-deleted"
        "input__widget value-deleted"
        True


overrideWriteView : String -> String -> Bool -> Model -> String -> String -> Int -> List String -> Html Msg
overrideWriteView nameInputClass valueInputClass deleted model nameInput valueInput ix keys =
    let
        ( btnClass, btnMsg ) =
            if deleted then
                ( "editable-row__delete spot spot--undo", RestoreOverride ix )

            else
                ( "editable-row__delete spot spot--cancel", DeleteOverride ix )
    in
    divClass "row"
        [ divClass "editable-row"
            [ overridePathWriteView model
                nameInputClass
                deleted
                nameInput
                keys
                ix
            , overrideValueWriteView
                model
                valueInputClass
                deleted
                valueInput
                ix
            , buttonClass btnClass btnMsg []
            ]
        ]


overrideReadView : String -> String -> String -> String -> Html Msg
overrideReadView nameClass valueClass nameInput valueInput =
    divClass "row"
        [ spanClass nameClass [ text (nameInput ++ ": ") ]
        , spanClass valueClass [ text valueInput ]
        ]


overridesLevelView : Model -> Dict Int OverrideTree -> List String -> Set (List String) -> List (Html Msg)
overridesLevelView model defaultOverrides keys editedPaths =
    let
        hasEmptyOrEdited t =
            case t of
                Tree.Node _ cs ->
                    List.any hasEmptyOrEdited cs

                Tree.Leaf ( _, o ) ->
                    o.value == "" || o.type_ == New || o.type_ == Edited

        overrideCompare a b =
            case ( a, b ) of
                ( Tree.Node x [ Tree.Leaf ( _, xx ) ], Tree.Node y [ Tree.Leaf ( _, yy ) ] ) ->
                    case ( xx.type_, yy.type_ ) of
                        ( New, Edited ) ->
                            LT

                        ( Edited, New ) ->
                            GT

                        ( New, Default ) ->
                            LT

                        ( Default, New ) ->
                            GT

                        ( Edited, Default ) ->
                            LT

                        ( Default, Edited ) ->
                            GT

                        _ ->
                            compare x y

                ( Tree.Node _ [ Tree.Leaf _ ], _ ) ->
                    LT

                ( _, Tree.Node _ [ Tree.Leaf _ ] ) ->
                    GT

                ( Tree.Node x _, Tree.Node y _ ) ->
                    case ( hasEmptyOrEdited a, hasEmptyOrEdited b ) of
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
                |> List.map (\( ix, o ) -> Tree.pathToTree (String.split "." o.name) ( ix, o ))
                |> mergeTreesWithSort overrideCompare
    in
    List.map
        (treeOverrideView model keys [] editedPaths)
        overridesTree


overrideWriteWrapper : Model -> List String -> Int -> Maybe Override -> OverrideTree -> Html Msg
overrideWriteWrapper model keys ix override overrideTree =
    case override of
        Just o ->
            case o.value of
                ValueAdded v ->
                    editedOverrideWriteView model overrideTree.name v ix keys

                ValueDeleted ->
                    deletedOverrideWriteView model overrideTree.name overrideTree.value ix keys

        Nothing ->
            defaultOverrideWriteView model overrideTree.name overrideTree.value ix keys


overrideReadWrapper : Maybe Override -> OverrideTree -> Html Msg
overrideReadWrapper override overrideTree =
    case override of
        Just o ->
            case o.value of
                ValueAdded v ->
                    editedOverrideReadView overrideTree.name v

                ValueDeleted ->
                    deletedOverrideReadView overrideTree.name overrideTree.value

        Nothing ->
            defaultOverrideReadView overrideTree.name overrideTree.value


editedOverrideReadView : String -> String -> Html Msg
editedOverrideReadView =
    overrideReadView
        "key-default-edited"
        "value-edited"


deletedOverrideReadView : String -> String -> Html Msg
deletedOverrideReadView =
    overrideReadView
        "key-deleted"
        "value-deleted"


defaultOverrideReadView : String -> String -> Html Msg
defaultOverrideReadView =
    overrideReadView
        "key-default-pristine"
        "value-pristine"


newOverrideReadView : String -> String -> Html Msg
newOverrideReadView =
    overrideReadView
        "key-custom-edited"
        "value-edited"


treeOverrideView : Model -> List String -> List String -> Set (List String) -> Tree ( Int, OverrideTree ) -> Html Msg
treeOverrideView model keys piecies editedPaths tree =
    case tree of
        Tree.Node _ [ Tree.Leaf ( ix, override ) ] ->
            case model.mode of
                Write ->
                    overrideWriteWrapper model keys ix (Dict.get ix model.editedOverrides) override

                Read ->
                    overrideReadWrapper (Dict.get ix model.editedOverrides) override

        Tree.Node piece cs ->
            let
                btnClass =
                    if Set.member (piece :: piecies) editedPaths then
                        "collapse__head collapse__head--has-changes"

                    else
                        "collapse__head"
            in
            if Set.member (piece :: piecies) model.openedPaths then
                divClass "collapse--project collapse collapse--expanded"
                    [ buttonClass btnClass
                        (ClosePath (piece :: piecies))
                        [ text piece ]
                    , divClass "collapse__inner"
                        (List.map (treeOverrideView model keys (piece :: piecies) editedPaths) cs)
                    ]

            else
                divClass "collapse--project collapse"
                    [ buttonClass btnClass
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
