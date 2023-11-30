module Page.Sidebar.Overrides exposing (..)

import Api
import Api.Endpoint exposing (..)
import Deployments exposing (..)
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


type alias OverrideData =
    { path : String
    , value : String
    }


emptyOverride : OverrideData
emptyOverride =
    OverrideData "" ""


type alias Model =
    { defaultOverrides : Api.WebData (Dict Int OverrideData)
    , keys : Api.WebData (List String)
    , editedOverrides : Dict Int (Maybe OverrideData)
    , nextId : Int
    , openedPaths : Set (List String)
    , autocomplete : Maybe Int
    , name : String
    }


init : String -> Model
init name =
    Model Loading Loading Dict.empty 0 Set.empty Nothing name


setDefaultOverrides : Api.WebData (List (List String)) -> Model -> Model
setDefaultOverrides defaultOverrides model =
    let
        emptyOverrides overrides =
            Dict.toList overrides
                |> List.filter (\( _, o ) -> o.value == "")
                |> List.map (Tuple.second >> .path >> String.split ".")
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

        dictOverrides =
            RemoteData.map overridesToDict defaultOverrides
    in
    { model
        | defaultOverrides = dictOverrides
        , nextId = RemoteData.unwrap 0 Dict.size dictOverrides
        , openedPaths = RemoteData.unwrap Set.empty emptyOverrides dictOverrides
    }


getFullOverrides : Model -> List (List String)
getFullOverrides model =
    case model.defaultOverrides of
        Success defaultOverrides ->
            Dict.merge
                (\i a acc -> Dict.insert i a acc)
                (\i a _ acc -> Dict.insert i a acc)
                (\i b acc -> Dict.insert i (Just b) acc)
                model.editedOverrides
                defaultOverrides
                Dict.empty
                |> Dict.toList
                |> List.filterMap (\( i, mv ) -> Maybe.map (\v -> ( i, v )) mv)
                |> Dict.fromList
                |> dictToOverrides

        _ ->
            []


getEditedOverrides : Model -> List Deployments.Override
getEditedOverrides model =
    let
        overrideDataToOverride ( i, mOd ) =
            case mOd of
                Just od ->
                    Just
                        { name = OverrideName od.path
                        , value = Deployments.ValueAdded od.value
                        }

                Nothing ->
                    case RemoteData.map (Dict.get i) model.defaultOverrides of
                        Success (Just od) ->
                            Just
                                { name = OverrideName od.path
                                , value = Deployments.ValueDeleted
                                }

                        _ ->
                            Nothing
    in
    model.editedOverrides
        |> Dict.toList
        |> List.filterMap overrideDataToOverride


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
            ( i, OverrideData p v )
    in
    Dict.fromList <| List.indexedMap g <| List.filterMap f raw


dictToOverrides : Dict Int OverrideData -> List (List String)
dictToOverrides =
    let
        f ( _, b ) =
            [ b.path, b.value ]
    in
    Dict.toList
        >> List.map f


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


changeData : Msg -> Bool
changeData msg =
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
        | editedOverrides = Dict.insert model.nextId (Just emptyOverride) model.editedOverrides
        , nextId = model.nextId + 1
    }


deleteOverride : Int -> Model -> Model
deleteOverride ix model =
    let
        delete _ =
            case RemoteData.map (Dict.get ix) model.defaultOverrides of
                Success (Just _) ->
                    Just Nothing

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
                ( Just (Just override), Success (Just default) ) ->
                    if { override | path = path } == default then
                        Nothing

                    else
                        Just (Just { override | path = path })

                ( Just (Just override), Success Nothing ) ->
                    Just (Just { override | path = path })

                ( _, Success (Just default) ) ->
                    if default.path == path then
                        Nothing

                    else
                        Just (Just { default | path = path })

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix editPath model.editedOverrides }


editOverrideValue : Int -> String -> Model -> Model
editOverrideValue ix value model =
    let
        editPath mOverride =
            case ( mOverride, RemoteData.map (Dict.get ix) model.defaultOverrides ) of
                ( Just (Just override), Success (Just default) ) ->
                    if { override | value = value } == default then
                        Nothing

                    else
                        Just (Just { override | value = value })

                ( Just (Just override), Success Nothing ) ->
                    Just (Just { override | value = value })

                ( _, Success (Just default) ) ->
                    if default.value == value then
                        Nothing

                    else
                        Just (Just { default | value = value })

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix editPath model.editedOverrides }


view : Model -> Html Msg
view model =
    divClass "deployment__section"
        [ h3Class "deployment__sub-heading" [ text model.name ]
        , case ( model.defaultOverrides, model.keys ) of
            ( Success defaultOverrides, Success keys ) ->
                overridesSectionData model defaultOverrides keys

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


hasEmptyOverrides : List ( Int, Maybe OverrideData ) -> Bool
hasEmptyOverrides overrides =
    overrides
        |> List.filter (\( _, x ) -> Maybe.map (\y -> y.path == "") x |> Maybe.withDefault False)
        |> (List.isEmpty >> not)


overridesSectionData : Model -> Dict Int OverrideData -> List String -> Html Msg
overridesSectionData model defaultOverrides keys =
    let
        newOverrideIds =
            Set.diff (Set.fromList (Dict.keys model.editedOverrides)) (Set.fromList (Dict.keys defaultOverrides))

        newOverrides =
            Set.foldl (\ix acc -> Maybe.map (\x -> ( ix, x )) (Dict.get ix model.editedOverrides) :: acc) [] newOverrideIds
                |> List.filterMap (\x -> x)

        addButton =
            if hasEmptyOverrides newOverrides then
                Html.button [ Attr.class "dash--disabled dash dash--add overrides__add", Attr.disabled True ] [ text "Add an override" ]

            else
                buttonClass "dash dash--add overrides__add" AddOverride [ text "Add an override" ]
    in
    divClass "deployment__widget"
        [ divClass "padded"
            (addButton
                :: (newOverridesView model newOverrides keys ++ overridesLevelView model defaultOverrides keys)
            )
        ]


newOverridesView : Model -> List ( Int, Maybe OverrideData ) -> List String -> List (Html Msg)
newOverridesView model overrides keys =
    let
        overridesSorted =
            overrides
                |> List.sortBy (\( ix, _ ) -> ix)
                |> List.filterMap (\( ix, m ) -> Maybe.map (\x -> ( ix, x )) m)
                |> List.reverse
    in
    List.map
        (newOverrideView model keys)
        overridesSorted


newOverrideView : Model -> List String -> ( Int, OverrideData ) -> Html Msg
newOverrideView =
    overrideView
        "input editable-row__key"
        "input__widget key-custom-edited"
        "input editable-row__value"
        "input__widget value-edited"


overrideValueView : Model -> String -> String -> List String -> ( Int, OverrideData ) -> Html Msg
overrideValueView _ valueClass inputClass _ ( ix, overrideData ) =
    let
        valueClass_ =
            if overrideData.value == "" then
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
            , Attr.value overrideData.value
            , onInput (EditOverrideValue ix)
            ]
            []
            :: (if overrideData.value == "" then
                    [ divClass "input__output" [ text "Value can not be empty" ] ]

                else
                    []
               )
        )


overridePathView : Model -> String -> String -> List String -> ( Int, OverrideData ) -> Html Msg
overridePathView model pathClass inputClass keys ( ix, overrideData ) =
    let
        suggestions =
            List.filter (String.startsWith overrideData.path) keys

        suggestionItem suggestion =
            Html.li
                [ Attr.class "input__suggest"
                , onMouseDown (EditOverridePath ix suggestion)
                ]
                [ b [] [ text overrideData.path ]
                , text (String.dropLeft (String.length overrideData.path) suggestion)
                ]

        inputPath =
            Html.input
                [ Attr.class inputClass
                , Attr.placeholder "value"
                , Attr.spellcheck False
                , Attr.type_ "text"
                , Attr.value overrideData.path
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


overrideView : String -> String -> String -> String -> Model -> List String -> ( Int, OverrideData ) -> Html Msg
overrideView pathClass pathInputClass valueClass valueInputClass model keys ( ix, overrideData ) =
    divClass "row"
        [ divClass "editable-row"
            [ overridePathView model pathClass pathInputClass keys ( ix, overrideData )
            , overrideValueView model valueClass valueInputClass keys ( ix, overrideData )
            , buttonClass "editable-row__delete spot spot--cancel" (DeleteOverride ix) []
            ]
        ]


overrideDeletedView : Model -> List String -> ( Int, OverrideData ) -> Html Msg
overrideDeletedView _ _ ( ix, overrideData ) =
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
                    , Attr.value overrideData.value
                    ]
                    []
                ]
            , buttonClass "editable-row__delete spot spot--undo" (RestoreOverride ix) []
            ]
        ]


overridesLevelView : Model -> Dict Int OverrideData -> List String -> List (Html Msg)
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
                |> List.map (\( ix, a ) -> Tree.pathToTree (String.split "." a.path) ( ix, a ))
                |> mergeTreesWithSort overrideCompare
    in
    List.map
        (treeOverrideView model keys [])
        overridesTree


defaultOverrideView : Model -> List String -> ( Int, OverrideData ) -> Html Msg
defaultOverrideView =
    overrideView
        "input editable-row__key"
        "input__widget key-default-pristine"
        "input editable-row__value"
        "input__widget value-pristine"


editedOverrideView : Model -> List String -> ( Int, OverrideData ) -> Html Msg
editedOverrideView =
    overrideView
        "input editable-row__key"
        "input__widget key-default-edited"
        "input editable-row__value"
        "input__widget value-edited"


treeOverrideView : Model -> List String -> List String -> Tree ( Int, OverrideData ) -> Html Msg
treeOverrideView model keys piecies defaultOverride =
    case defaultOverride of
        Tree.Node _ [ Tree.Leaf ( ix, default ) ] ->
            case Dict.get ix model.editedOverrides of
                Just (Just edited) ->
                    editedOverrideView model keys ( ix, edited )

                Just Nothing ->
                    overrideDeletedView model keys ( ix, default )

                Nothing ->
                    defaultOverrideView model keys ( ix, default )

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
    case model.defaultOverrides of
        Success defaultOverrides ->
            Dict.merge
                (\i a acc -> Dict.insert i a acc)
                (\i a _ acc -> Dict.insert i a acc)
                (\i b acc -> Dict.insert i (Just b) acc)
                model.editedOverrides
                defaultOverrides
                Dict.empty
                |> Dict.toList
                |> List.filterMap (\( _, mv ) -> mv)
                |> List.filter (\v -> v.value == "")
                |> (List.isEmpty >> not)

        _ ->
            False
