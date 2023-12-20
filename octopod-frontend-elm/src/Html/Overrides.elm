module Html.Overrides exposing
    ( Mode(..)
    , Model
    , Msg
    , dataChanged
    , getEditedOverrides
    , getFullOverrides
    , hasEmptyValues
    , init
    , overridesSectionLoading
    , update
    , view
    )

import Api
import Api.Endpoint exposing (..)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Common exposing (..)
import Html.Events exposing (onBlur, onFocus, onInput, onMouseDown)
import List exposing (reverse)
import List.Extra
import RemoteData exposing (RemoteData(..))
import Set exposing (Set)
import Time exposing (Month(..))
import Tree exposing (Tree, mergeTreesWithSort)
import Types.Override exposing (..)
import Types.OverrideWithDefault exposing (..)



-- MODEL


type Mode
    = Read
    | Write


type OverrideDataStatus
    = New
    | Edited
    | Deleted


type alias OverrideData =
    { name : OverrideName
    , value : String
    , status : OverrideDataStatus
    }


type WrappedOverride
    = DefaultOverride OverrideWithDefault
    | NonOverrideWithDefault OverrideData


type alias Model =
    { keys : List OverrideName
    , treeSchema : List (Tree Int)
    , defaultOverrides : Dict Int OverrideWithDefault
    , editedOverrides : Dict Int OverrideData
    , nextId : Int
    , openedNames : Set (List String)
    , autocomplete : Maybe Int
    , name : String
    , mode : Mode
    }



-- init : String -> Mode -> Model
-- init name mode =
--     { treeSchema = []
--     , keys = []
--     , defaultOverrides = Dict.empty
--     , editedOverrides = Dict.empty
--     , nextId = 0
--     , openedNames = Set.empty
--     , autocomplete = Nothing
--     , name = name
--     , mode = mode
--     }


init : List OverrideWithDefault -> List Override -> List OverrideName -> Mode -> String -> Model
init defaults edits keys mode name =
    let
        defaultNameDict =
            mkDefaultNameDict defaults

        editNameDict =
            mkEditsNameDict edits

        ids =
            mkIds defaultNameDict editNameDict

        defaultsDict =
            mkDefaultsDict ids defaultNameDict

        overridesDict =
            mkEditsDict ids editNameDict
                |> mkOverrideDataList defaultsDict

        treeData =
            mkWrappedOverride defaultsDict overridesDict
    in
    { treeSchema = mkTree ids treeData
    , defaultOverrides = defaultsDict
    , editedOverrides = overridesDict
    , nextId = Dict.size defaultsDict + Dict.size overridesDict
    , openedNames = mkOpenedPaths treeData
    , name = name
    , mode = mode
    , keys = keys
    , autocomplete = Nothing
    }


getFullOverrides : Model -> List OverrideWithDefault
getFullOverrides model =
    Dict.merge
        (\_ e res ->
            case e.status of
                New ->
                    { name = e.name, value = e.value } :: res

                _ ->
                    res
        )
        (\_ e _ res ->
            case e.status of
                Edited ->
                    { name = e.name, value = e.value } :: res

                _ ->
                    res
        )
        (\_ d res ->
            d :: res
        )
        model.editedOverrides
        model.defaultOverrides
        []


getEditedOverrides : Model -> List Override
getEditedOverrides model =
    model.editedOverrides
        |> Dict.values
        |> List.map
            (\o ->
                case o.status of
                    Deleted ->
                        Override o.name ValueDeleted

                    New ->
                        Override o.name (ValueAdded o.value)

                    Edited ->
                        Override o.name (ValueAdded o.value)
            )


hasEmptyValues : Model -> Bool
hasEmptyValues model =
    getFullOverrides model
        |> List.filter (\v -> v.value == "")
        |> (List.isEmpty >> not)



-- UPDATE


type Msg
    = AddOverride
    | DeleteOverride Int
    | RestoreOverride Int
    | EditOverrideName Int String
    | EditOverrideValue Int String
    | OpenName (List String)
    | CloseName (List String)
    | ShowAutocomplete Int
    | HideAutocomplete


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddOverride ->
            ( newOverride model, Cmd.none )

        DeleteOverride ix ->
            ( deleteOverride ix model, Cmd.none )

        RestoreOverride ix ->
            ( restoreOverride ix model, Cmd.none )

        EditOverrideName ix name ->
            ( editOverrideName ix name model, Cmd.none )

        EditOverrideValue ix value ->
            ( editOverrideValue ix value model, Cmd.none )

        OpenName name ->
            ( { model | openedNames = Set.insert name model.openedNames }, Cmd.none )

        CloseName name ->
            ( { model | openedNames = Set.remove name model.openedNames }, Cmd.none )

        ShowAutocomplete ix ->
            ( { model | autocomplete = Just ix }, Cmd.none )

        HideAutocomplete ->
            ( { model | autocomplete = Nothing }, Cmd.none )


newOverride : Model -> Model
newOverride model =
    { model
        | editedOverrides = Dict.insert model.nextId (OverrideData (OverrideName "") "" New) model.editedOverrides
        , nextId = model.nextId + 1
    }


deleteOverride : Int -> Model -> Model
deleteOverride ix model =
    let
        delete mOverride =
            case ( mOverride, Dict.get ix model.defaultOverrides ) of
                ( Nothing, Just default ) ->
                    Just { name = default.name, value = default.value, status = Deleted }

                ( Just override, Nothing ) ->
                    Just { override | status = Deleted }

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix delete model.editedOverrides }


restoreOverride : Int -> Model -> Model
restoreOverride ix model =
    let
        restore mOverride =
            case ( mOverride, Dict.get ix model.defaultOverrides ) of
                ( Just override, Nothing ) ->
                    Just { override | status = New }

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix restore model.editedOverrides }


editOverrideName : Int -> String -> Model -> Model
editOverrideName ix name model =
    let
        editName mOverride =
            case ( mOverride, Dict.get ix model.defaultOverrides ) of
                ( Just override, Just default ) ->
                    if OverrideName name == default.name && override.value == default.value then
                        Nothing

                    else
                        Just { override | name = OverrideName name, status = Edited }

                ( Just override, Nothing ) ->
                    Just { override | name = OverrideName name, status = New }

                ( Nothing, Just default ) ->
                    if OverrideName name == default.name then
                        Nothing

                    else
                        Just { name = OverrideName name, value = default.value, status = Edited }

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix editName model.editedOverrides }


editOverrideValue : Int -> String -> Model -> Model
editOverrideValue ix value model =
    let
        editName mOverride =
            case ( mOverride, Dict.get ix model.defaultOverrides ) of
                ( Just override, Just default ) ->
                    if value == default.value && override.name == default.name then
                        Nothing

                    else
                        Just { override | value = value, status = Edited }

                ( Just override, Nothing ) ->
                    Just { override | value = value, status = New }

                ( Nothing, Just default ) ->
                    if value == default.value then
                        Nothing

                    else
                        Just { name = default.name, value = value, status = Edited }

                _ ->
                    Nothing
    in
    { model | editedOverrides = Dict.update ix editName model.editedOverrides }



-- VIEW


view : Model -> Html Msg
view model =
    divClass "deployment__section"
        [ h3Class "deployment__sub-heading" [ text model.name ]
        , overridesSectionData model
        ]


overridesSectionLoading : Mode -> Html Msg
overridesSectionLoading mode =
    let
        rowLoader =
            divClass "editable-row loader"
                [ divClass "editable-row__placeholder" []
                , divClass "editable-row__placeholder" []
                , divClass "overrides__delete spot spot--loader" []
                ]

        addBtn =
            case mode of
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


overridesSectionData : Model -> Html Msg
overridesSectionData model =
    let
        isEmptyOverride _ v =
            v.value == ""

        hasEmptyOverrides overrides =
            overrides
                |> Dict.filter isEmptyOverride
                |> (Dict.isEmpty >> not)

        treeIds =
            List.concatMap Tree.values model.treeSchema |> Set.fromList

        newOverrides =
            Dict.filter (\id _ -> Set.member id treeIds |> not) model.editedOverrides

        addButton =
            case model.mode of
                Write ->
                    [ if hasEmptyOverrides newOverrides then
                        Html.button
                            [ Attr.class "dash--disabled dash dash--add overrides__add"
                            , Attr.disabled True
                            ]
                            [ text "Add an override" ]

                      else
                        buttonClass "dash dash--add overrides__add"
                            AddOverride
                            [ text "Add an override" ]
                    ]

                Read ->
                    []
    in
    divClass "deployment__widget"
        [ divClass "padded"
            (addButton
                ++ newOverridesView model newOverrides
                ++ overridesLevelView model
            )
        ]


newOverridesView : Model -> Dict Int OverrideData -> List (Html Msg)
newOverridesView model overrides =
    let
        overridesSorted =
            overrides
                |> Dict.toList
                |> List.sortBy (\( ix, _ ) -> ix)
                |> List.reverse

        newOverrideWriteView_ ( ix, override ) =
            overrideWriteWrapper model ix (NonOverrideWithDefault override)
    in
    case model.mode of
        Write ->
            List.map newOverrideWriteView_ overridesSorted

        Read ->
            []


overridesLevelView : Model -> List (Html Msg)
overridesLevelView model =
    let
        treeData =
            mkWrappedOverride model.defaultOverrides model.editedOverrides

        editedNames =
            Dict.toList treeData
                |> List.filter (Tuple.second >> isDefautOverride >> not)
                |> List.map
                    (Tuple.second
                        >> getWrappedName
                        >> unOverrideName
                        >> String.split "."
                        >> List.Extra.inits
                        >> List.map reverse
                    )
                |> List.concat
                |> Set.fromList
    in
    List.map
        (treeOverrideView model treeData editedNames [])
        model.treeSchema


treeOverrideView : Model -> Dict Int WrappedOverride -> Set (List String) -> List String -> Tree Int -> Html Msg
treeOverrideView model treeData editedNames piecies treeSchema =
    case treeSchema of
        Tree.Node _ [ Tree.Leaf ix ] ->
            case ( model.mode, Dict.get ix treeData ) of
                ( Write, Just override ) ->
                    overrideWriteWrapper model ix override

                ( Read, Just override ) ->
                    overrideReadWrapper override

                _ ->
                    div [] []

        Tree.Node piece cs ->
            let
                btnClass =
                    if Set.member (piece :: piecies) editedNames then
                        "collapse__head collapse__head--has-changes"

                    else
                        "collapse__head"
            in
            if Set.member (piece :: piecies) model.openedNames then
                divClass "collapse--project collapse collapse--expanded"
                    [ buttonClass btnClass
                        (CloseName (piece :: piecies))
                        [ text piece ]
                    , divClass "collapse__inner"
                        (List.map (treeOverrideView model treeData editedNames (piece :: piecies)) cs)
                    ]

            else
                divClass "collapse--project collapse"
                    [ buttonClass btnClass
                        (OpenName (piece :: piecies))
                        [ text piece ]
                    , divClass "collapse__inner" []
                    ]

        Tree.Leaf _ ->
            div [] []



-- WRITE VIEW


overrideValueWriteView : Model -> String -> Bool -> String -> Int -> Html Msg
overrideValueWriteView _ inputClass deleted overrideValue ix =
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


overrideNameWriteView : Model -> String -> Bool -> String -> List OverrideName -> Int -> Html Msg
overrideNameWriteView model inputClass deleted overrideName keys ix =
    let
        suggestions =
            List.filter (unOverrideName >> String.startsWith overrideName) keys

        suggestionItem suggestion =
            Html.li
                [ Attr.class "input__suggest"
                , onMouseDown (EditOverrideName ix (unOverrideName suggestion))
                ]
                [ b [] [ text overrideName ]
                , text (String.dropLeft (String.length overrideName) (unOverrideName suggestion))
                ]

        nameClass =
            "editable-row__value input"
                ++ (if deleted then
                        " input--deleted"

                    else
                        ""
                   )

        inputName =
            Html.input
                [ Attr.class inputClass
                , Attr.placeholder "value"
                , Attr.spellcheck False
                , Attr.type_ "text"
                , Attr.value overrideName
                , Attr.disabled deleted
                , onInput (EditOverrideName ix)
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
        (inputName
            :: suggestionsView
        )


defaultOverrideWriteView : Model -> String -> String -> Int -> Html Msg
defaultOverrideWriteView =
    overrideWriteView
        "input__widget key-default-pristine"
        "input__widget value-pristine"
        False


editedOverrideWriteView : Model -> String -> String -> Int -> Html Msg
editedOverrideWriteView =
    overrideWriteView
        "input__widget key-default-edited"
        "input__widget value-edited"
        False


newOverrideWriteView : Model -> String -> String -> Int -> Html Msg
newOverrideWriteView =
    overrideWriteView
        "input__widget key-custom-pristine"
        "input__widget value-edited"
        False


deletedOverrideWriteView : Model -> String -> String -> Int -> Html Msg
deletedOverrideWriteView =
    overrideWriteView
        "input__widget key-deleted"
        "input__widget value-deleted"
        True


overrideWriteWrapper : Model -> Int -> WrappedOverride -> Html Msg
overrideWriteWrapper model ix wrapped =
    let
        ( viewer, overrideName, overrideValue ) =
            case wrapped of
                DefaultOverride override ->
                    ( defaultOverrideWriteView, override.name, override.value )

                NonOverrideWithDefault override ->
                    case override.status of
                        Edited ->
                            ( editedOverrideWriteView, override.name, override.value )

                        Deleted ->
                            ( deletedOverrideWriteView, override.name, override.value )

                        New ->
                            ( newOverrideWriteView, override.name, override.value )
    in
    viewer model (unOverrideName overrideName) overrideValue ix


overrideWriteView : String -> String -> Bool -> Model -> String -> String -> Int -> Html Msg
overrideWriteView nameInputClass valueInputClass deleted model nameInput valueInput ix =
    let
        ( btnClass, btnMsg ) =
            if deleted then
                ( "editable-row__delete spot spot--undo", RestoreOverride ix )

            else
                ( "editable-row__delete spot spot--cancel", DeleteOverride ix )
    in
    divClass "row"
        [ divClass "editable-row"
            [ overrideNameWriteView model
                nameInputClass
                deleted
                nameInput
                model.keys
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



-- READ VIEW


overrideReadView : String -> String -> String -> String -> Html Msg
overrideReadView nameClass valueClass nameInput valueInput =
    divClass "row"
        [ spanClass nameClass [ text (nameInput ++ ": ") ]
        , spanClass valueClass [ text valueInput ]
        ]


overrideReadWrapper : WrappedOverride -> Html Msg
overrideReadWrapper wrapped =
    case wrapped of
        DefaultOverride override ->
            defaultOverrideReadView (unOverrideName override.name) override.value

        NonOverrideWithDefault override ->
            case override.status of
                Edited ->
                    editedOverrideReadView (unOverrideName override.name) override.value

                Deleted ->
                    deletedOverrideReadView (unOverrideName override.name) override.value

                New ->
                    newOverrideReadView (unOverrideName override.name) override.value


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



-- UTILS


compareOverrideData : OverrideData -> OverrideData -> Order
compareOverrideData a b =
    case ( a.status, b.status ) of
        ( Deleted, Deleted ) ->
            compare (unOverrideName a.name) (unOverrideName b.name)

        ( New, New ) ->
            compare (unOverrideName a.name) (unOverrideName b.name)

        ( Edited, Edited ) ->
            compare (unOverrideName a.name) (unOverrideName b.name)

        ( Deleted, _ ) ->
            LT

        ( _, Deleted ) ->
            GT

        ( New, _ ) ->
            LT

        ( _, New ) ->
            GT


isDefautOverride : WrappedOverride -> Bool
isDefautOverride w =
    case w of
        DefaultOverride _ ->
            True

        _ ->
            False


getWrappedValue : WrappedOverride -> String
getWrappedValue w =
    case w of
        DefaultOverride x ->
            x.value

        NonOverrideWithDefault x ->
            x.value


getWrappedName : WrappedOverride -> OverrideName
getWrappedName w =
    case w of
        DefaultOverride x ->
            x.name

        NonOverrideWithDefault x ->
            x.name


mkWrappedOverride : Dict Int OverrideWithDefault -> Dict Int OverrideData -> Dict Int WrappedOverride
mkWrappedOverride defaults edits =
    Dict.merge
        (\id d res -> Dict.insert id (DefaultOverride d) res)
        (\id _ e res -> Dict.insert id (NonOverrideWithDefault e) res)
        (\id e res -> Dict.insert id (NonOverrideWithDefault e) res)
        defaults
        edits
        Dict.empty


compareWrappedOverride : WrappedOverride -> WrappedOverride -> Order
compareWrappedOverride a b =
    case ( a, b ) of
        ( NonOverrideWithDefault x, NonOverrideWithDefault y ) ->
            compareOverrideData x y

        ( NonOverrideWithDefault _, _ ) ->
            LT

        ( _, NonOverrideWithDefault _ ) ->
            GT

        ( DefaultOverride x, DefaultOverride y ) ->
            compare (unOverrideName x.name) (unOverrideName y.name)


dataChanged : Msg -> Bool
dataChanged msg =
    case msg of
        DeleteOverride _ ->
            True

        RestoreOverride _ ->
            True

        EditOverrideName _ _ ->
            True

        EditOverrideValue _ _ ->
            True

        _ ->
            False


mkDefaultNameDict : List OverrideWithDefault -> Dict String OverrideWithDefault
mkDefaultNameDict defaultsList =
    defaultsList |> List.map (\d -> ( unOverrideName d.name, d )) |> Dict.fromList


mkEditsNameDict : List Override -> Dict String Override
mkEditsNameDict editsList =
    editsList |> List.map (\d -> ( unOverrideName d.name, d )) |> Dict.fromList


mkIds : Dict String OverrideWithDefault -> Dict String Override -> Dict String Int
mkIds defaults edits =
    Dict.merge
        (\n _ res -> Dict.insert n (Dict.size res) res)
        (\n _ _ res -> Dict.insert n (Dict.size res) res)
        (\n _ res -> Dict.insert n (Dict.size res) res)
        defaults
        edits
        Dict.empty


mkDefaultsDict : Dict String Int -> Dict String OverrideWithDefault -> Dict Int OverrideWithDefault
mkDefaultsDict ids defaults =
    Dict.merge
        (\_ _ res -> res)
        (\_ id def res -> Dict.insert id def res)
        (\_ _ res -> res)
        ids
        defaults
        Dict.empty


mkEditsDict : Dict String Int -> Dict String Override -> Dict Int Override
mkEditsDict ids edits =
    Dict.merge
        (\_ _ res -> res)
        (\_ id def res -> Dict.insert id def res)
        (\_ _ res -> res)
        ids
        edits
        Dict.empty


mkOverrideDataList : Dict Int OverrideWithDefault -> Dict Int Override -> Dict Int OverrideData
mkOverrideDataList defaults edits =
    Dict.merge
        (\_ _ res -> res)
        (\id def edit res ->
            case edit.value of
                ValueAdded v ->
                    Dict.insert id (OverrideData edit.name v Edited) res

                ValueDeleted ->
                    Dict.insert id (OverrideData edit.name def.value Deleted) res
        )
        (\id edit res ->
            case edit.value of
                ValueAdded v ->
                    Dict.insert id (OverrideData edit.name v New) res

                ValueDeleted ->
                    res
        )
        defaults
        edits
        Dict.empty


treeHasEmptyOrEdited : Dict Int WrappedOverride -> Tree Int -> Bool
treeHasEmptyOrEdited overrideData t =
    case t of
        Tree.Node _ cs ->
            List.any (treeHasEmptyOrEdited overrideData) cs

        Tree.Leaf i ->
            case Dict.get i overrideData of
                Just (NonOverrideWithDefault _) ->
                    True

                Just (DefaultOverride x) ->
                    x.value == ""

                _ ->
                    False


compareTree : Dict Int WrappedOverride -> Tree Int -> Tree Int -> Order
compareTree overrideData a b =
    case ( a, b ) of
        ( Tree.Node x [ Tree.Leaf i ], Tree.Node y [ Tree.Leaf j ] ) ->
            case ( Dict.get i overrideData, Dict.get j overrideData ) of
                ( Just aa, Just bb ) ->
                    compareWrappedOverride aa bb

                _ ->
                    compare x y

        ( Tree.Node _ [ Tree.Leaf _ ], _ ) ->
            LT

        ( _, Tree.Node _ [ Tree.Leaf _ ] ) ->
            GT

        ( Tree.Node x _, Tree.Node y _ ) ->
            case ( treeHasEmptyOrEdited overrideData a, treeHasEmptyOrEdited overrideData b ) of
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


mkTree : Dict String Int -> Dict Int WrappedOverride -> List (Tree Int)
mkTree ids overridesData =
    Dict.toList ids
        |> List.map (\( n, id ) -> Tree.pathToTree (String.split "." n) id)
        |> mergeTreesWithSort (compareTree overridesData)


mkOpenedPaths : Dict Int WrappedOverride -> Set (List String)
mkOpenedPaths overridesData =
    Dict.toList overridesData
        |> List.map Tuple.second
        |> List.filter (getWrappedValue >> (\x -> x == ""))
        |> List.map
            (getWrappedName
                >> unOverrideName
                >> String.split "."
                >> List.Extra.init
                >> Maybe.withDefault []
                >> List.reverse
                >> List.Extra.tails
            )
        |> List.concat
        |> Set.fromList
