module Types.Override exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, string)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (optional, required, resolve)
import Json.Encode as Encode
import Time


type OverrideName
    = OverrideName String


unOverrideName : OverrideName -> String
unOverrideName (OverrideName name) =
    name


overrideNameDecoder : Decoder OverrideName
overrideNameDecoder =
    Decode.map OverrideName Decode.string


type OverrideValue
    = ValueAdded String
    | ValueDeleted


getOverrideValue : Override -> Maybe String
getOverrideValue override =
    case override.value of
        ValueAdded x ->
            Just x

        ValueDeleted ->
            Nothing


type alias Override =
    { name : OverrideName
    , value : OverrideValue
    }


type OverrideHelper
    = OverrideName_ String
    | OverrideValue_ OverrideValue


overrideValueDecoder : Decoder OverrideValue
overrideValueDecoder =
    let
        toDecoder : String -> Maybe String -> Decoder OverrideValue
        toDecoder tag contents =
            case ( tag, contents ) of
                ( "ValueDeleted", _ ) ->
                    Decode.succeed ValueDeleted

                ( "ValueAdded", Just value ) ->
                    Decode.succeed (ValueAdded value)

                ( _, _ ) ->
                    Decode.fail ("Unknown DeploymentStatus: " ++ tag)
    in
    Decode.succeed toDecoder
        |> required "tag" string
        |> optional "contents" (Decode.maybe string) Nothing
        |> resolve


overrideToHelper : Override -> List OverrideHelper
overrideToHelper override =
    [ OverrideName_ (unOverrideName override.name)
    , OverrideValue_ override.value
    ]


overrideDecoder : Decoder Override
overrideDecoder =
    let
        overrideHelperDecoder =
            Decode.list
                (Decode.oneOf
                    [ string |> Decode.map OverrideName_
                    , overrideValueDecoder |> Decode.map OverrideValue_
                    ]
                )

        listDecoder vals =
            case vals of
                [ OverrideName_ name, OverrideValue_ val ] ->
                    Decode.succeed (Override (OverrideName name) val)

                [ OverrideValue_ val, OverrideName_ name ] ->
                    Decode.succeed (Override (OverrideName name) val)

                _ ->
                    Decode.fail "Unknown override"
    in
    overrideHelperDecoder |> andThen listDecoder


overrideValueEncode : OverrideValue -> Encode.Value
overrideValueEncode value =
    case value of
        ValueAdded v ->
            Encode.object
                [ ( "tag", Encode.string "ValueAdded" )
                , ( "contents", Encode.string v )
                ]

        ValueDeleted ->
            Encode.object
                [ ( "tag", Encode.string "ValueDeleted" ) ]


overrideHelperEncode : OverrideHelper -> Encode.Value
overrideHelperEncode helper =
    case helper of
        OverrideName_ name ->
            Encode.string name

        OverrideValue_ val ->
            overrideValueEncode val


overrideEncode : Override -> Encode.Value
overrideEncode override =
    let
        helper =
            overrideToHelper override
    in
    Encode.list overrideHelperEncode helper
