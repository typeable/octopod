module Types.OverrideWithDefault exposing (..)

import Json.Decode as Decoder exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (resolve)
import Json.Encode as Encode
import Types.Override exposing (OverrideName(..), unOverrideName)


type alias OverrideWithDefault =
    { name : OverrideName
    , value : String
    }


defaultOverridesDecode : Decoder OverrideWithDefault
defaultOverridesDecode =
    let
        f x =
            case x of
                [ p, v ] ->
                    Decoder.succeed (OverrideWithDefault (OverrideName p) v)

                _ ->
                    Decoder.fail "not an override"
    in
    Decoder.list Decoder.string
        |> Decoder.map f
        |> resolve


defaultOverrideEncoder : OverrideWithDefault -> Value
defaultOverrideEncoder defaultOverride =
    Encode.list Encode.string [ unOverrideName defaultOverride.name, defaultOverride.value ]
