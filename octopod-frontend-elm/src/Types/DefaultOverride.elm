module Types.DefaultOverride exposing (..)

import Json.Decode as Decoder exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (resolve)
import Json.Encode as Encode
import Types.Deployment exposing (OverrideName(..), unOverrideName)


type alias DefaultOverride =
    { name : OverrideName
    , value : String
    }


defaultOverridesDecode : Decoder DefaultOverride
defaultOverridesDecode =
    let
        f x =
            case x of
                [ p, v ] ->
                    Decoder.succeed (DefaultOverride (OverrideName p) v)

                _ ->
                    Decoder.fail "not an override"
    in
    Decoder.list Decoder.string
        |> Decoder.map f
        |> resolve


defaultOverrideEncoder : DefaultOverride -> Value
defaultOverrideEncoder defaultOverride =
    Encode.list Encode.string [ unOverrideName defaultOverride.name, defaultOverride.value ]
