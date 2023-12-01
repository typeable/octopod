module Api.Types.DefaultOverrides exposing (..)

import Api.Types.Deployment exposing (OverrideName(..), unOverrideName)
import Json.Decode as Decoder exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (resolve)
import Json.Encode as Encode


type alias DefaultOverride =
    { name : OverrideName
    , value : String
    }


type alias DefaultOverrides =
    List DefaultOverride


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


defaultOverridesDecoder : Decoder DefaultOverrides
defaultOverridesDecoder =
    Decoder.list defaultOverridesDecode


defaultOverrideEncoder : DefaultOverride -> Value
defaultOverrideEncoder defaultOverride =
    Encode.list Encode.string [ unOverrideName defaultOverride.name, defaultOverride.value ]
