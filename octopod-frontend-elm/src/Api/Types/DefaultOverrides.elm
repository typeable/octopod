module Api.Types.DefaultOverrides exposing (..)

import Api.Types.Deployment exposing (OverrideName(..))
import Json.Decode as Decoder exposing (Decoder, Value, string)
import Json.Encode as Encode


type alias DefaultOverrides =
    List (List String)


defaultOverridesDecoder : Decoder DefaultOverrides
defaultOverridesDecoder =
    Decoder.list (Decoder.list Decoder.string)


defaultOverridesEncoder : DefaultOverrides -> Value
defaultOverridesEncoder =
    Encode.list (Encode.list Encode.string)
