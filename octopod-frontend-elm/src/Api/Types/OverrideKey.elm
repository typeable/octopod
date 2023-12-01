module Api.Types.OverrideKey exposing (..)

import Api.Types.Deployment exposing (OverrideName(..))
import Json.Decode as Decoder exposing (Decoder, string)


type alias OverrideKeys =
    List String


keysDecoder : Decoder OverrideKeys
keysDecoder =
    Decoder.list Decoder.string
