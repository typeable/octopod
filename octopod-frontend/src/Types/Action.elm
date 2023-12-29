module Types.Action exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, float, int, list, string, succeed)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (optional, required, resolve)
import Json.Encode as Encode
import Time
import Types.Override exposing (Override, overrideDecoder)


type ActionType
    = Create
    | Update
    | Restore
    | Archive


actionTypeDecoder : Decoder ActionType
actionTypeDecoder =
    let
        tagDecoder tag =
            case tag of
                "CreateAction" ->
                    Decode.succeed Create

                "UpdateAction" ->
                    Decode.succeed Update

                "RestoreAction" ->
                    Decode.succeed Restore

                "ArchiveAction" ->
                    Decode.succeed Archive

                _ ->
                    Decode.fail ("Unknown ActionType: " ++ tag)
    in
    string |> andThen tagDecoder


type ActionId
    = ActionId Int


unActionId : ActionId -> Int
unActionId (ActionId id) =
    id


actionIdDecoder : Decoder ActionId
actionIdDecoder =
    Decode.map ActionId int


type ExitCode
    = ExitCode Int


unExitCode : ExitCode -> Int
unExitCode (ExitCode code) =
    code


exitCodeDecocder : Decoder ExitCode
exitCodeDecocder =
    Decode.map ExitCode int


type alias Duration =
    { months : Int
    , time : Float
    }


durationDecoder : Decoder Duration
durationDecoder =
    Decode.succeed Duration
        |> required "months" int
        |> required "time" float


type alias Log =
    { action : ActionType
    , actionId : ActionId
    , createdAt : Time.Posix
    , appOverrides : List Override
    , deploymentOverrides : List Override
    , duration : Duration
    , exitCode : ExitCode
    }


logDecoder : Decoder Log
logDecoder =
    Decode.succeed Log
        |> required "action" actionTypeDecoder
        |> required "action_id" actionIdDecoder
        |> required "created_at" datetime
        |> required "deployment_app_overrides" (list overrideDecoder)
        |> required "deployment_dep_overrides" (list overrideDecoder)
        |> required "duration" durationDecoder
        |> required "exit_code" exitCodeDecocder


type alias LogWrapper =
    { logs : List Log
    }


logWrapperDecoder : Decoder LogWrapper
logWrapperDecoder =
    Decode.succeed LogWrapper
        |> required "logs" (list logDecoder)
