module Types.Deployment exposing (..)

import Json.Decode as Decode exposing (Decoder, andThen, string)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (optional, required, resolve)
import Json.Encode as Encode
import Time
import Types.Override exposing (..)


type DeploymentName
    = DeploymentName String


unDeploymentName : DeploymentName -> String
unDeploymentName (DeploymentName name) =
    name


type alias Info =
    { name : DeploymentName
    , appOverrides : List Override
    , deploymentOverrides : List Override
    }


type alias Metadata =
    { name : String
    , link : String
    }


type alias Deployment =
    { deployment : Info
    , status : Status
    , metadata : List Metadata
    , createdAt : Time.Posix
    , updatedAt : Time.Posix
    }


type DeploymentStatus
    = Running
    | Failure FailureType
    | CreatePending
    | UpdatePending
    | ArchivePending
    | Archived
    | CleanupFailed


type FailureType
    = GenericFailure
    | TagMismatch
    | PartialAvailability


type Status
    = DeploymentPending DeploymentStatus
    | DeploymentNotPending DeploymentStatus


unStatus : Status -> DeploymentStatus
unStatus status =
    case status of
        DeploymentPending ds ->
            ds

        DeploymentNotPending ds ->
            ds


failureTypeDecoder : Decoder FailureType
failureTypeDecoder =
    let
        tagDecoder tag =
            case tag of
                "GenericFailure" ->
                    Decode.succeed GenericFailure

                "PartialAvailability" ->
                    Decode.succeed PartialAvailability

                "TagMismatch" ->
                    Decode.succeed TagMismatch

                _ ->
                    Decode.fail ("Unknown FailureType: " ++ tag)
    in
    string |> andThen tagDecoder


deploymentStatusDecoder : Decoder DeploymentStatus
deploymentStatusDecoder =
    let
        toDecoder : String -> Maybe FailureType -> Decoder DeploymentStatus
        toDecoder tag contents =
            case ( tag, contents ) of
                ( "Running", _ ) ->
                    Decode.succeed Running

                ( "CreatePending", _ ) ->
                    Decode.succeed CreatePending

                ( "UpdatePending", _ ) ->
                    Decode.succeed UpdatePending

                ( "ArchivePending", _ ) ->
                    Decode.succeed ArchivePending

                ( "Archived", _ ) ->
                    Decode.succeed Archived

                ( "CleanupFailed", _ ) ->
                    Decode.succeed CleanupFailed

                ( "Failure", Just failureType ) ->
                    Decode.succeed (Failure failureType)

                ( _, _ ) ->
                    Decode.fail ("Unknown DeploymentStatus: " ++ tag)
    in
    Decode.succeed toDecoder
        |> required "tag" string
        |> optional "contents" (Decode.maybe failureTypeDecoder) Nothing
        |> resolve


statusDecoder : Decoder Status
statusDecoder =
    let
        toDecoder : String -> DeploymentStatus -> Decoder Status
        toDecoder tag contents =
            case tag of
                "DeploymentNotPending" ->
                    Decode.succeed (DeploymentNotPending contents)

                "DeploymentPending" ->
                    Decode.succeed (DeploymentPending contents)

                _ ->
                    Decode.fail ("Unknown Status: " ++ tag)
    in
    Decode.succeed toDecoder
        |> required "tag" string
        |> required "recorded_status" deploymentStatusDecoder
        |> resolve


metadataDecoder : Decoder Metadata
metadataDecoder =
    Decode.succeed Metadata
        |> required "name" string
        |> required "link" string


infoDecoder : Decoder Info
infoDecoder =
    Decode.succeed Info
        |> required "name" (Decode.map DeploymentName string)
        |> required "app_overrides" (Decode.list overrideDecoder)
        |> required "deployment_overrides" (Decode.list overrideDecoder)


deploymentDecoder : Decoder Deployment
deploymentDecoder =
    Decode.succeed Deployment
        |> required "deployment" infoDecoder
        |> required "status" statusDecoder
        |> required "metadata" (Decode.list metadataDecoder)
        |> required "created_at" datetime
        |> required "updated_at" datetime


isDeploymentArchived : Deployment -> Bool
isDeploymentArchived d =
    case d.status of
        DeploymentPending _ ->
            False

        DeploymentNotPending s ->
            List.member s [ ArchivePending, Archived ]


isPending : Status -> Bool
isPending status =
    case unStatus status of
        Running ->
            False

        Failure _ ->
            False

        Archived ->
            False

        CreatePending ->
            True

        UpdatePending ->
            True

        ArchivePending ->
            True

        CleanupFailed ->
            True


isDeploymentPending : Status -> Bool
isDeploymentPending status =
    case status of
        DeploymentPending _ ->
            True

        DeploymentNotPending _ ->
            isPending status


infoEncode : Info -> Encode.Value
infoEncode info =
    Encode.object
        [ ( "name", Encode.string (unDeploymentName info.name) )
        , ( "app_overrides", Encode.list overrideEncode info.appOverrides )
        , ( "deployment_overrides", Encode.list overrideEncode info.deploymentOverrides )
        ]


updateInfoEncode : Info -> Encode.Value
updateInfoEncode info =
    Encode.object
        [ ( "app_overrides", Encode.list overrideEncode info.appOverrides )
        , ( "deployment_overrides", Encode.list overrideEncode info.deploymentOverrides )
        ]
