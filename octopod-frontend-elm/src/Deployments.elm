module Deployments exposing (..)

import Json.Decode as Decoder exposing (Decoder, string, andThen)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (required, optional, resolve)
import Time
import Route exposing (Route(..))


type OverrideValue
  = ValueAdded String
  | ValueDeleted

type alias Override =
  { name : String
  , value : OverrideValue }

type alias Info =
  { name : String
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
  , updatedAt : Time.Posix }

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


type alias Deployments = List Deployment

failureTypeDecoder : Decoder FailureType
failureTypeDecoder =
  let
    tagDecoder tag = case tag of
      "GenericFailure" -> Decoder.succeed GenericFailure
      "PartialAvailability" -> Decoder.succeed PartialAvailability
      "TagMismatch" -> Decoder.succeed TagMismatch
      _ -> Decoder.fail ("Unknown FailureType: " ++ tag)
  in
  string |> andThen tagDecoder

deploymentStatusDecoder : Decoder DeploymentStatus
deploymentStatusDecoder =
  let
    toDecoder : String -> Maybe FailureType -> Decoder DeploymentStatus
    toDecoder tag contents =
      case (tag, contents) of
        ("Running", _) -> Decoder.succeed Running
        ("CreatePending", _) -> Decoder.succeed CreatePending
        ("UpdatePending", _) -> Decoder.succeed UpdatePending
        ("ArchivePending", _) -> Decoder.succeed ArchivePending
        ("Archived", _) -> Decoder.succeed Archived
        ("CleanupFailed", _) -> Decoder.succeed CleanupFailed
        ("Failure", Just failureType) -> Decoder.succeed (Failure failureType)
        (_,_) -> Decoder.fail ("Unknown DeploymentStatus: " ++ tag)
  in
  Decoder.succeed toDecoder
    |> required "tag" string
    |> optional "contents" (Decoder.maybe failureTypeDecoder) Nothing
    |> resolve

statusDecoder : Decoder Status
statusDecoder =
  let
    toDecoder : String -> DeploymentStatus -> Decoder Status
    toDecoder tag contents =
      case tag of
        "DeploymentNotPending" -> Decoder.succeed (DeploymentNotPending contents)
        "DeploymentPending" -> Decoder.succeed (DeploymentPending contents)
        _ -> Decoder.fail ("Unknown Status: " ++ tag)
  in
  Decoder.succeed toDecoder
    |> required "tag" string
    |> required "recorded_status" deploymentStatusDecoder
    |> resolve

metadataDecoder : Decoder Metadata
metadataDecoder =
  Decoder.succeed Metadata
    |> required "name" string
    |> required "link" string

overrideValueDecoder : Decoder OverrideValue
overrideValueDecoder =
  let
    toDecoder : String -> Maybe String -> Decoder OverrideValue
    toDecoder tag contents =
      case (tag, contents) of
        ("ValueDeleted", _) -> Decoder.succeed ValueDeleted
        ("ValueAdded", Just value) -> Decoder.succeed (ValueAdded value)
        (_,_) -> Decoder.fail ("Unknown DeploymentStatus: " ++ tag)
  in
  Decoder.succeed toDecoder
    |> required "tag" string
    |> optional "contents" (Decoder.maybe string) Nothing
    |> resolve

type OverrideHelper
  = OverrideName String
  | OverrideValue OverrideValue



overrideDecoder : Decoder Override
overrideDecoder =
  let
    overrideHelperDecoder = Decoder.list (Decoder.oneOf
      [ string |> Decoder.map OverrideName
      , overrideValueDecoder |> Decoder.map OverrideValue
      ])
    listDecoder vals = case vals of
      [OverrideName name, OverrideValue val] -> Decoder.succeed (Override name val)
      [OverrideValue val, OverrideName name] -> Decoder.succeed (Override name val)
      _ -> Decoder.fail ("Unknown override")
  in
    overrideHelperDecoder |> andThen listDecoder

infoDecoder : Decoder Info
infoDecoder =
  Decoder.succeed Info
    |> required "name" string
    |> required "app_overrides" (Decoder.list overrideDecoder)
    |> required "deployment_overrides" (Decoder.list overrideDecoder)

deploymentDecoder : Decoder Deployment
deploymentDecoder =
  Decoder.succeed Deployment
    |> required "deployment" infoDecoder
    |> required "status" statusDecoder
    |> required "metadata" (Decoder.list metadataDecoder)
    |> required "created_at" datetime
    |> required "updated_at" datetime

deploymentsDecoder : Decoder Deployments
deploymentsDecoder = Decoder.list deploymentDecoder
