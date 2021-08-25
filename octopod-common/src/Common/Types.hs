{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module      : Common.Types
-- Description : Common types for backend and frontend.
--
-- This module contains common types between the backend and the frontend.
module Common.Types where

import Control.Lens
import Data.Aeson hiding (Result)
import Data.Csv
import Data.Generics.Labels ()
import Data.Int
import Data.Map.Ordered.Strict (OMap, (<>|))
import qualified Data.Map.Ordered.Strict as OM
import Data.Map.Ordered.Strict.Extra ()
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Traversable
import Deriving.Aeson
import Deriving.Aeson.Stock
import Web.HttpApiData

data OverrideLevel = ApplicationLevel | DeploymentLevel

class KnownOverrideLevel (l :: OverrideLevel) where
  knownOverrideLevel :: OverrideLevel

instance KnownOverrideLevel 'ApplicationLevel where
  knownOverrideLevel = ApplicationLevel

instance KnownOverrideLevel 'DeploymentLevel where
  knownOverrideLevel = DeploymentLevel

data OverrideValue = ValueAdded Text | ValueDeleted
  deriving (ToJSON, FromJSON) via Snake OverrideValue
  deriving stock (Eq, Ord, Show, Generic)

newtype DefaultConfig (l :: OverrideLevel) = DefaultConfig (OMap Text Text)
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype Config (l :: OverrideLevel) = Config {unConfig :: OMap Text Text}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

data FullDefaultConfig = FullDefaultConfig
  { appDefaultConfig :: DefaultConfig 'ApplicationLevel
  , depDefaultConfig :: DefaultConfig 'DeploymentLevel
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving (ToJSON, FromJSON) via Snake FullDefaultConfig

data FullConfig = FullConfig
  { appConfig :: Config 'ApplicationLevel
  , depConfig :: Config 'DeploymentLevel
  }
  deriving stock (Show, Ord, Eq, Generic)
  deriving (ToJSON, FromJSON) via Snake FullConfig

applyOverrides :: Overrides l -> DefaultConfig l -> Config l
applyOverrides (Overrides oo) (DefaultConfig dd) =
  Config . extract $ oo <>| (ValueAdded <$> dd)
  where
    extract :: OMap Text OverrideValue -> OMap Text Text
    extract =
      fmap
        ( \case
            ValueAdded v -> v
            ValueDeleted -> error "invariant"
        )
        . OM.filter
          ( \_ -> \case
              ValueAdded _ -> True
              ValueDeleted -> False
          )

newtype Overrides (l :: OverrideLevel) = Overrides {unOverrides :: OMap Text OverrideValue}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

extractOverrides :: DefaultConfig l -> Config l -> Overrides l
extractOverrides (DefaultConfig dCfg) (Config cfg) =
  Overrides . OM.fromList $ removed <> present
  where
    present :: [(Text, OverrideValue)]
    present = mapMaybe processPresent . OM.assocs $ cfg

    processPresent :: (Text, Text) -> Maybe (Text, OverrideValue)
    processPresent (k, v) = case OM.lookup k dCfg of
      Just v' | v == v' -> Nothing
      _ -> Just (k, ValueAdded v)

    processRemoved :: (Text, Text) -> Maybe (Text, OverrideValue)
    processRemoved (k, _) =
      if OM.member k cfg
        then Nothing
        else Just (k, ValueDeleted)

    removed :: [(Text, OverrideValue)]
    removed = mapMaybe processRemoved . OM.assocs $ dCfg

ov :: Text -> OverrideValue -> Overrides l
ov k v = Overrides $ OM.singleton (k, v)

instance Semigroup (Overrides l) where
  (Overrides lhs) <> (Overrides rhs) = Overrides $ rhs <>| lhs

instance Monoid (Overrides l) where
  mempty = Overrides OM.empty

newtype DeploymentId = DeploymentId {unDeploymentId :: Int64}
  deriving stock (Show)

newtype DeploymentName = DeploymentName {unDeploymentName :: Text}
  deriving newtype
    (Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq, Ord)

newtype DeploymentTag = DeploymentTag {unDeploymentTag :: Text}
  deriving newtype
    (Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq)

data Action = RestoreAction | ArchiveAction | UpdateAction | CreateAction
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON) via Snake Action

actionText :: [(Action, Text)]
actionText =
  [ (RestoreAction, "restore")
  , (ArchiveAction, "archive")
  , (UpdateAction, "update")
  , (CreateAction, "create")
  ]

actionToText :: Action -> Text
actionToText k = fromMaybe (error $ "forgot case: " <> show k) . Prelude.lookup k $ actionText

newtype ArchivedFlag = ArchivedFlag {unArchivedFlag :: Bool}
  deriving newtype (Show, FromJSON, ToJSON)

newtype Duration = Duration {unDuration :: CalendarDiffTime}
  deriving newtype (Show, Eq, FromJSON, ToJSON, FormatTime)

newtype Timestamp = Timestamp {unTimestamp :: CalendarDiffTime}
  deriving newtype (Show, Eq, FromJSON, ToJSON)

newtype ProjectName = ProjectName {uProjectName :: Text}
  deriving newtype (Show, FromJSON, ToJSON)

deploymentStatusText :: [(DeploymentStatus, Text)]
deploymentStatusText =
  [ (Running, "Running")
  , (Failure GenericFailure, "GenericFailure")
  , (Failure TagMismatch, "TagMismatch")
  , (Failure PartialAvailability, "PartialAvailability")
  , (CreatePending, "CreatePending")
  , (UpdatePending, "UpdatePending")
  , (ArchivePending, "ArchivePending")
  , (Archived, "Archived")
  ]

deploymentStatusToText :: DeploymentStatus -> Text
deploymentStatusToText k = fromMaybe (error $ "forgot case: " <> show k) . Prelude.lookup k $ deploymentStatusText

data DeploymentStatus
  = Running
  | Failure FailureType
  | CreatePending
  | UpdatePending
  | ArchivePending
  | Archived
  deriving stock (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentStatus

data FailureType
  = GenericFailure
  | TagMismatch
  | PartialAvailability
  deriving stock (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake FailureType

data PreciseDeploymentStatus
  = -- | The deployment is currently being processed by the server
    DeploymentPending {recordedStatus :: DeploymentStatus}
  | DeploymentNotPending {recordedStatus :: DeploymentStatus}
  deriving stock (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake PreciseDeploymentStatus

archivedStatuses :: [DeploymentStatus]
archivedStatuses = [ArchivePending, Archived]

isArchivedStatus :: DeploymentStatus -> Bool
isArchivedStatus = (`elem` archivedStatuses)

data Deployment = Deployment
  { name :: DeploymentName
  , tag :: DeploymentTag
  , appOverrides :: Overrides 'ApplicationLevel
  , deploymentOverrides :: Overrides 'DeploymentLevel
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake Deployment

data DeploymentLog = DeploymentLog
  { actionId :: ActionId
  , action :: Action
  , deploymentTag :: DeploymentTag
  , deploymentAppOverrides :: Overrides 'ApplicationLevel
  , deploymentDepOverrides :: Overrides 'DeploymentLevel
  , exitCode :: Int64
  , duration :: Duration
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via Snake DeploymentLog

newtype DeploymentMetadata = DeploymentMetadata {unDeploymentMetadata :: [DeploymentMetadatum]}
  deriving newtype (Eq, Show, Ord, FromJSON, ToJSON)

data DeploymentMetadatum = DeploymentMetadatum
  { -- | The name of the link
    name :: Text
  , -- | The URL
    link :: Text
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving (FromJSON, ToJSON) via Snake DeploymentMetadatum
  deriving anyclass (FromRecord)

data DeploymentInfo = DeploymentInfo
  { deployment :: Deployment
  , metadata :: DeploymentMetadata
  , logs :: [DeploymentLog]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentInfo

data DeploymentFullInfo = DeploymentFullInfo
  { deployment :: Deployment
  , status :: PreciseDeploymentStatus
  , metadata :: DeploymentMetadata
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  , deploymentDefaultConfig :: FullDefaultConfig
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentFullInfo

getDeploymentConfig :: DeploymentFullInfo -> FullConfig
getDeploymentConfig d =
  FullConfig
    { appConfig = applyOverrides (d ^. #deployment . #appOverrides) (d ^. #deploymentDefaultConfig . #appDefaultConfig)
    , depConfig = applyOverrides (d ^. #deployment . #deploymentOverrides) (d ^. #deploymentDefaultConfig . #depDefaultConfig)
    }

isDeploymentArchived :: DeploymentFullInfo -> Bool
isDeploymentArchived DeploymentFullInfo {status = s} = case s of
  DeploymentNotPending s' -> isArchivedStatus s'
  -- if the deployment is currently undergoing some process,
  -- then it is not considered archived
  DeploymentPending _ -> False

data DeploymentUpdate = DeploymentUpdate
  { newTag :: DeploymentTag
  , appOverrides :: Overrides 'ApplicationLevel
  , deploymentOverrides :: Overrides 'DeploymentLevel
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentUpdate

data CurrentStatus
  = Ok
  | Error
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake CurrentStatus

newtype CurrentDeploymentStatus = CurrentDeploymentStatus {status :: CurrentStatus}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake CurrentDeploymentStatus

data CommandResponse
  = Success
  | ValidationError
      { nameField :: [Text]
      , tagField :: [Text]
      }
  | AppError
      {errorMessage :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake CommandResponse

data WSEvent = FrontendPleaseUpdateEverything
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake WSEvent

newtype ActionId = ActionId {unActionId :: Int64}
  deriving newtype
    (Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq)

newtype Stdout = Stdout {unStdout :: Text}
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake Stdout

newtype Stderr = Stderr {unStderr :: Text}
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake Stderr

data ActionInfo = ActionInfo
  { stdout :: Stdout
  , stderr :: Stderr
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake ActionInfo

-- | Parses setting overrides.
parseSetOverrides :: [Text] -> Either Text (Overrides l)
parseSetOverrides texts = do
  pairs' <- for texts $ \text -> case parseSingleOverride text of
    Just x -> Right x
    Nothing ->
      Left $ "Malformed override key-value pair " <> text <> ", should be similar to FOO=bar"
  return . Overrides $ OM.fromList pairs'
  where
    parseSingleOverride :: Text -> Maybe (Text, OverrideValue)
    parseSingleOverride t
      | Just i <- T.findIndex (== '=') t =
        let (key, value) = bimap T.strip (T.tail . T.strip) $ T.splitAt i t
         in Just (key, ValueAdded value)
    parseSingleOverride _ = Nothing

parseUnsetOverrides :: [Text] -> Overrides l
parseUnsetOverrides = Overrides . OM.fromList . fmap (,ValueDeleted)

formatOverrides :: Overrides l -> Text
formatOverrides = T.unlines . formatOverrides'

formatOverrides' :: Overrides l -> [Text]
formatOverrides' (Overrides m) = fmap (\(k, v) -> k <> "=" <> showValue v) . OM.assocs $ m
  where
    showValue (ValueAdded v) = v
    showValue ValueDeleted = "<removed>"
