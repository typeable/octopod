{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module      : Common.Types
-- Description : Common types for backend and frontend.
--
-- This module contains common types between the backend and the frontend.
module Common.Types where

import Common.Orphans ()
import Control.DeepSeq (NFData)
import Control.Lens
import Control.Searchable
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

instance Searchable needle t => Searchable needle (OverrideValue' t) where
  type SearchableConstraint needle (OverrideValue' t) res = SearchableConstraint needle t res
  type Searched (OverrideValue' t) res = OverrideValue' (Searched t res)
  searchWith _ ValueDeleted = pure ValueDeleted
  searchWith f (ValueAdded t) = ValueAdded <$> searchWith f t
  {-# INLINE searchWith #-}

data OverrideValue' t = ValueAdded t | ValueDeleted
  deriving (ToJSON, FromJSON) via Snake (OverrideValue' t)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

type OverrideValue = OverrideValue' Text

type DefaultConfig = DefaultConfig' Text

newtype DefaultConfig' te (l :: OverrideLevel) = DefaultConfig (OMap te te)
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

instance Searchable t x => Searchable t (DefaultConfig' x l) where
  type SearchableConstraint t (DefaultConfig' x l) res = (Ord (Searched x res), SearchableConstraint t x res)
  type Searched (DefaultConfig' x l) res = DefaultConfig' (Searched x res) l
  searchWith f (DefaultConfig oMap) = do
    oMap' <- searchWith f . OM.assocs $ oMap
    pure $ DefaultConfig $ OM.fromList oMap'
  {-# INLINE searchWith #-}

lookupDefaultConfig :: DefaultConfig l -> Text -> Maybe Text
lookupDefaultConfig (DefaultConfig m) k = OM.lookup k m

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

instance Searchable t x => Searchable t (Overrides' x l) where
  type SearchableConstraint t (Overrides' x l) res = (Ord (Searched x res), SearchableConstraint t x res)
  type Searched (Overrides' x l) res = Overrides' (Searched x res) l
  searchWith f (Overrides oMap) = do
    oMap' <- searchWith f . OM.assocs $ oMap
    pure $ Overrides $ OM.fromList oMap'
  {-# INLINE searchWith #-}

newtype Overrides' t (l :: OverrideLevel) = Overrides {unOverrides :: OMap t (OverrideValue' t)}
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON, NFData)

type Overrides = Overrides' Text

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

instance Searchable needle t => Searchable needle (DeploymentName' t) where
  type SearchableConstraint needle (DeploymentName' t) res = SearchableConstraint needle t res
  type Searched (DeploymentName' t) res = DeploymentName' (Searched t res)
  searchWith f (DeploymentName t) = DeploymentName <$> searchWith f t
  {-# INLINE searchWith #-}

newtype DeploymentName' t = DeploymentName {unDeploymentName :: t}
  deriving newtype
    (Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, Eq, Ord, NFData)

type DeploymentName = DeploymentName' Text

data Action = RestoreAction | ArchiveAction | UpdateAction | CreateAction | CleanupAction
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
  , (CleanupFailed, "CleanupFailed")
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
  | CleanupFailed
  deriving stock (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake DeploymentStatus
  deriving anyclass (NFData)

data FailureType
  = GenericFailure
  | TagMismatch
  | PartialAvailability
  deriving stock (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake FailureType
  deriving anyclass (NFData)

data PreciseDeploymentStatus
  = -- | The deployment is currently being processed by the server
    DeploymentPending {recordedStatus :: DeploymentStatus}
  | DeploymentNotPending {recordedStatus :: DeploymentStatus}
  deriving stock (Generic, Read, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake PreciseDeploymentStatus
  deriving anyclass (NFData)

archivedStatuses :: [DeploymentStatus]
archivedStatuses = [ArchivePending, Archived]

isArchivedStatus :: DeploymentStatus -> Bool
isArchivedStatus = (`elem` archivedStatuses)

data Deployment' t = Deployment
  { name :: DeploymentName' t
  , appOverrides :: Overrides' t 'ApplicationLevel
  , deploymentOverrides :: Overrides' t 'DeploymentLevel
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake (Deployment' t)
  deriving anyclass (NFData)

instance (Searchable needle t) => Searchable needle (Deployment' t) where
  type
    SearchableConstraint needle (Deployment' t) res =
      (SearchableConstraint needle t res, Ord (Searched t res))
  type Searched (Deployment' t) res = Deployment' (Searched t res)
  searchWith f d = do
    name' <- searchWith f $ d ^. #name
    appOverrides' <- searchWith f $ d ^. #appOverrides
    deploymentOverrides' <- searchWith f $ d ^. #deploymentOverrides
    pure
      Deployment
        { name = name'
        , appOverrides = appOverrides'
        , deploymentOverrides = deploymentOverrides'
        }
  {-# INLINE searchWith #-}

type Deployment = Deployment' Text

data DeploymentLog = DeploymentLog
  { actionId :: ActionId
  , action :: Action
  , deploymentAppOverrides :: Overrides 'ApplicationLevel
  , deploymentDepOverrides :: Overrides 'DeploymentLevel
  , exitCode :: Int64
  , duration :: Duration
  , createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving (ToJSON, FromJSON) via Snake DeploymentLog

newtype DeploymentMetadata = DeploymentMetadata {unDeploymentMetadata :: [DeploymentMetadatum]}
  deriving newtype (Eq, Show, Ord, FromJSON, ToJSON, NFData)

data DeploymentMetadatum = DeploymentMetadatum
  { -- | The name of the link
    name :: Text
  , -- | The URL
    link :: Text
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving (FromJSON, ToJSON) via Snake DeploymentMetadatum
  deriving anyclass (FromRecord)
  deriving anyclass (NFData)

data DeploymentInfo = DeploymentInfo
  { deployment :: Deployment
  , metadata :: DeploymentMetadata
  , logs :: [DeploymentLog]
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via Snake DeploymentInfo

instance (Searchable needle t) => Searchable needle (DeploymentFullInfo' t) where
  type
    SearchableConstraint needle (DeploymentFullInfo' t) res =
      (SearchableConstraint needle t res, Ord (Searched t res))
  type Searched (DeploymentFullInfo' t) res = DeploymentFullInfo' (Searched t res)
  searchWith f d = do
    deployment' <- searchWith f $ d ^. #deployment
    pure $ d & #deployment .~ deployment'
  {-# INLINE searchWith #-}

data DeploymentFullInfo' t = DeploymentFullInfo
  { deployment :: Deployment' t
  , status :: PreciseDeploymentStatus
  , metadata :: DeploymentMetadata
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake (DeploymentFullInfo' t)
  deriving anyclass (NFData)

type DeploymentFullInfo = DeploymentFullInfo' Text

isDeploymentArchived :: DeploymentFullInfo' t -> Bool
isDeploymentArchived DeploymentFullInfo {status = s} = case s of
  DeploymentNotPending s' -> isArchivedStatus s'
  -- if the deployment is currently undergoing some process,
  -- then it is not considered archived
  DeploymentPending _ -> False

data DeploymentUpdate = DeploymentUpdate
  { appOverrides :: Overrides 'ApplicationLevel
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
