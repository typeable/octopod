{-# OPTIONS_GHC -Wno-orphans #-}

module Database.PostgreSQL.Simple.Instances
  (
  ) where

import           Common.Types
import           Control.Applicative
import           Control.Arrow
import qualified Data.Map as M
import           Data.Text (Text)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField

deploymentStatusText :: DeploymentStatus -> Text
deploymentStatusText Running = "Running"
deploymentStatusText (Failure GenericFailure) = "GenericFailure"
deploymentStatusText (Failure TagMismatch) = "TagMismatch"
deploymentStatusText (Failure PartialAvailability) = "PartialAvailability"
deploymentStatusText CreatePending = "CreatePending"
deploymentStatusText UpdatePending = "UpdatePending"
deploymentStatusText ArchivePending = "ArchivePending"
deploymentStatusText Archived = "Archived"

instance ToField DeploymentStatus where
  toField = toField @Text . deploymentStatusText

instance FromField DeploymentStatus where
  fromField f b = fromField f b >>= maybe empty return . flip M.lookup m
    where
      m = M.fromList . fmap (deploymentStatusText &&& id) $
        [ Running
        , Failure GenericFailure
        , Failure TagMismatch
        , Failure PartialAvailability
        , CreatePending
        , UpdatePending
        , ArchivePending
        , Archived
        ]
