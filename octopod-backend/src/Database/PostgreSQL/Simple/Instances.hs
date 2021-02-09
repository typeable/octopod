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
import qualified Data.Text.Encoding as T

instance ToField DeploymentStatus where
  toField = toField @Text . deploymentStatusText

instance FromField DeploymentStatus where
  fromField _ (Just b) =
    (either (const empty) pure . T.decodeUtf8' $ b) >>= maybe empty return . flip M.lookup m
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
  fromField _ Nothing = empty
