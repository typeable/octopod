{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import Common.Types
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

deriving newtype instance FromField Action

deriving newtype instance ToField Action

deriving newtype instance FromField DeploymentTag

deriving newtype instance ToField DeploymentTag

deriving newtype instance FromField DeploymentName

deriving newtype instance ToField DeploymentName
