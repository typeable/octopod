{-# OPTIONS_GHC -fno-warn-orphans #-}
module Orphans where

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

import Common.Types

deriving instance FromField Action
deriving instance ToField Action

deriving instance FromField DeploymentTag
deriving instance ToField DeploymentTag

deriving instance FromField DeploymentName
deriving instance ToField DeploymentName
