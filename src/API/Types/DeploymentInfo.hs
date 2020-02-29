{-# LANGUAGE DeriveGeneric     #-}

module API.Types.DeploymentInfo
  ( DeploymentInfo (..)
  , DeploymentLog (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Options.Generic

import API.Types.Deployment

data DeploymentLog
  = DeploymentLog { action :: Text, deploymentTag :: Text, deploymentEnvs :: [Text], exitCode :: Int, createdAt :: Int }
  deriving (Generic, Show)

instance FromJSON DeploymentLog
instance ToJSON DeploymentLog

data DeploymentInfo
  = DeploymentInfo { deployment :: Deployment, logs :: [DeploymentLog] }
  deriving (Generic, Show)

instance FromJSON DeploymentInfo
instance ToJSON DeploymentInfo
