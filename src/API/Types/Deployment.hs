{-# LANGUAGE DeriveGeneric     #-}

module API.Types.Deployment (Deployment (..)) where

import Data.Aeson
import Data.Text (Text)
import Options.Generic

data Deployment
  = Deployment { name :: Text, tag :: Text, envs :: [Text] }
  deriving (Generic, Show)

instance FromJSON Deployment
instance ToJSON Deployment
