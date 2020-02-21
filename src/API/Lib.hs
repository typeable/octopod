{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}

module API.Lib
  ( Deployment (..)
  , DeploymentAPI
  ) where

import Data.Aeson
import Data.Text (Text)
import Options.Generic
import Servant.API

data Deployment
  = Deployment { name :: Text, tag :: Text, envs :: [Text] }
  deriving (Generic, Show)

instance ToJSON Deployment
instance FromJSON Deployment

type DeploymentAPI = "api" :> "v1" :> "deployments" :>
                        (    Get '[JSON] [Text]
                        :<|> ReqBody '[JSON] Deployment :> PostNoContent '[PlainText] Text
                        :<|> Capture "name" Text :> Get '[JSON] [Deployment]
                        :<|> Capture "name" Text :> ReqBody '[JSON] Deployment :> PatchNoContent '[PlainText] Text
                        :<|> Capture "name" Text :> DeleteNoContent '[PlainText] Text
                        :<|> Capture "name" Text :> ReqBody '[JSON] Deployment :> PutNoContent '[PlainText] Text
                        )
