{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module API.Lib
  ( Deployment (..)
  , DeploymentAPI
  , DeploymentInfo (..)
  , DeploymentLog (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Options.Generic
import Servant.API

import API.Types.Deployment
import API.Types.DeploymentInfo

type DeploymentAPI = "api" :> "v1" :>
                        (    "deployments" :>
                                (    Get '[JSON] [Text]
                                :<|> ReqBody '[JSON] Deployment :> PostNoContent '[PlainText] Text
                                :<|> Capture "name" Text :> Get '[JSON] [Deployment]
                                :<|> Capture "name" Text :> ReqBody '[JSON] Deployment :> PatchNoContent '[PlainText] Text
                                :<|> Capture "name" Text :> DeleteNoContent '[PlainText] Text
                                :<|> Capture "name" Text :> ReqBody '[JSON] Deployment :> PutNoContent '[PlainText] Text
                                :<|> Capture "name" Text :> "info" :> Get '[JSON] [DeploymentInfo]
                                )
                        :<|> "ping" :> GetNoContent '[PlainText] Text
                        )
