module API where

import Data.Aeson (Value (..))
import Servant.API
import Servant.API.WebSocketConduit

import Common.API (ApiWSPath)

type WebSocketAPI = ApiWSPath :> WebSocketSource Value
