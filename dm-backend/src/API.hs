module API where

import Data.Aeson (Value (..))
import Servant.API
import Servant.API.WebSocketConduit

type WebSocketAPI = "event" :> WebSocketSource Value
