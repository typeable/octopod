module Frontend.GHCJS where

import Data.Aeson as A (Value, fromJSON, Result(..))
import Control.Monad.Catch
import Data.Text as T (pack, Text)
import Deriving.Aeson.Stock
import GHCJS.DOM
import GHCJS.DOM.Response
import GHCJS.DOM.Storage
import GHCJS.DOM.Types
import GHCJS.DOM.Window

appUrlKey, wsUrlKey, appAuthKey :: JSString
appUrlKey = "app"
wsUrlKey = "ws"
appAuthKey = "auth"

data ProjectConfig = ProjectConfig
  { appUrl :: String
  , wsUrl :: String
  , appAuth :: String }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake ProjectConfig

data BadConfig = BadConfig
  deriving (Show)

instance Exception BadConfig

initConfig' :: JSM ()
initConfig' = do
  w <- currentWindowUnchecked
  resp <- fetch w ("/config.json" :: JSString) Nothing
  jsonVal <- json resp
  val :: A.Value <- fromJSValUnchecked jsonVal
  let mcfg = fromJSON val
  case mcfg of
    A.Success cfg -> do
      stor <- getSessionStorage w
      setItem stor appUrlKey $ appUrl cfg
      setItem stor wsUrlKey $ wsUrl cfg
      setItem stor appAuthKey $ appAuth cfg
    A.Error _ -> throwM BadConfig

initConfig :: MonadJSM m => m Bool
initConfig = liftJSM $ catchAll (initConfig' >> pure True) (const $ pure False)

getVar :: MonadJSM m => JSString -> m T.Text
getVar k = do
  w <- currentWindowUnchecked
  stor <- getSessionStorage w
  x <- getItemUnsafe stor k
  pure $ pack x

getAppUrl :: MonadJSM m => m T.Text
getAppUrl = getVar appUrlKey

getAppAuth :: MonadJSM m => m T.Text
getAppAuth = getVar appAuthKey

getWsUrl :: MonadJSM m => m T.Text
getWsUrl = getVar wsUrlKey
