module Main where

import Data.ByteString (ByteString)
import Data.Text (pack)
import Obelisk.Route.Frontend
import Reflex.Dom
import Servant.Reflex

import Common.Types as CT
import Frontend.API
import Frontend.Route
import Page.Deployment
import Page.Deployments
import Page.Utils
import Frontend.GHCJS


main :: IO ()
main = mainWidgetWithHead headWidget $ do
  initConfigWidget


initConfigWidget :: (MonadWidget t m, Prerender js t m) => m ()
initConfigWidget = do
  pb <- getPostBuild
  x <- performEvent (initConfig <$ pb)
  widgetHold_ loadingWidget $ leftmost
    [ (headerWidget >> routeWidget) <$ ffilter id x
    , errorWidget <$ ffilter not x ]

tstW :: MonadWidget t m => m ()
tstW = do
  pb <- getPostBuild
  x <- performEvent (initConfig <$ pb)
  xd <- holdDyn False x
  dynText $ pack . show <$> xd
  blank

wsUpdate :: forall t m . MonadWidget t m => m (Event t ())
wsUpdate = do
  let
    wsConfig = WebSocketConfig
      { _webSocketConfig_send = (never :: Event t [ByteString])
      , _webSocketConfig_close = never
      , _webSocketConfig_reconnect = True
      , _webSocketConfig_protocols = []
      }
  wsUrl <- getWsUrl
  ws <- webSocket wsUrl wsConfig
  pure $ () <$ _webSocket_recv ws

routeWidget
  ::
    ( MonadWidget t m
    , Prerender js t m )
  => m ()
routeWidget = do
  let Right encoder = checkEncoder routeEncoder
  pb <- getPostBuild
  updAllEv <- wsUpdate
  runRouteViewT encoder pb True $ mdo
    subRoute_ $ \case
      DashboardRoute -> do
        r <- askRoute
        dyn_ $ ffor r $ \case
          Nothing -> deploymentsPage updAllEv
          Just dn -> deploymentPage updAllEv dn
    blank

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ( "charset" =: "urf8 ") blank
  elAttr "meta"
    (  "http-equiv" =: "x-ua-compatible"
    <> "content" =: "ie=edge" ) blank
  elAttr "name"
    (  "name" =: "viewport"
    <> "content" =: "width=device-width, initial-scale=1" ) blank
  elAttr "link"
    (  "href" =: "https://fonts.googleapis.com/css2?family=Roboto+Mono&family=Roboto:wght@400;500;700&display=swap"
    <> "rel" =: "stylesheet") blank
  elAttr "link"
    (  "href" =: "/static/styles/style.css"
    <> "rel" =: "stylesheet") blank
  elAttr "script"
    (  "src" =: "/static/vendors/outline/outline.js" ) blank


headerWidget :: MonadWidget t m => m ()
headerWidget =
  elClass "header" "header" $
    divClass "header__wrap container" $ do
      elClass "b" "header__logo" $
        text "Deployment Manager"
      elClass "div" "header__project" $ do
        pb <- getPostBuild
        respEv <- projectName pb
        nameDyn <- holdDyn "" $ uProjectName <$> fmapMaybe reqSuccess respEv
        dynText nameDyn

emptyHeaderWidget :: MonadWidget t m => m ()
emptyHeaderWidget =
  elClass "header" "header" $
    divClass "header__wrap container" $ do
      elClass "b" "header__logo" $
        text "Deployment Manager"
      elClass "div" "header__project" blank

loadingWidget :: MonadWidget t m => m ()
loadingWidget =
  divClass "no-page" $
    divClass "no-page__inner" $
      loadingCommonWidget

errorWidget :: MonadWidget t m => m ()
errorWidget =
  divClass "no-page" $
    divClass "no-page__inner" $
      errorCommonWidget
