{-|
Module      : Page.Deployment
Description : Runner.

This module contains app routing and main.
-}

module Main where

import           Data.ByteString (ByteString)
import           Obelisk.Route.Frontend
import           Reflex.Dom
import           Servant.Reflex

import           Common.Types as CT
import qualified Data.Semigroup as S
import           Frontend.API
import           Frontend.GHCJS
import           Frontend.Route
import           Frontend.Utils (errorCommonWidget, loadingCommonWidget)
import           Page.Deployment
import           Page.Deployments


main :: IO ()
main = mdo
  mainWidgetWithHead' (headWidget, \() -> do
      ((), projectNameEv) <- runEventWriterT initConfigWidget
      return $ fmap S.getLast projectNameEv
    )

-- | Receives the config file.
-- If request fails then an error message is displayed.
initConfigWidget
  :: (MonadWidget t m, Prerender js t m, EventWriter t (S.Last ProjectName) m)
  => m ()
initConfigWidget = do
  pb <- getPostBuild
  x <- performEvent (initConfig <$ pb)
  widgetHold_ loadingWidget $ leftmost
    [ (headerWidget >> routeWidget) <$ ffilter id x
    , errorWidget <$ ffilter not x ]
  return ()

-- | Sets up websockets. WS url is obtained from session storage.
wsUpdate :: forall t m . MonadWidget t m => m (Event t ())
wsUpdate = do
  let
    wsConfig = WebSocketConfig
      { _webSocketConfig_send = never :: Event t [ByteString]
      , _webSocketConfig_close = never
      , _webSocketConfig_reconnect = True
      , _webSocketConfig_protocols = []
      }
  wsHost <- getWsUrl
  let wsUrl = wsHost <> "/" <> wsPath
  ws <- webSocket wsUrl wsConfig
  pure $ () <$ _webSocket_recv ws

-- | Widget that controls app routing.
-- It uses the routing scheme defined in 'Frontend.Route'.
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

-- | Content of the @head@ DOM element.
headWidget
  :: (DomBuilder t m, MonadHold t m, PostBuild t m)
  => Event t ProjectName -> m ()
headWidget projectNameEv = do
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
  projectNameDyn <- holdDyn "Octopod" $ ("Octopod – " <>) . uProjectName <$> projectNameEv
  el "title" $ dynText projectNameDyn

-- | Common headers of all pages. Displays the project name.
headerWidget :: (MonadWidget t m, EventWriter t (S.Last ProjectName) m) => m ()
headerWidget =
  elClass "header" "header" $
    divClass "header__wrap container" $ do
      elClass "b" "header__logo" $
        text "Octopod"
      elClass "div" "header__project" $ do
        pb <- getPostBuild
        respEv <- fmapMaybe reqSuccess <$> projectName pb
        tellEvent $ S.Last <$> respEv
        nameDyn <- holdDyn "" $ uProjectName <$> respEv
        dynText nameDyn

-- | Widget with a loading spinner.
loadingWidget :: MonadWidget t m => m ()
loadingWidget =
  divClass "no-page" $
    divClass "no-page__inner" $
      loadingCommonWidget

-- | Widget with an error message.
errorWidget :: MonadWidget t m => m ()
errorWidget =
  divClass "no-page" $
    divClass "no-page__inner" $
      errorCommonWidget
