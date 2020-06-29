module Main where

import Obelisk.Route.Frontend
import Reflex.Dom

import Frontend.Route
import Page.Deployments

main :: IO ()
main = mainWidgetWithHead headWidget $
  routeWidget

routeWidget :: MonadWidget t m => m ()
routeWidget = do
  let Right encoder = checkEncoder routeEncoder
  pb <- getPostBuild
  runRouteViewT encoder pb True $ mdo
    subRoute_ $ \case
      DashboardRoute -> do
        r <- askRoute
        dyn_ $ ffor r $ \case
          Nothing -> deploymentsPage
          Just _  -> blank
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
