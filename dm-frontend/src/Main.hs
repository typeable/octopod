module Main where

import Reflex.Dom

import Page.Deployments


main :: IO ()
main = mainWidgetWithHead headWidget $
  deploymentsPage


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
    (  "href" =: "https://fonts.googleapis.com/css2?family=Roboto+Mono&family=Roboto:wght@400;500;700;900&display=swap"
    <> "rel" =: "stylesheet") blank
  elAttr "link"
    (  "href" =: "/static/styles/style.css"
    <> "rel" =: "stylesheet") blank
