module Page.Utils where

import Data.Text
import GHCJS.DOM
import GHCJS.DOM.Element as DOM
import GHCJS.DOM.EventM (on, target)
import GHCJS.DOM.GlobalEventHandlers as Events (click)
import GHCJS.DOM.Node as DOM
import Reflex.Dom

clickInside :: MonadWidget t m => DOM.Element -> m (Event t Bool)
clickInside elm = do
  doc <- currentDocumentUnchecked
  wrapDomEvent doc (`on` Events.click) $ do
    t :: Maybe DOM.Element <- target
    DOM.contains elm t

documentClickWidget :: MonadWidget t m => m (Event t (Maybe Text))
documentClickWidget = do
  doc <- currentDocumentUnchecked
  wrapDomEvent doc (`on` Events.click) $ do
    t :: Maybe DOM.Element <- target
    sequence $ getId <$> t

dropdownWidget
  :: MonadWidget t m
  => Text
  -> m ()
  -> m ()
  -> m ()
dropdownWidget elId btn body = mdo
  docClickEv <- documentClickWidget
  let
    openEv = ffilter (Just elId ==) docClickEv
    closeEv = docClickEv
  wrapperClassDyn <- holdDyn "drop drop--actions" $ leftmost
    [ "drop drop--actions drop--expanded" <$ openEv
    , "drop drop--actions" <$ closeEv ]
  elDynClass "div" wrapperClassDyn $ do
    btn
    divClass "drop__dropdown panel panel--flying" body
