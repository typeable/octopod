module Page.Utils where

import Data.Monoid
import Data.Text
import Control.Lens
import GHCJS.DOM
import GHCJS.DOM.Element as DOM
import GHCJS.DOM.EventM (on, target)
import GHCJS.DOM.GlobalEventHandlers as Events (click)
import GHCJS.DOM.Node as DOM
import Reflex.Dom as R

newtype ClickedElement =
  ClickedElement { unClickedElement :: Maybe DOM.Element }

elementClick :: MonadWidget t m =>  m (Event t ClickedElement)
elementClick = do
  doc <- currentDocumentUnchecked
  wrapDomEvent doc (`on` Events.click) $ ClickedElement <$> target

dropdownWidget
  :: MonadWidget t m
  => m ()
  -> m (Event t a)
  -> m (Event t a)
dropdownWidget btn body = mdo
  clickedEl <- elementClick
  dropdownWidget' clickedEl btn body

dropdownWidget'
  :: MonadWidget t m
  => Event t ClickedElement
  -> m ()
  -> m (Event t a)
  -> m (Event t a)
dropdownWidget' clickedEl btn body = mdo
  clickInsideEv <- performEvent $ ffor clickedEl $ \(ClickedElement clicked) ->
    DOM.contains (_element_raw btnEl) clicked
  openedDyn <- foldDyn switchState False clickInsideEv
  let
    switchState ev cur = ev && not cur
    wrapperClassDyn = ffor openedDyn $ \case
      True -> "drop drop--actions drop--expanded"
      False -> "drop drop--actions"
  (btnEl, wEv) <- elDynClass' "div" wrapperClassDyn $ do
    btn
    divClass "drop__dropdown" body
  pure wEv

showT :: Show a => a -> Text
showT = pack . show

sidebar
  :: MonadWidget t m
  => Event t ()
  -> Event t ()
  -> m (Event t a, Event t ())
  -> m (Event t a)
sidebar showEv closeEv m = mdo
  let
    blank' = pure (never, never)
    selfCloseEv = switchDyn $ snd <$> resultEvDyn
    closeEv' = leftmost
      [ closeEv, selfCloseEv ]
    animationDuration = 0.3
  deferDomClearEv <- delay animationDuration closeEv'
  popupClassDyn <- holdDyn "popup" $ leftmost
    [ "popup" <$ closeEv'
    , "popup popup--visible" <$ showEv ]
  resultEvDyn <- elDynClass "div" popupClassDyn $ do
    popupOverlay
    widgetHold blank' $ leftmost
      [ m <$ showEv
      , blank' <$ deferDomClearEv ]
  pure $ switchDyn $ fst <$> resultEvDyn

popupOverlay :: DomBuilder t m => m ()
popupOverlay =
  elAttr "div" ("class" =: "popup__overlay" <> "aria-hidden" =: "true") blank

-- Lens convenience helpers
(<^.>) :: Functor f => f a -> Getting b a b -> f b
(<^.>) fa l = fa <&> (^. l)

(<^?>) :: Functor f => f a -> Getting (First b) a b -> f (Maybe b)
(<^?>) fa l = fa <&> (^? l)

(<^..>) :: Functor f => f a -> Getting (Endo [b]) a b -> f [b]
(<^..>) fa t = fa <&> (^.. t)

infixl 8 <^.>, <^..>, <^?>


eventWriterWrapper :: (MonadWidget t m, Semigroup e) => (Event t e -> EventWriterT t e m ()) -> m (Event t e)
eventWriterWrapper m = mdo
  (_, ev) <- runEventWriterT (m ev)
  pure ev

buttonClass :: DomBuilder t m => Text -> Text -> m (Event t ())
buttonClass cl lbl = do
  (bEl, _) <- elAttr' "button" ("class" =: cl <> "type" =: "button") $ text lbl
  return $ domEvent Click bEl

buttonDynClass
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Text
  -> m (Event t ())
buttonDynClass clDyn lblDyn = do
  let attrDyn = ffor clDyn $ \cl -> "class" =: cl <> "type" =: "button"
  (bEl, _) <- elDynAttr' "button" attrDyn $ dynText lblDyn
  return $ domEvent Click bEl

buttonClassEnabled
  :: (DomBuilder t m, PostBuild t m)
  => Text -> Text -> Dynamic t Bool -> m (Event t ())
buttonClassEnabled cl lbl dDyn = do
  let
    attrDyn = ffor dDyn $ \case
      True  -> "class" =: cl <> "type" =: "button"
      False ->  "class" =: (cl <> " button--disabled")
        <> "type" =: "button" <> "disabled" =: ""
  (bEl, _) <- elDynAttr' "button" attrDyn $ text lbl
  return $ domEvent Click bEl
