module Page.ClassicPopup where

import Reflex.Dom

import Page.Utils


classicPopup
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (), Event t ())
  -> m (Event t ())
classicPopup showEv m = mdo
  let
    closeEv = switchDyn $ snd <$> evsDyn
    okEv = switchDyn $ fst <$> evsDyn
    empty = pure (never, never)
  evsDyn <- widgetHold empty $ leftmost
    [ empty <$ closeEv
    , empty <$ okEv
    , popupWidget m <$ showEv ]
  pure okEv

popupWidget
  :: MonadWidget t m
  => m (Event t (), Event t ())
  -> m (Event t (), Event t ())
popupWidget m =
  fmap snd $
    elDynAttrWithStopPropagationEvent' Click  "div"
    (constDyn $ "class" =: "classic-popup" <> "style" =: "display: block;") $
      divClass "classic-popup__container" $
        divClass "classic-popup__viewport" $
          divClass "classic-popup__slot" $ do
            (okEv, cancelEv) <- m
            closeEv <- buttonClass "classic-popup__close" "Close"
            pure $ (okEv, leftmost [cancelEv, closeEv])

confirmDeletePopup
  :: MonadWidget t m
  => Event t ()
  -> m ()
  -> m (Event t ())
confirmDeletePopup showEv txt = do
  let
    body =
      divClass "dialog dialog--delete" $ do
        divClass "dialog__content" txt
        divClass "dialog__footer" $ do
          okEv <- buttonClass "dialog__action button" "Delete"
          cancelEv <-
            buttonClass "dialog__action button--secondary button" "Cancel"
          pure (okEv, cancelEv)
  classicPopup showEv body
