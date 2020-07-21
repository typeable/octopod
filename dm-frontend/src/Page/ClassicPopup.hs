{-|
Module      : Page.Popup.EditStaging
Description : Edit staging sidebar.

This module contains definition of classic popups with ok/cancel buttons.
-}
module Page.ClassicPopup where

import Reflex.Dom

import Frontend.Utils

-- | The root function for popup.
classicPopup
  :: MonadWidget t m
  => Event t ()                 -- ^ @Show@ event.
  -> m (Event t (), Event t ()) -- ^ Body of popup returnung ok/cancel events.
  -> m (Event t ())             -- Return @ok@ event.
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

-- | Wrapper for popup widget that darkens background and supports
-- @close@ button.
popupWidget
  :: MonadWidget t m
  => m (Event t (), Event t ())
  -> m (Event t (), Event t ())
popupWidget m =
  fmap snd $
    elAttr' "div"
    ("class" =: "classic-popup" <> "style" =: "display: block;") $
      divClass "classic-popup__container" $
        divClass "classic-popup__viewport" $
          divClass "classic-popup__slot" $ do
            (okEv, cancelEv) <- m
            closeEv <- buttonClass "classic-popup__close" "Close"
            pure $ (okEv, leftmost [cancelEv, closeEv])

-- | Popup that requires confirmation of staging deletion.
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

