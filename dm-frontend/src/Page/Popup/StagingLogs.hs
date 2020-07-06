module Page.Popup.StagingLogs where

import Data.Functor
import Data.Text as T (replace)
import Prelude as P
import Reflex.Dom

import Common.Types
import Frontend.API
import Page.Utils
import Servant.Reflex


stagingLogsPopup
  :: MonadWidget t m
  => Event t (ActionId)
  -> Event t ()
  -> m (Event t ())
stagingLogsPopup showEv hideEv = sidebar showEv hideEv $ \actionId -> do
  divClass "popup__body" $ mdo
    closeEv <- popupHeader
    typeDyn <- popupSubHeader
    popupBody actionId typeDyn
    pure (never, closeEv)

popupHeader
  :: MonadWidget t m
  => m (Event t ())
popupHeader =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text "Logs"
    pure closeEv

data LogsType
  = StdErr
  | StdOut
  deriving (Show, Eq, Ord)

popupSubHeader
  :: MonadWidget t m
  => m (Dynamic t LogsType)
popupSubHeader =
  divClass "popup__sub-head" $
    divClass "nav" $ mdo
      curTypeDyn <- holdDyn StdOut $
        leftmost [StdOut <$ stdOutEv, StdErr <$ stdErrEv]
      let
        bntClassDyn tp = curTypeDyn <&> \cur ->
          if cur == tp then "nav__link nav__link--current" else "nav__link"
      stdOutEv <- buttonDynClass (bntClassDyn StdOut) (constDyn "stdout")
      stdErrEv <- buttonDynClass (bntClassDyn StdErr) (constDyn "stderr")
      pure curTypeDyn

popupBody :: (MonadWidget t m) => ActionId -> Dynamic t LogsType -> m ()
popupBody actId typeDyn = divClass "popup__content" $ do
  pb <- getPostBuild
  respEv <- getActionInfoEndpoint (constDyn $ pure actId) pb
  let
    okEv = fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqFailure respEv
    divNoLog m = divClass "no-log" m
    loadingWidget = divNoLog loadingCommonWidget
    errorWidget = divNoLog errorCommonWidget
  widgetHold_ loadingWidget $ leftmost
    [ errorWidget <$ errEv
    , logsWidget typeDyn <$> okEv ]

logsWidget :: MonadWidget t m => Dynamic t LogsType -> ActionInfo -> m ()
logsWidget typeDyn ActionInfo{..} = do
  let
    wrapper t = elDynHtmlAttr' "div" ("class" =: "log")
      (constDyn $ replace "\n" "<br>" t)
  dyn_ $ typeDyn <&> \case
    StdOut -> wrapper stdout
    StdErr -> wrapper stderr
