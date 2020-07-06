module Page.Utils where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Maybe (fromMaybe)
import Data.Text as T (Text, pack)
import Data.Time
import Data.Time.Clock.POSIX
import GHCJS.DOM
import GHCJS.DOM.Element as DOM
import GHCJS.DOM.EventM (on, target)
import GHCJS.DOM.GlobalEventHandlers as Events (click)
import GHCJS.DOM.Node as DOM
import Reflex.Dom as R

import Common.Types as CT

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
  openedDyn <- foldDyn switchState False $ leftmost
    [ clickInsideEv
    , False <$ domEvent Click bodyEl
    , True <$ domEvent Click btnEl ]
  let
    switchState ev cur = ev && not cur
    wrapperClassDyn = ffor openedDyn $ \case
      True -> "class" =: "drop drop--actions drop--expanded"
      False -> "class" =: "drop drop--actions"
  (btnEl, (bodyEl, wEv)) <- elDynAttrWithStopPropagationEvent' Click
    "div" wrapperClassDyn $ do
      btn
      elDynAttrWithStopPropagationEvent' Click "div"
        (constDyn $ "class" =: "drop__dropdown") body
  pure wEv

showT :: Show a => a -> Text
showT = pack . show

sidebar
  :: MonadWidget t m
  => Event t d
  -> Event t ()
  -> (d -> m (Event t a, Event t ()))
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
      [ m <$> showEv
      , blank' <$ deferDomClearEv ]
  pure $ switchDyn $ fst <$> resultEvDyn

popupOverlay :: DomBuilder t m => m ()
popupOverlay =
  elAttr "div" ("class" =: "popup__overlay" <> "aria-hidden" =: "true") blank

eventWriterWrapper :: (MonadWidget t m, Semigroup e) => (Event t e -> EventWriterT t e m ()) -> m (Event t e)
eventWriterWrapper m = mdo
  (_, ev) <- runEventWriterT (m ev)
  pure ev

buttonClass :: (DomBuilder t m, PostBuild t m) => Text -> Text -> m (Event t ())
buttonClass cl lbl = do
  (bEl, _) <- elDynAttrWithStopPropagationEvent' Click "button"
    (constDyn $ "class" =: cl <> "type" =: "button") $ text lbl
  return $ domEvent Click bEl

buttonDynClass
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Text
  -> m (Event t ())
buttonDynClass clDyn lblDyn = do
  let attrDyn = ffor clDyn $ \cl -> "class" =: cl <> "type" =: "button"
  (bEl, _) <- elDynAttrWithStopPropagationEvent' Click "button" attrDyn $
    dynText lblDyn
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
  (bEl, _) <- elDynAttrWithStopPropagationEvent' Click "button" attrDyn $
    text lbl
  return $ domEvent Click bEl

intToUTCTime :: Int -> UTCTime
intToUTCTime = posixSecondsToUTCTime . realToFrac

elDynAttrWithPreventDefaultEvent'
  :: forall a en m t. (DomBuilder t m, PostBuild t m)
  => EventName en              -- ^ Event on the element to configure with 'preventDefault'
  -> Text                      -- ^ Element tag
  -> Dynamic t (Map Text Text) -- ^ Element attributes
  -> m a                       -- ^ Child of element
  -> m (R.Element EventResult (DomBuilderSpace m) t, a) -- An element and the result of the child
elDynAttrWithPreventDefaultEvent' ev = elDynAttrWithModifyConfig'
  (\elCfg -> elCfg & elementConfig_eventSpec %~
    addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) ev (const preventDefault))

elDynAttrWithStopPropagationEvent'
  :: forall a en m t. (DomBuilder t m, PostBuild t m)
  => EventName en              -- ^ Event on the element to configure with 'preventDefault'
  -> Text                      -- ^ Element tag
  -> Dynamic t (Map Text Text) -- ^ Element attributes
  -> m a                       -- ^ Child of element
  -> m (R.Element EventResult (DomBuilderSpace m) t, a) -- An element and the result of the child
elDynAttrWithStopPropagationEvent' ev = elDynAttrWithModifyConfig'
  (\elCfg -> elCfg & elementConfig_eventSpec %~
    addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) ev (const stopPropagation))

elDynAttrWithModifyConfig'
  :: (DomBuilder t m, PostBuild t m)
  => (ElementConfig EventResult t (DomBuilderSpace m) -> ElementConfig EventResult t (DomBuilderSpace m))
  -> Text
  -> Dynamic t (Map Text Text)
  -> m a
  -> m (R.Element EventResult (DomBuilderSpace m) t, a)
elDynAttrWithModifyConfig' f elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg = def & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result <- R.element elementTag (f cfg) child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure result

formatPosixToDate :: Int -> Text
formatPosixToDate = pack
  . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
  . intToUTCTime

formatPosixToDateTime :: Int -> Text
formatPosixToDateTime = pack
  . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
  . intToUTCTime

statusWidget :: MonadWidget t m => Dynamic t (Maybe CurrentDeploymentStatus) -> m ()
statusWidget stDyn = do
  stDyn' <- holdUniqDyn stDyn
  dyn_ $ stDyn' <&> \case
    Nothing -> divClass "loading loading--status-alike" $ text "Loading"
    Just (CurrentDeploymentStatus st) -> case st of
      CT.Ok -> divClass "status status--success" $ text "Success"
      CT.Error -> divClass "status status--failure" $ text "Failure"

dmTextInput
  :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> Maybe Text
  -> Dynamic t (Maybe Text)
  -> m (Dynamic t Text)
dmTextInput clss lbl placeholder val errDyn =
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text lbl
    elClass "div" "staging__widget" $
      dmTextInput' clss placeholder val errDyn

dmTextInput'
  :: MonadWidget t m
  => Text
  -> Text
  -> Maybe Text
  -> Dynamic t (Maybe Text)
  -> m (Dynamic t Text)
dmTextInput' clss placeholder val errDyn = do
  let
    classDyn = errDyn <&> \case
      Nothing -> clss <> " input"
      Just _  -> clss <> " input input--error"
  elDynClass "div" classDyn $ do
    inp <- inputElement $ def
      & initialAttributes .~
        (  "type" =: "text"
        <> "class" =: "input__widget"
        <> "placeholder" =: placeholder )
      & inputElementConfig_initialValue .~ fromMaybe "" val
    dyn_ $ errDyn <&> \case
      Nothing -> blank
      Just x  -> divClass "input__output" $ text x
    pure $ value inp


loadingCommonWidget :: MonadWidget t m => m ()
loadingCommonWidget =
  divClass "loading loading--enlarged loading--alternate" $
    text "Loading..."

errorCommonWidget :: MonadWidget t m => m ()
errorCommonWidget =
  divClass "null null--data" $
    divClass "null__content" $ do
      elClass "b" "null__heading" $ text "Cannot retrieve the data"
      divClass "null__message" $ text "Try to reload page"

overridesWidget :: MonadWidget t m => EnvPairs -> m ()
overridesWidget envs = divClass "listing" $ do
  let
    visible = take 3 envs
    envLength = length envs
  listing visible
  when (envLength > 3) $ mdo
    let hidden = drop 3 envs
    showDyn <- toggle False toggleEv
    dyn_ $ showDyn <&> \case
      True -> listing hidden
      False -> blank
    let
      btnClassDyn = ifThenElseDyn showDyn
        "listing__item expander bar expander--open"
        "listing__item expander bar"
      btnTextDyn = ifThenElseDyn showDyn "Hide"
        $ "Show all (" <> (pack . show $ envLength) <> ")"
    toggleEv <- buttonDynClass btnClassDyn btnTextDyn
    blank
  where
    listing envs' = do
      forM_ envs' $ \(var, val) ->
        divClass "listing__item bar" $ do
          el "b" $ text $ var <> ": "
          text val

ifThenElseDyn :: Reflex t => Dynamic t Bool -> b -> b -> Dynamic t b
ifThenElseDyn bDyn t f = bDyn <&> \case
  True -> t
  False -> f
