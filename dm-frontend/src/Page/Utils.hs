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
  openedDyn <- foldDyn switchState False $ clickInsideEv
  let
    switchState ev cur = ev && not cur
    wrapperClassDyn = ffor openedDyn $ \case
      True -> "class" =: "drop drop--actions drop--expanded"
      False -> "class" =: "drop drop--actions"
  (btnEl, (_, wEv)) <- elDynAttr'
    "div" wrapperClassDyn $ do
      btn
      elDynAttr' "div"
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
  (bEl, _) <- elDynAttr' "button"
    (constDyn $ "class" =: cl <> "type" =: "button") $ text lbl
  return $ domEvent Click bEl

buttonDynClass
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Text
  -> m (Event t ())
buttonDynClass clDyn lblDyn = do
  let attrDyn = ffor clDyn $ \cl -> "class" =: cl <> "type" =: "button"
  (bEl, _) <- elDynAttr' "button" attrDyn $
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
  (bEl, _) <- elDynAttr' "button" attrDyn $
    text lbl
  return $ domEvent Click bEl

buttonClassEnabled'
  :: (DomBuilder t m, PostBuild t m)
  => Text -> Text -> Dynamic t Bool -> Text -> m (Event t ())
buttonClassEnabled' cl lbl dDyn disClass = do
  let
    attrDyn = ffor dDyn $ \case
      True  -> "class" =: cl <> "type" =: "button"
      False ->  "class" =: (cl <> " " <> disClass)
        <> "type" =: "button" <> "disabled" =: ""
  (bEl, _) <- elDynAttr' "button" attrDyn $
    text lbl
  return $ domEvent Click bEl

aButtonClass :: (DomBuilder t m, PostBuild t m) => Text -> Text -> m (Event t ())
aButtonClass cl lbl = do
  (bEl, _) <- elDynAttr' "a"
    (constDyn $ "class" =: cl <> "type" =: "button") $ text lbl
  return $ domEvent Click bEl

aButtonDynClass
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Text
  -> m (Event t ())
aButtonDynClass clDyn lblDyn = do
  let attrDyn = ffor clDyn $ \cl -> "class" =: cl <> "type" =: "button"
  (bEl, _) <- elDynAttr' "a" attrDyn $
    dynText lblDyn
  return $ domEvent Click bEl

aButtonClassEnabled
  :: (DomBuilder t m, PostBuild t m)
  => Text -> Text -> Dynamic t Bool -> m (Event t ())
aButtonClassEnabled cl lbl dDyn = do
  let
    attrDyn = ffor dDyn $ \case
      True  -> "class" =: cl <> "type" =: "button"
      False ->  "class" =: (cl <> " button--disabled")
        <> "type" =: "button" <> "disabled" =: ""
  (bEl, _) <- elDynAttr' "a" attrDyn $
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

currentStatusWidget
  :: MonadWidget t m
  => Dynamic t (Maybe CurrentDeploymentStatus)
  -> m ()
currentStatusWidget stDyn = do
  stDyn' <- holdUniqDyn stDyn
  dyn_ $ stDyn' <&> \case
    Nothing -> divClass "loading loading--status-alike" $ text "Loading"
    Just (CurrentDeploymentStatus st) -> case st of
      CT.Ok -> divClass "status status--success" $ text "Success"
      CT.Error -> divClass "status status--failure" $ text "Failure"

statusWidget :: MonadWidget t m => Dynamic t DeploymentStatus -> m ()
statusWidget stDyn = do
  stDyn' <- holdUniqDyn stDyn
  let
    pendingWidget = divClass "status status--pending"
  dyn_ $ stDyn' <&> \case
    Running -> divClass "status status--success" $ text "Running"
    Failure -> divClass "status status--failure" $ text "Failure"
    CreatePending -> pendingWidget $ text "Creating..."
    UpdatePending -> pendingWidget $ text "Updating..."
    DeletePending -> pendingWidget $ text "Deleting..."

dmTextInput
  :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> Maybe Text
  -> Event t Text
  -> m (Dynamic t Text, Dynamic t Bool)
dmTextInput clss lbl placeholder val errEv =
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text lbl
    elClass "div" "staging__widget" $
      dmTextInput' clss placeholder val errEv

dmTextInput'
  :: MonadWidget t m
  => Text
  -> Text
  -> Maybe Text
  -> Event t Text
  -> m (Dynamic t Text, Dynamic t Bool)
dmTextInput' clss placeholder val errEv = mdo
  let
    inpClass = " input"
    inpErrClass = " input input--error"
  isValid <- holdDyn True $ leftmost
    [ False <$ errEv
    , True <$ updated valDyn ]
  classDyn <- holdDyn (clss <> inpClass) $ leftmost
    [ (clss <> inpErrClass) <$ errEv
    , (clss <> inpClass) <$ updated valDyn ]
  valDyn <- elDynClass "div" classDyn $ do
    inp <- inputElement $ def
      & initialAttributes .~
        (  "type" =: "text"
        <> "class" =: "input__widget"
        <> "placeholder" =: placeholder )
      & inputElementConfig_initialValue .~ fromMaybe "" val
    widgetHold_ blank $ leftmost
      [ divClass "input__output" . text <$> errEv
      , blank <$ updated valDyn ]
    pure $ value inp
  pure (valDyn, isValid)


loadingCommonWidget :: MonadWidget t m => m ()
loadingCommonWidget =
  divClass "loading loading--enlarged loading--alternate" $
    text "Loading..."

errorCommonWidget :: MonadWidget t m => m ()
errorCommonWidget =
  divClass "null null--data" $
    divClass "null__content" $ do
      elClass "b" "null__heading" $ text "Cannot retrieve the data"
      divClass "null__message" $ text "Try to reload the page"

overridesWidget :: MonadWidget t m => Overrides -> m ()
overridesWidget envs = divClass "listing listing--for-text" $ do
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
        "listing__more expander expander--open"
        "listing__more expander"
      btnTextDyn = ifThenElseDyn showDyn "Hide"
        $ "Show all (" <> (showT $ envLength) <> ")"
    toggleEv <- buttonDynClass btnClassDyn btnTextDyn
    blank
  where
    listing envs' = do
      forM_ envs' $ \(Override var val _) ->
        divClass "listing__item" $ do
          el "b" $ text $ var <> ": "
          text val

ifThenElseDyn :: Reflex t => Dynamic t Bool -> b -> b -> Dynamic t b
ifThenElseDyn bDyn t f = bDyn <&> \case
  True -> t
  False -> f

data DeploymentPageNotification
  = DPMOk Text
  | DPMError Text
  | DPMClear

pageNotification
  :: MonadWidget t m => Event t DeploymentPageNotification -> m ()
pageNotification notEv = mdo
  let
    messageWidget (DPMOk txt) = messageClassWidget txt "notification--success"
    messageWidget (DPMError txt) = messageClassWidget txt "notification--danger"
    messageWidget DPMClear = pure never
    messageClassWidget txt cl =
      divClass ("page__output notification " <> cl) $ do
        text txt
        buttonClass "notification__close" ""
    closeEv = switchDyn closeEvDyn
  closeEvDyn <- widgetHold (pure never) $ leftmost
    [ messageWidget <$> notEv
    , pure never <$ closeEv ]
  blank
