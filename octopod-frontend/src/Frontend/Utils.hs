{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
--Module      : Frontend.Utils
--Description : Client utils and helpers.
--
--This module contains common types, functions and operators that are used by
--frontend modules.
module Frontend.Utils where

import Control.Lens
import Control.Monad
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
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
import Control.Monad.Reader
import Frontend.GHCJS

-- | Wrapper for @Maybe DOM.Element@. It's used by 'elementClick'.
newtype ClickedElement = ClickedElement {unClickedElement :: Maybe DOM.Element}

-- | Returns a click event bound to the current document.
-- The event carries 'ClickedElement'.
elementClick :: MonadWidget t m => m (Event t ClickedElement)
elementClick = do
  doc <- currentDocumentUnchecked
  wrapDomEvent doc (`on` Events.click) $ ClickedElement <$> target

-- | Dropdown widget which binds its own @document click@ event.
dropdownWidget ::
  MonadWidget t m =>
  -- | Button widget which opens the dropdown widget.
  m () ->
  -- | Widget with the dropdown list.
  -- Returns an event carrying the user's selection.
  m (Event t a) ->
  m (Event t a)
dropdownWidget btn body = mdo
  clickedEl <- elementClick
  dropdownWidget' clickedEl btn body

-- | Similar to 'dropdownWidget' but uses @document click@ event that may be
-- shared between other widgets.
dropdownWidget' ::
  MonadWidget t m =>
  -- | Document click event that may be shared between other widgets.
  Event t ClickedElement ->
  -- | Button widget which opens the dropdown widget.
  m () ->
  -- | Widget with the dropdown list.
  -- Returns an event carrying the user's selection.
  m (Event t a) ->
  m (Event t a)
dropdownWidget' clickedEl btn body = mdo
  clickInsideEv <- performEvent $
    ffor clickedEl $ \(ClickedElement clicked) ->
      DOM.contains (_element_raw btnEl) clicked
  openedDyn <- foldDyn switchState False $ clickInsideEv
  let switchState ev cur = ev && not cur
      wrapperClassDyn = ffor openedDyn $ \case
        True -> "class" =: "drop drop--actions drop--expanded"
        False -> "class" =: "drop drop--actions"
  (btnEl, (_, wEv)) <- elDynAttr'
    "div"
    wrapperClassDyn
    $ do
      btn
      elDynAttr'
        "div"
        (constDyn $ "class" =: "drop__dropdown")
        body
  pure wEv

showT :: Show a => a -> Text
showT = pack . show

-- | Wrapper for a sidebar that provides opening and closing behavior.
sidebar ::
  MonadWidget t m =>
  -- | Show event with data to be passed to sidebar.
  Event t d ->
  -- | Close event.
  Event t () ->
  -- | Sidebar body which returns data with first event and close event.
  (d -> m (Event t a, Event t ())) ->
  m (Event t a)
sidebar showEv closeEv m = mdo
  let blank' = pure (never, never)
      selfCloseEv = switchDyn $ snd <$> resultEvDyn
      closeEv' =
        leftmost
          [closeEv, selfCloseEv]
      animationDuration = 0.3
  deferDomClearEv <- delay animationDuration closeEv'
  popupClassDyn <-
    holdDyn "popup" $
      leftmost
        [ "popup" <$ closeEv'
        , "popup popup--visible" <$ showEv
        ]
  resultEvDyn <- elDynClass "div" popupClassDyn $ do
    popupOverlay
    widgetHold blank' $
      leftmost
        [ m <$> showEv
        , blank' <$ deferDomClearEv
        ]
  pure $ switchDyn $ fst <$> resultEvDyn

-- | Dark unclickable background for opened sidebar.
popupOverlay :: DomBuilder t m => m ()
popupOverlay =
  elAttr "div" ("class" =: "popup__overlay" <> "aria-hidden" =: "true") blank

-- | Button with customizable classes and label text.
buttonClass ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Text ->
  -- | Label text.
  Text ->
  m (Event t ())
buttonClass cl lbl = do
  (bEl, _) <-
    elDynAttr'
      "button"
      (constDyn $ "class" =: cl <> "type" =: "button")
      $ text lbl
  return $ domEvent Click bEl

-- | Advanced version of 'buttonClass' with dynamic arguments.
buttonDynClass ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Dynamic t Text ->
  -- | Label text.
  Dynamic t Text ->
  m (Event t ())
buttonDynClass clDyn lblDyn = do
  let attrDyn = ffor clDyn $ \cl -> "class" =: cl <> "type" =: "button"
  (bEl, _) <-
    elDynAttr' "button" attrDyn $
      dynText lblDyn
  return $ domEvent Click bEl

-- | Advanced version of 'buttonClass' with a disabled state.
buttonClassEnabled ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Text ->
  -- | Label text.
  Text ->
  -- | Enabled flag.
  Dynamic t Bool ->
  m (Event t ())
buttonClassEnabled cl lbl dDyn = do
  let attrDyn = ffor dDyn $ \case
        True -> "class" =: cl <> "type" =: "button"
        False ->
          "class" =: (cl <> " button--disabled")
            <> "type" =: "button"
            <> "disabled" =: ""
  (bEl, _) <-
    elDynAttr' "button" attrDyn $
      text lbl
  return $ domEvent Click bEl

-- | Special version of 'buttonClassEnabled' that supports custom classes for
-- the disabled state.
buttonClassEnabled' ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Text ->
  -- | Label text.
  Text ->
  -- | Enabled flag.
  Dynamic t Bool ->
  -- | Custom classes for disabled state.
  Text ->
  m (Event t ())
buttonClassEnabled' cl lbl dDyn disClass = do
  let attrDyn = ffor dDyn $ \case
        True -> "class" =: cl <> "type" =: "button"
        False ->
          "class" =: (cl <> " " <> disClass)
            <> "type" =: "button"
            <> "disabled" =: ""
  (bEl, _) <-
    elDynAttr' "button" attrDyn $
      text lbl
  return $ domEvent Click bEl

-- | Version of 'buttonClass' for links that should look like buttons.
aButtonClass ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Text ->
  -- | Label text.
  Text ->
  m (Event t ())
aButtonClass cl lbl = do
  (bEl, _) <-
    elDynAttr'
      "a"
      (constDyn $ "class" =: cl)
      $ text lbl
  return $ domEvent Click bEl

-- | Version of 'buttonClass' for links that should look like buttons.
aButtonClass' ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Text ->
  -- | Label text.
  Text ->
  -- | Extra attributes
  Dynamic t (Map Text Text) ->
  m (Event t ())
aButtonClass' cl lbl eAttrs = do
  (bEl, _) <-
    elDynAttr'
      "a"
      (fmap (<> ("class" =: cl)) eAttrs)
      $ text lbl
  return $ domEvent Click bEl

-- | Version of 'buttonDynClass' for links that should look like
-- buttons.
aButtonDynClass ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Dynamic t Text ->
  -- | Label text.
  Dynamic t Text ->
  m (Event t ())
aButtonDynClass clDyn lblDyn = do
  let attrDyn = ffor clDyn $ \cl -> "class" =: cl
  (bEl, _) <-
    elDynAttr' "a" attrDyn $
      dynText lblDyn
  return $ domEvent Click bEl

-- | Version of 'buttonDynClass' for links that should look like
-- buttons.
aButtonDynClass' ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Dynamic t Text ->
  -- | Label text.
  Dynamic t Text ->
  -- | Extra attributes
  Dynamic t (Map Text Text) ->
  m (Event t ())
aButtonDynClass' clDyn lblDyn eAttrs = do
  let attrDyn' = ffor clDyn $ \cl -> "class" =: cl
      attrDyn = (<>) <$> eAttrs <*> attrDyn'
  (bEl, _) <-
    elDynAttr' "a" attrDyn $
      dynText lblDyn
  return $ domEvent Click bEl

-- | Version of 'buttonClassEnabled' for links that should look like
-- buttons.
aButtonClassEnabled ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Classes.
  Text ->
  -- | Label text.
  Text ->
  -- | Enabled flag.
  Dynamic t Bool ->
  m (Event t ())
aButtonClassEnabled cl lbl dDyn = do
  let attrDyn = ffor dDyn $ \case
        True -> "class" =: cl
        False ->
          "class" =: (cl <> " button--disabled")
            <> "disabled" =: ""
  (bEl, _) <-
    elDynAttr' "a" attrDyn $
      text lbl
  return $ domEvent Click bEl

-- | Converter from posix seconds to `UTCTime`.
intToUTCTime :: Int -> UTCTime
intToUTCTime = posixSecondsToUTCTime . realToFrac

-- | Taken from <https://gist.github.com/3noch/134b1ee7fa48c347be9d164c3fac4ef7>
--
-- Like 'elDynAttr'' but configures "prevent default" on the given event.
elDynAttrWithPreventDefaultEvent' ::
  forall a en m t.
  (DomBuilder t m, PostBuild t m) =>
  -- | Event on the element to configure with 'preventDefault'
  EventName en ->
  -- | Element tag
  Text ->
  -- | Element attributes
  Dynamic t (Map Text Text) ->
  -- | Child of element
  m a ->
  -- | An element and the result of the child
  m (R.Element EventResult (DomBuilderSpace m) t, a)
elDynAttrWithPreventDefaultEvent' ev =
  elDynAttrWithModifyConfig'
    ( \elCfg ->
        elCfg & elementConfig_eventSpec
          %~ addEventSpecFlags
            (Proxy :: Proxy (DomBuilderSpace m))
            ev
            (const preventDefault)
    )

-- | Taken from <https://gist.github.com/3noch/134b1ee7fa48c347be9d164c3fac4ef7>
--
-- Like 'elDynAttr'' but configures "stop propagation" on the given event.
elDynAttrWithStopPropagationEvent' ::
  forall a en m t.
  (DomBuilder t m, PostBuild t m) =>
  -- | Event on the element to configure with 'preventDefault'
  EventName en ->
  -- | Element tag
  Text ->
  -- | Element attributes
  Dynamic t (Map Text Text) ->
  -- | Child of element
  m a ->
  -- | An element and the result of the child
  m (R.Element EventResult (DomBuilderSpace m) t, a)
elDynAttrWithStopPropagationEvent' ev =
  elDynAttrWithModifyConfig'
    ( \elCfg ->
        elCfg & elementConfig_eventSpec
          %~ addEventSpecFlags
            (Proxy :: Proxy (DomBuilderSpace m))
            ev
            (const stopPropagation)
    )

-- | Taken from <https://gist.github.com/3noch/134b1ee7fa48c347be9d164c3fac4ef7>
--
-- Like 'elDynAttr'' but allows you to modify the element configuration.
--
-- Special thanks to @luigy:
-- <https://gist.github.com/luigy/b49ce04de8462e594c9c2b5b455ae5a5#file-foo-hs>
elDynAttrWithModifyConfig' ::
  (DomBuilder t m, PostBuild t m) =>
  ( ElementConfig EventResult t (DomBuilderSpace m) ->
    ElementConfig EventResult t (DomBuilderSpace m)
  ) ->
  Text ->
  Dynamic t (Map Text Text) ->
  m a ->
  m (R.Element EventResult (DomBuilderSpace m) t, a)
elDynAttrWithModifyConfig' f elementTag attrs child = do
  modifyAttrs <- dynamicAttributesToModifyAttributes attrs
  let cfg =
        def & modifyAttributes .~ fmapCheap mapKeysToAttributeName modifyAttrs
  result <- R.element elementTag (f cfg) child
  postBuild <- getPostBuild
  notReadyUntil postBuild
  pure result

-- | Formats posix seconds to date in iso8601.
formatPosixToDate :: Int -> Text
formatPosixToDate =
  pack
    . formatTime defaultTimeLocale (iso8601DateFormat Nothing)
    . intToUTCTime

-- | Formats posix seconds to date in iso8601 with time.
formatPosixToDateTime :: Int -> Text
formatPosixToDateTime =
  pack
    . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
    . intToUTCTime

-- | Widget displaying the current deployment status.
statusWidget :: MonadWidget t m => Dynamic t PreciseDeploymentStatus -> m ()
statusWidget stDyn = do
  stDyn' <- holdUniqDyn stDyn
  let loadingWidget = divClass "loading loading--status-alike"
  dyn_ $
    stDyn' <&> \case
      DeploymentPending _ -> divClass "status status--pending" $ text "Pending..."
      DeploymentNotPending Running -> divClass "status status--success" $ text "Running"
      DeploymentNotPending (Failure _) -> divClass "status status--failure" $ text "Failure"
      DeploymentNotPending CreatePending -> loadingWidget $ text "Creating..."
      DeploymentNotPending UpdatePending -> loadingWidget $ text "Updating..."
      DeploymentNotPending ArchivePending -> loadingWidget $ text "Archiving..."
      DeploymentNotPending Archived -> divClass "status status--archived" $ text "Archived"

-- | Text input field with label.
octopodTextInput ::
  MonadWidget t m =>
  -- | Input field classes.
  Text ->
  -- | Label text.
  Text ->
  -- | Placeholder for input field.
  Text ->
  -- | Initial value.
  Maybe Text ->
  -- | Event carrying error message.
  Event t Text ->
  m (Dynamic t Text, Dynamic t Bool)
octopodTextInput clss lbl placeholder val errEv =
  elClass "section" "deployment__section" $ do
    elClass "h3" "deployment__sub-heading" $ text lbl
    elClass "div" "deployment__widget" $
      octopodTextInput' clss placeholder val errEv

-- | The only text input field that is used in project forms. This input
-- provides automatic error message hiding after user starts typing.
octopodTextInput' ::
  MonadWidget t m =>
  -- | Input field classes.
  Text ->
  -- | Placeholder for input field.
  Text ->
  -- | Possible init value.
  Maybe Text ->
  -- | Event carrying the error message.
  Event t Text ->
  m (Dynamic t Text, Dynamic t Bool)
octopodTextInput' clss placeholder val errEv = mdo
  let inpClass = " input"
      inpErrClass = " input input--error"
  isValid <-
    holdDyn True $
      leftmost
        [ False <$ errEv
        , True <$ updated valDyn
        ]
  classDyn <-
    holdDyn (clss <> inpClass) $
      leftmost
        [ (clss <> inpErrClass) <$ errEv
        , (clss <> inpClass) <$ updated valDyn
        ]
  valDyn <- elDynClass "div" classDyn $ do
    inp <-
      inputElement $
        def
          & initialAttributes
            .~ ( "type" =: "text"
                  <> "class" =: "input__widget"
                  <> "placeholder" =: placeholder
               )
          & inputElementConfig_initialValue .~ fromMaybe "" val
    widgetHold_ blank $
      leftmost
        [ divClass "input__output" . text <$> errEv
        , blank <$ updated valDyn
        ]
    pure $ value inp
  pure (valDyn, isValid)

-- | Widget with a loading spinner.
loadingCommonWidget :: MonadWidget t m => m ()
loadingCommonWidget =
  divClass "loading loading--enlarged loading--alternate" $
    text "Loading..."

-- | Widget with an error message.
errorCommonWidget :: MonadWidget t m => m ()
errorCommonWidget =
  divClass "null null--data" $
    divClass "null__content" $ do
      elClass "b" "null__heading" $ text "Cannot retrieve the data"
      divClass "null__message" $ text "Try to reload the page"

-- | Widget that can show and hide overrides if there are more than 3. This
-- widget is used in the deployments table and the deployment action table.
overridesWidget ::
  MonadWidget t m =>
  -- | List of overrides.
  Overrides ->
  m ()
overridesWidget envs = divClass "listing listing--for-text" $ do
  let visible = take 3 envs
      envLength = length envs
  listing visible
  when (envLength > 3) $ mdo
    let hidden = drop 3 envs
    showDyn <- toggle False toggleEv
    dyn_ $
      showDyn <&> \case
        True -> listing hidden
        False -> blank
    let btnClassDyn =
          ifThenElseDyn
            showDyn
            "listing__more expander expander--open"
            "listing__more expander"
        btnTextDyn =
          ifThenElseDyn showDyn "Hide" $
            "Show all (" <> (showT $ envLength) <> ")"
    toggleEv <- buttonDynClass btnClassDyn btnTextDyn
    blank
  where
    listing envs' = do
      forM_ envs' $ \(Override var val _) ->
        divClass "listing__item" $ do
          el "b" $ text $ var <> ": "
          text val

-- | @if-then-else@ helper for cases when bool value is wrapped in 'Dynamic'.
ifThenElseDyn ::
  Reflex t =>
  -- | Condition wrapped in `Dynamic`.
  Dynamic t Bool ->
  -- | `then` branch.
  b ->
  -- | `else` branch.
  b ->
  Dynamic t b
ifThenElseDyn bDyn t f =
  bDyn <&> \case
    True -> t
    False -> f

-- | Type of notification at the top of pages.
data DeploymentPageNotification
  = -- | Message text for a \"success\" events.
    DPMOk Text
  | -- | Message text for an \"error\" events.
    DPMError Text
  | -- | Clear notification widget.
    DPMClear

-- | Notification widget that can show success and failure messages.
pageNotification ::
  MonadWidget t m =>
  -- | Event carrying notifications.
  Event t DeploymentPageNotification ->
  m ()
pageNotification notEv = mdo
  let messageWidget (DPMOk txt) = messageClassWidget txt "notification--success"
      messageWidget (DPMError txt) = messageClassWidget txt "notification--danger"
      messageWidget DPMClear = pure never
      messageClassWidget txt cl =
        divClass ("page__output notification " <> cl) $ do
          text txt
          buttonClass "notification__close" ""
      closeEv = switchDyn closeEvDyn
  closeEvDyn <-
    widgetHold (pure never) $
      leftmost
        [ messageWidget <$> notEv
        , pure never <$ closeEv
        ]
  blank

kubeDashboardUrl ::
  (MonadReader ProjectConfig m, MonadWidget t m) =>
  Dynamic t DeploymentFullInfo ->
  m (Dynamic t (Maybe Text))
kubeDashboardUrl deploymentInfo = do
  template <- asks kubernetesDashboardUrlTemplate
  let name = unDeploymentName . view (#deployment . #name) <$> deploymentInfo
  return $ name <&> (\n -> (<> n) <$> template)
