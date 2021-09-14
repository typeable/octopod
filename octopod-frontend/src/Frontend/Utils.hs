-- |
--Module      : Frontend.Utils
--Description : Client utils and helpers.
--
--This module contains common types, functions and operators that are used by
--frontend modules.
module Frontend.Utils
  ( sidebar,
    buttonClass,
    buttonClassEnabled,
    wrapRequestErrors,
    octopodTextInput,
    deploymentPopupBody,
    ClickedElement (..),
    pageNotification,
    aButtonClassEnabled,
    buttonClassEnabled',
    kubeDashboardUrl,
    loadingCommonWidget,
    errorCommonWidget,
    aButtonDynClass',
    formatPosixToDate,
    overridesWidget,
    aButtonClass',
    statusWidget,
    elementClick,
    showT,
    DeploymentPageNotification (..),
    formatPosixToDateTime,
    dropdownWidget,
    dropdownWidget',
    buttonDynClass,
    deploymentConfigProgressiveComponents,
    deploymentConfigProgressive,
    holdClearingWith,
    unitEv,
    deploymentSection,
  )
where

import Common.Types as CT
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Align
import qualified Data.Foldable as F
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Sum
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Ordered.Strict as OM
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text as T (Text, intercalate, null, pack)
import Data.These
import Data.Time
import Data.UniqMap
import Data.Unique
import Data.Witherable
import Data.WorkingOverrides
import Frontend.API
import Frontend.GHCJS
import GHCJS.DOM
import GHCJS.DOM.Element as DOM
import GHCJS.DOM.EventM (on, target)
import GHCJS.DOM.GlobalEventHandlers as Events (click)
import GHCJS.DOM.Node as DOM
import Reflex.Dom as R
import Reflex.Network
import Servant.Common.Req
import Servant.Reflex.Extra

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

-- | Formats posix seconds to date in iso8601.
formatPosixToDate :: FormatTime t => t -> Text
formatPosixToDate = pack . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

-- | Formats posix seconds to date in iso8601 with time.
formatPosixToDateTime :: FormatTime t => t -> Text
formatPosixToDateTime =
  pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))

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
      octopodTextInput' (pure False) clss placeholder (pure . fromMaybe "" $ val) errEv

-- | The only text input field that is used in project forms. This input
-- provides automatic error message hiding after user starts typing.
octopodTextInput' ::
  MonadWidget t m =>
  -- | Disabled?
  Dynamic t Bool ->
  -- | Input field classes.
  Text ->
  -- | Placeholder for input field.
  Text ->
  -- | Possible init value.
  (Dynamic t Text) ->
  -- | Event carrying the error message.
  Event t Text ->
  m (Dynamic t Text, Dynamic t Bool)
octopodTextInput' disabledDyn clss placeholder inValDyn' errEv = mdo
  inValDyn <- holdUniqDyn inValDyn'
  let inValEv =
        align (updated inValDyn) (updated valDyn)
          & fmapMaybe
            ( \case
                This x -> Just x
                These inV currV | inV /= currV -> Just inV
                _ -> Nothing
            )
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
  inVal <- sample . current $ inValDyn
  disabled <- sample . current $ disabledDyn
  valDyn <- elDynClass "div" classDyn $ do
    inp <-
      inputElement $
        def
          & initialAttributes
            .~ ( "type" =: "text"
                  <> "class" =: "input__widget"
                  <> "placeholder" =: placeholder
               )
          & inputElementConfig_setValue .~ inValEv
          & inputElementConfig_initialValue .~ inVal
          & inputElementConfig_elementConfig . elementConfig_initialAttributes
            %~ (if disabled then M.insert "disabled" "disabled" else id)
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes
            <>~ updated
              ( do
                  disabled' <- disabledDyn
                  pure $
                    M.singleton "disabled" $
                      if disabled' then Just "disabled" else Nothing
              )
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
  Overrides l ->
  m ()
overridesWidget (Overrides (OM.assocs -> envs)) = divClass "listing listing--for-text" $ do
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
            "Show all (" <> showT envLength <> ")"
    toggleEv <- buttonDynClass btnClassDyn btnTextDyn
    blank
  where
    listing envs' = do
      forM_ envs' $ \(var, val) ->
        divClass "listing__item" $ do
          el "b" $ text $ var <> ": "
          case val of
            ValueAdded v -> text v
            ValueDeleted -> el "i" $ text "<deleted>"

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

deploymentPopupBody ::
  forall t m tag.
  MonadWidget t m =>
  RequestErrorHandler t m ->
  Maybe DeploymentTag ->
  Overrides 'ApplicationLevel ->
  Overrides 'DeploymentLevel ->
  -- | \"Edit request\" failure event.
  Event t (ReqResult tag CommandResponse) ->
  -- | Returns deployment update and validation state.
  m (Dynamic t (Maybe DeploymentUpdate))
deploymentPopupBody hReq defTag defAppOv defDepOv errEv = mdo
  pb <- getPostBuild
  (defDepEv, defApp, depCfgEv) <- deploymentConfigProgressiveComponents hReq deploymentOvsDyn
  defAppM <- holdClearingWith defApp (unitEv deploymentOvsDyn)
  defDep <- holdDynMaybe defDepEv
  depKeys <- deploymentOverrideKeys pb >>= hReq

  let commandResponseEv = fmapMaybe commandResponse errEv
      tagErrEv = getTagError commandResponseEv tagDyn
  void $ hReq (errEv `R.difference` commandResponseEv)

  (tagDyn, tOkEv) <- octopodTextInput "tag" "Tag" "Tag" (unDeploymentTag <$> defTag) tagErrEv

  let holdDCfg :: Dynamic t (Maybe (DefaultConfig l)) -> Overrides l -> m (Dynamic t (Overrides l))
      holdDCfg dCfgDyn ovs = mdo
        ovsDyn <- holdDyn ovs ovsEv
        x <- attachDyn (current ovsDyn) dCfgDyn
        ovsEv <- dyn (x <&> \(ovs', dCfg) -> envVarsInput dCfg ovs') >>= switchHold never >>= debounce 0.5
        pure ovsDyn
      holdDKeys n keysDyn = do
        let loading = loadingCommonWidget
        deploymentSection n $
          widgetHold_ loading $
            keysDyn <&> \case
              Just keys -> el "ul" $
                forM_ keys $ \key -> el "li" $ text key
              Nothing -> loading

  deploymentOvsDyn <- deploymentSection "Deployment overrides" $ holdDCfg defDep defDepOv
  holdDKeys "Deployment keys" (Just <$> depKeys)
  appKeys <- waitForValuePromptly depCfgEv $ \deploymentCfg -> do
    pb' <- getPostBuild
    applicationOverrideKeys (pure $ Right deploymentCfg) pb' >>= hReq >>= immediateNothing
  applicationOvsDyn <- deploymentSection "App overrides" $ holdDCfg defAppM defAppOv
  holdDKeys "App keys" appKeys

  validDyn <- holdDyn True $ updated tOkEv
  pure $
    validDyn >>= \case
      False -> pure Nothing
      True -> do
        depCfg <- deploymentOvsDyn
        appOvs <- applicationOvsDyn
        tag' <- DeploymentTag <$> tagDyn
        pure $
          Just $
            DeploymentUpdate
              { newTag = tag'
              , appOverrides = appOvs
              , deploymentOverrides = depCfg
              }
  where
    getTagError crEv tagDyn =
      let tagErrEv' = fmapMaybe (preview (_Ctor @"ValidationError" . _2)) crEv
          tagErrEv = ffilter (/= "") $ T.intercalate ". " <$> tagErrEv'
          badTagText = "Tag should not be empty"
          badNameEv = badTagText <$ ffilter (== "") (updated tagDyn)
       in leftmost [tagErrEv, badNameEv]

deploymentConfigProgressiveComponents ::
  MonadWidget t m =>
  RequestErrorHandler t m ->
  Dynamic t (Overrides 'DeploymentLevel) ->
  m
    ( Event t (DefaultConfig 'DeploymentLevel)
    , Event t (DefaultConfig 'ApplicationLevel)
    , Event t (Config 'DeploymentLevel)
    )
deploymentConfigProgressiveComponents hReq depOvsDyn = do
  pb <- getPostBuild
  defDepEv <- defaultDeploymentOverrides pb >>= hReq
  defDepMDyn <- holdDynMaybe defDepEv
  let depCfgEv = catMaybes . updated $ do
        defDepM <- defDepMDyn
        depOvs <- depOvsDyn
        pure $ defDepM <&> applyOverrides depOvs
  defAppEv <-
    fmap switchDyn $
      networkHold (pure never) $
        depCfgEv <&> \depCfg -> do
          pb' <- getPostBuild
          defaultApplicationOverrides (pure $ Right depCfg) pb' >>= hReq
  pure (defDepEv, defAppEv, depCfgEv)

deploymentConfigProgressive ::
  MonadWidget t m =>
  RequestErrorHandler t m ->
  Dynamic t (Overrides 'DeploymentLevel) ->
  Dynamic t (Overrides 'ApplicationLevel) ->
  m (Event t FullConfig)
deploymentConfigProgressive hReq depOvsDyn appOvsDyn = do
  (_, defAppEv, depCfgEv) <- deploymentConfigProgressiveComponents hReq depOvsDyn
  defAppMDyn <- holdDynMaybe defAppEv
  depCfgMDyn <- holdDynMaybe depCfgEv
  pure . catMaybes . updated $ do
    defAppM <- defAppMDyn
    appOvs <- appOvsDyn
    depCfgM <- depCfgMDyn
    pure $ do
      defApp <- defAppM
      depCfg <- depCfgM
      pure
        FullConfig
          { appConfig = applyOverrides appOvs defApp
          , depConfig = depCfg
          }

waitForValuePromptly :: (MonadHold t m, Adjustable t m) => Event t x -> (x -> m (Event t y)) -> m (Event t y)
waitForValuePromptly ev f = fmap switchPromptlyDyn $ networkHold (pure never) $ f <$> ev

type RequestErrorHandler t m = forall tag a. Event t (ReqResult tag a) -> m (Event t a)

wrapRequestErrors ::
  MonadWidget t m =>
  ( forall w.
    Semigroup w =>
    RequestErrorHandler t (EventWriterT t w m) ->
    EventWriterT t w m x
  ) ->
  m x
wrapRequestErrors f = mdo
  errs <- foldDyn (flip (<>)) mempty ev
  void $ list (patchMapNewElementsMap <$> errs) errorHeader
  (x, ev :: Event t (PatchMap Unique Text)) <- runEventWriterT $
    f $ \reqEv -> do
      k <- liftIO newUnique
      tellEvent $ fmapCheap (PatchMap . M.singleton k . reqErrorBody) reqEv
      pure $ fmapMaybeCheap reqSuccess reqEv
  pure x

-- | The widget used to display errors.
errorHeader ::
  MonadWidget t m =>
  -- | Message text.
  Dynamic t Text ->
  m ()
errorHeader appErr = do
  divClass "deployment__output notification notification--danger" $ do
    el "b" $ text "App error: "
    dynText appErr

-- | Widget with override fields. This widget supports adding and
-- removing key-value pairs.
envVarsInput ::
  forall l t m.
  MonadWidget t m =>
  Maybe (DefaultConfig l) ->
  -- | Initial deployment overrides.
  Overrides l ->
  -- | Updated deployment overrides.
  m (Event t (Overrides l))
envVarsInput dCfg ovs = mdo
  envsDyn <- foldDyn appEndo (constructWorkingOverrides dCfg ovs) $ leftmost [addEv, updEv]
  let addEv = clickEv $> Endo (fst . insertUniqStart newWorkingOverride)
  clickEv <-
    buttonClassEnabled'
      "overrides__add dash dash--add"
      "Add an override"
      addingIsEnabled
      "dash--disabled"
  updEv <-
    switchDyn . fmap F.fold
      <$> listWithKey
        (uniqMap <$> envsDyn)
        (\i x -> fmap (performUserOverrideAction (lookupDefaultConfig <$> dCfg) i) <$> envVarInput x)
  let addingIsEnabled = all (\(WorkingOverrideKey _ x, _) -> not . T.null $ x) . elemsUniq <$> envsDyn
  case dCfg of
    Just _ -> pure ()
    Nothing -> loadingCommonWidget
  pure . updated $ destructWorkingOverrides <$> envsDyn

deploymentSection :: DomBuilder t m => Text -> m a -> m a
deploymentSection n m = elClass "section" "deployment__section" $ do
  elClass "h3" "deployment__sub-heading" $ text n
  elClass "div" "deployment__widget" m

-- | Widget for entering a key-value pair. The updated overrides list is
-- written to the 'EventWriter'.
envVarInput ::
  (MonadWidget t m) =>
  -- | Current variable key and value.
  Dynamic t WorkingOverride ->
  m (Event t UserOverrideAction)
envVarInput val = do
  let v =
        val <&> snd <&> \case
          WorkingCustomValue x -> x
          WorkingDefaultValue x -> x
          WorkingDeletedValue (Just x) -> x
          WorkingDeletedValue Nothing -> "<loading deleted>"
      k = val <&> \(WorkingOverrideKey _ x, _) -> x
      disabledKey = val <&> \(WorkingOverrideKey t _, _) -> t == DefaultWorkingOverrideKey

  divClass "overrides__item" $ do
    (keyTextDyn, _) <-
      octopodTextInput' disabledKey "overrides__key" "key" k never
    (valTextDyn, _) <-
      octopodTextInput' (pure False) "overrides__value" "value" v never
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    pure $
      leftmost
        [ UpdateKey <$> updated keyTextDyn
        , UpdateValue <$> updated valTextDyn
        , closeEv $> DeleteOverride
        ]

data UserOverrideAction = UpdateKey !Text | UpdateValue !Text | DeleteOverride

performUserOverrideAction ::
  Maybe (Text -> Maybe Text) ->
  Int ->
  UserOverrideAction ->
  Endo WorkingOverrides
performUserOverrideAction f i (UpdateValue v) = Endo $
  updateUniq i $ \(k@(WorkingOverrideKey _ kt), _) ->
    ( k
    , case f >>= ($ kt) of
        Just v' | v == v' -> WorkingDefaultValue v
        _ -> WorkingCustomValue v
    )
performUserOverrideAction f i (UpdateKey k) = Endo $ updateUniq i $ \(_, v) -> (WorkingOverrideKey t k, v)
  where
    t = case f >>= ($ k) of
      Nothing -> CustomWorkingOverrideKey
      Just _ -> DefaultWorkingOverrideKey
performUserOverrideAction f i DeleteOverride = Endo $ \m ->
  case f of
    Nothing -> updateUniq i (\(k, _) -> (k, WorkingDeletedValue Nothing)) m
    Just f' -> case lookupUniq i m of
      Nothing -> m
      Just (WorkingOverrideKey _ k, _) -> case f' k of
        Nothing -> deleteUniq i m
        Just v -> updateUniq i (const (WorkingOverrideKey DefaultWorkingOverrideKey k, WorkingDeletedValue (Just v))) m

holdDynMaybe :: (Reflex t, MonadHold t m) => Event t a -> m (Dynamic t (Maybe a))
holdDynMaybe ev = holdDyn Nothing $ fmapCheap Just ev

immediateNothing :: (PostBuild t m) => Event t a -> m (Event t (Maybe a))
immediateNothing ev = do
  pb <- getPostBuild
  pure $ leftmost [pb $> Nothing, fmapCheap Just ev]

attachDyn :: (Reflex t, MonadHold t m) => Behavior t a -> Dynamic t b -> m (Dynamic t (a, b))
attachDyn b d = do
  currD <- sample . current $ d
  currB <- sample b
  holdDyn (currB, currD) (attach b $ updated d)

holdClearingWith :: (Reflex t, MonadHold t m) => Event t a -> Event t () -> m (Dynamic t (Maybe a))
holdClearingWith aEv clear =
  holdDyn Nothing $ leftmost [fmapCheap Just aEv, fmapCheap (const Nothing) clear]

unitEv :: Reflex t => Dynamic t a -> Event t ()
unitEv = fmapCheap (const ()) . updated
