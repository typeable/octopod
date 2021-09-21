-- |
--Module      : Frontend.Utils
--Description : Client utils and helpers.
--
--This module contains common types, functions and operators that are used by
--frontend modules.
module Frontend.Utils
  ( sidebar,
    wrapRequestErrors,
    octopodTextInput,
    deploymentPopupBody,
    ClickedElement (..),
    pageNotification,
    kubeDashboardUrl,
    formatPosixToDate,
    overridesWidget,
    statusWidget,
    elementClick,
    showT,
    DeploymentPageNotification (..),
    formatPosixToDateTime,
    dropdownWidget,
    dropdownWidget',
    deploymentConfigProgressiveComponents,
    deploymentConfigProgressive,
    holdClearingWith,
    unitEv,
    (<&&>),
    ProgressiveFullConfig (..),
    RequestErrorHandler,
    deploymentOverridesWidget,
    applicationOverridesWidget,
  )
where

import Common.Types as CT
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Sum
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid
import Data.Text as T (Text, intercalate, null, pack)
import Data.Time
import Data.UniqMap
import Data.Unique
import Data.Witherable
import Data.WorkingOverrides
import Frontend.API
import Frontend.GHCJS
import Frontend.UIKit
import GHC.Generics (Generic)
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
  Classes ->
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
    elClass "div" "deployment__widget" $ do
      (value -> valDyn, validDyn) <- octopodTextInput' (pure []) (pure False) (pure clss) placeholder (pure . fromMaybe "" $ val) errEv
      pure (valDyn, validDyn)

-- | Widget that can show and hide overrides if there are more than 3. This
-- widget is used in the deployments table and the deployment action table.
overridesWidget ::
  MonadWidget t m =>
  -- | List of overrides.
  Overrides l ->
  (Event t () -> m (Event t (DefaultConfig l))) ->
  m ()
overridesWidget ovs getDef = divClass "listing listing--for-text" $ mdo
  defMDyn <- getDef firstExpand >>= holdDynMaybe
  dyn_ $
    expandState <&> \case
      ExpandedState ->
        void $
          networkView $
            do
              defM <- defMDyn
              pure $
                showNonEditableWorkingOverride (isNothing defM) RegularNonEditableWorkingOverrideStyle $
                  elemsUniq $ constructWorkingOverrides defM ovs
      ContractedState ->
        let ovsList = elemsUniq $ constructWorkingOverrides Nothing ovs
         in showNonEditableWorkingOverride False RegularNonEditableWorkingOverrideStyle $
              take 3 ovsList

  expandState <-
    expanderButton
      ExpanderButtonConfig
        { buttonText = do
            state <- expandState
            pure $ case state of
              ExpandedState -> "Hide"
              ContractedState -> "Show all"
        , buttonInitialState = ContractedState
        , buttonType = Just ListingExpanderButton
        , buttonStyle = RegularExpanderButtonStyle
        }
  firstExpand <- headE $ ($> ()) $ ffilter (== ExpandedState) $ updated expandState
  pure ()

deploymentOverridesWidget ::
  MonadWidget t m =>
  RequestErrorHandler t m ->
  Overrides 'DeploymentLevel ->
  m ()
deploymentOverridesWidget hReq depOvs =
  overridesWidget depOvs $ defaultDeploymentOverrides >=> hReq

applicationOverridesWidget ::
  MonadWidget t m =>
  RequestErrorHandler t m ->
  Overrides 'DeploymentLevel ->
  Overrides 'ApplicationLevel ->
  m ()
applicationOverridesWidget hReq depOvs appOvs =
  overridesWidget appOvs $ \fire -> do
    depDefEv <- defaultDeploymentOverrides fire >>= hReq
    fmap switchDyn $
      networkHold (pure never) $
        depDefEv <&> \depDef -> do
          pb <- getPostBuild
          defaultApplicationOverrides (pure $ Right $ applyOverrides depOvs depDef) pb >>= hReq

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
          closeNotificationButton
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

  let holdDCfg ::
        Dynamic t [Text] ->
        Dynamic t (Maybe (DefaultConfig l)) ->
        Overrides l ->
        m (Dynamic t (Overrides l))
      holdDCfg values dCfgDyn ovs = mdo
        ovsDyn <- holdDyn ovs ovsEv
        x <- attachDyn (current ovsDyn) dCfgDyn
        ovsEv <- dyn (x <&> \(ovs', dCfg) -> envVarsInput values dCfg ovs') >>= switchHold never >>= debounce 0.5
        pure ovsDyn

  depKeysDyn <- holdDyn [] depKeys
  deploymentOvsDyn <- deploymentSection "Deployment overrides" $ holdDCfg depKeysDyn defDep defDepOv

  appKeys <- waitForValuePromptly depCfgEv $ \deploymentCfg -> do
    pb' <- getPostBuild
    applicationOverrideKeys (pure $ Right deploymentCfg) pb' >>= hReq >>= immediateNothing
  appKeysDyn <- holdDyn [] $ catMaybes appKeys
  applicationOvsDyn <- deploymentSection "App overrides" $ holdDCfg appKeysDyn defAppM defAppOv

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
  m (Dynamic t ProgressiveFullConfig)
deploymentConfigProgressive hReq depOvsDyn appOvsDyn = do
  (depCfgEv, defAppEv, _) <- deploymentConfigProgressiveComponents hReq depOvsDyn
  defAppMDyn <- holdDynMaybe defAppEv
  defCfgMDyn <- holdDynMaybe depCfgEv
  pure $ do
    defAppM <- defAppMDyn
    appOvs <- appOvsDyn
    defDepM <- defCfgMDyn
    depOvs <- depOvsDyn
    pure
      ProgressiveFullConfig
        { appConfig = constructWorkingOverrides defAppM appOvs
        , appConfigLoading = isNothing defAppM
        , depConfig = constructWorkingOverrides defDepM depOvs
        , depConfigLoading = isNothing defDepM
        }

data ProgressiveFullConfig = ProgressiveFullConfig
  { appConfig :: WorkingOverrides
  , appConfigLoading :: Bool
  , depConfig :: WorkingOverrides
  , depConfigLoading :: Bool
  }
  deriving stock (Generic)

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
  Dynamic t [Text] ->
  Maybe (DefaultConfig l) ->
  -- | Initial deployment overrides.
  Overrides l ->
  -- | Updated deployment overrides.
  m (Event t (Overrides l))
envVarsInput values dCfg ovs = mdo
  envsDyn <- foldDyn appEndo (constructWorkingOverrides dCfg ovs) $ leftmost [addEv, updEv]
  let addEv = clickEv $> Endo (fst . insertUniqStart newWorkingOverride)
  clickEv <-
    dashButton
      DashButtonConfig
        { buttonText = "Add an override"
        , buttonEnabled = addingIsEnabled
        , buttonType = Just AddDashButtonType
        , buttonStyle = OverridesDashButtonStyle
        }
  updEv <-
    switchDyn . fmap F.fold
      <$> listWithKey
        (uniqMap <$> envsDyn)
        (\i x -> fmap (performUserOverrideAction (lookupDefaultConfig <$> dCfg) i) <$> envVarInput values x)
  let addingIsEnabled = all (\(WorkingOverrideKey _ x, _) -> not . T.null $ x) . elemsUniq <$> envsDyn
  case dCfg of
    Just _ -> pure ()
    Nothing -> loadingOverrides
  pure . updated $ destructWorkingOverrides <$> envsDyn

-- | Widget for entering a key-value pair. The updated overrides list is
-- written to the 'EventWriter'.
envVarInput ::
  (MonadWidget t m) =>
  -- | The key values to suggest to the user
  Dynamic t [Text] ->
  -- | Current variable key and value.
  Dynamic t WorkingOverride ->
  m (Event t UserOverrideAction)
envVarInput values val = do
  let kDyn = val <&> \(WorkingOverrideKey _ x, _) -> x
  -- Either <override present> <override deleted>
  d <-
    eitherDyn $
      val <&> snd <&> \case
        WorkingCustomValue v -> Right (v, EditedOverrideFieldType)
        WorkingDefaultValue v -> Right (v, DefaultOverrideFieldType)
        WorkingDeletedValue v -> Left v
  networkView >=> switchHold never $
    d <&> \case
      Right vtDyn -> do
        let (vDyn, vTypeDyn) = splitDynPure vtDyn
        (keyTextDyn, valTextDyn, closeEv) <-
          overrideField
            values
            OverrideField
              { fieldValue = kDyn
              , fieldError = never
              , fieldDisabled =
                  val <&> \(WorkingOverrideKey t _, _) -> t == DefaultWorkingOverrideKey
              , fieldType =
                  val <&> \(WorkingOverrideKey t _, _) -> case t of
                    CustomWorkingOverrideKey -> EditedOverrideFieldType
                    DefaultWorkingOverrideKey -> DefaultOverrideFieldType
              }
            OverrideField
              { fieldValue = vDyn
              , fieldError = never
              , fieldDisabled = pure False
              , fieldType = vTypeDyn
              }
        pure $
          leftmost
            [ UpdateKey <$> updated keyTextDyn
            , UpdateValue <$> updated valTextDyn
            , closeEv $> DeleteOverride
            ]
      Left vDyn -> do
        restoreEv <-
          networkView >=> switchHold never $ do
            v <- vDyn
            k <- kDyn
            pure $ deletedOverride k v
        pure $
          flip push restoreEv $ \() -> do
            v <- sample . current $ vDyn
            pure $ UpdateValue <$> v

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

(<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
x <&&> f = (fmap . fmap) f x
