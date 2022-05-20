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
    deploymentConfigProgressiveComponents,
    -- deploymentConfigProgressive,
    holdClearingWith,
    unitEv,
    (<&&>),
    RequestErrorHandler,
    debounceDyn,
    catchReturns,
    holdDynMaybe,
    constructWorkingOverridesEv,
  )
where

import Common.Types as CT
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import qualified Data.ConfigTree as CT
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Sum
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text as T (Text, null, pack, strip)
import Data.Time
import Data.UniqMap
import Data.Unique
import Data.Witherable
import Data.WorkingOverrides
import Debug.Trace
import Frontend.API
import Frontend.GHCJS
import Frontend.UIKit
import GHCJS.DOM
import GHCJS.DOM.Element as DOM
import GHCJS.DOM.EventM (on, target)
import GHCJS.DOM.GlobalEventHandlers as Events (click)
import Reflex.Dom as R
import Reflex.Dom.Renderable
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

-- | Similar to 'dropdownWidget' but uses @document click@ event that may be
-- shared between other widgets.
dropdownWidget ::
  MonadWidget t m =>
  -- | Widget with the dropdown list.
  -- Returns an event carrying the user's selection.
  m (Event t a) ->
  m (Event t a)
dropdownWidget body = mdo
  openedDyn <- toggle False $ btnEl
  let wrapperClassDyn = ffor openedDyn $ \case
        True -> "class" =: "drop drop--actions drop--expanded"
        False -> "class" =: "drop drop--actions"
  (btnEl, wEv) <- elDynAttr
    "div"
    wrapperClassDyn
    $ do
      clickedEv <- dropButton
      aEv <-
        elDynAttr
          "div"
          (constDyn $ "class" =: "drop__dropdown")
          body
      pure (clickedEv, aEv)
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
      DeploymentNotPending CleanupFailed -> divClass "status status--failure" $ text "Cleanup failed (contact admin)"

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
      (value -> valDyn, validDyn) <- octopodTextInput' (pure []) (pure False) (pure mempty) (pure clss) placeholder (pure . fromMaybe "" $ val) errEv
      pure (valDyn, validDyn)

-- | Widget that can show and hide overrides if there are more than 3. This
-- widget is used in the deployments table and the deployment action table.
overridesWidget ::
  (MonadWidget t m, Renderable te, IsString te, Monoid te) =>
  -- | List of overrides.
  Overrides' te l ->
  m ()
overridesWidget (Overrides ovs) = mdo
  let l = CT.toFlatList ovs
      expandable = length l > 3

  if expandable
    then mdo
      dyn_ $
        expandState <&> \s -> do
          let l' = case s of
                ExpandedState -> l
                ContractedState -> take 3 l
          showFlatConfig l'
          pure ()

      expandState <-
        expanderButton
          ExpanderButtonConfig
            { buttonText = do
                s <- expandState
                pure $ case s of
                  ExpandedState -> "Hide default configuration"
                  ContractedState -> "Show full configuration"
            , buttonInitialState = ContractedState
            , buttonType = Just ListingExpanderButton
            , buttonStyle = RegularExpanderButtonStyle
            }
      pure ()
    else showFlatConfig l
  pure ()

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
  Overrides 'ApplicationLevel ->
  Overrides 'DeploymentLevel ->
  -- | \"Edit request\" failure event.
  Event t (ReqResult tag CommandResponse) ->
  -- | Returns deployment update and validation state.
  m (Dynamic t (Maybe DeploymentUpdate))
deploymentPopupBody hReq defAppOv defDepOv errEv = mdo
  pb <- getPostBuild
  (defDepEv, defAppEv, depCfgEv) <-
    deploymentConfigProgressiveComponents hReq deploymentOvsDynDebounced

  defAppEvM <- holdClearingWith defAppEv (unitEv deploymentOvsDyn)

  depKeys <- deploymentOverrideKeys pb >>= hReq

  let commandResponseEv = fmapMaybe commandResponse errEv
  void $ hReq (errEv `R.difference` commandResponseEv)

  let holdDCfg ::
        Dynamic t [Text] ->
        (DefaultConfig l) ->
        Overrides l ->
        m (Dynamic t (Overrides l), Dynamic t (Overrides l))
      holdDCfg (traceDyn "values" -> values) cfgDyn@(DefaultConfig ct) ovs = mdo
        let ovs' = constructWorkingOverrides cfgDyn ovs
            lookupOverride k = CT.lookup (CT.deconstructConfigKey k) ct
        (resCfgDyn :: Dynamic t [Dynamic t WorkingOverride]) <- envVarsInput values lookupOverride ovs'
        (resEvDebounced :: Event t [Dynamic t WorkingOverride]) <- debounce 2 $ updated resCfgDyn
        let resDyn' = join . fmap destructWorkingOverridesDyn $ resCfgDyn
        debouncedResDyn <- fmap join . holdDyn (pure ovs) . fmap destructWorkingOverridesDyn $ resEvDebounced
        pure (resDyn', debouncedResDyn)

  depKeysDyn <- holdDyn [] depKeys

  (join . fmap fst &&& join . fmap snd -> (deploymentOvsDyn, deploymentOvsDynDebounced)) <-
    deploymentSection "Deployment configuration" $
      networkHold (loadingOverrides $> (pure defDepOv, pure defDepOv)) $
        defDepEv <&> \defDep -> holdDCfg depKeysDyn defDep defDepOv

  appKeys <- waitForValuePromptly depCfgEv $ \deploymentCfg -> do
    pb' <- getPostBuild
    applicationOverrideKeys (pure $ Right deploymentCfg) pb' >>= hReq >>= immediateNothing

  appKeysDyn <- holdDyn [] $ catMaybes appKeys
  (join -> applicationOvsMDyn) <-
    deploymentSection "App configuration" >=> holdDyn (pure Nothing) . fmap sequenceA $
      networkView $
        defAppEvM <&> \case
          Nothing -> loadingOverrides $> Nothing
          Just defApp -> Just . fst <$> holdDCfg appKeysDyn defApp defAppOv

  pure $ do
    depCfg <- deploymentOvsDyn
    appOvsM <- applicationOvsMDyn
    pure $ do
      appOvs <- appOvsM
      Just $
        DeploymentUpdate
          { appOverrides = appOvs
          , deploymentOverrides = depCfg
          }

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

constructWorkingOverridesEv ::
  MonadWidget t m => Event t (DefaultConfig l) -> Dynamic t (Overrides l) -> m (Event t WorkingOverrides)
constructWorkingOverridesEv defCfgEv ovsDyn = do
  defCfgMDyn <- holdDynMaybe defCfgEv
  pure $
    catMaybes . updated $ do
      defCfgM <- defCfgMDyn
      ovs <- ovsDyn
      pure $ do
        defCfg <- defCfgM
        pure $ constructWorkingOverrides defCfg ovs

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
      tellEvent $ fmap (PatchMap . M.singleton k . fmap catchEmptyErrors . reqErrorBody) reqEv
      pure $ fmapMaybeCheap reqSuccess reqEv
  pure x
  where
    catchEmptyErrors t
      | (T.null . T.strip) t =
        "Something went wrong, but no explanation was provided."
    catchEmptyErrors t = t

-- | The widget used to display errors.
errorHeader ::
  MonadWidget t m =>
  -- | Message text.
  Dynamic t Text ->
  m ()
errorHeader appErr =
  elClass "pre" "deployment__output notification notification--danger" $
    dynText appErr

-- | Widget with override fields. This widget supports adding and
-- removing key-value pairs.
envVarsInput ::
  forall t m.
  MonadWidget t m =>
  Dynamic t [Text] ->
  (Text -> Maybe Text) ->
  -- | Initial deployment overrides.
  WorkingOverrides ->
  -- | Updated deployment overrides. [(<full deconstructed key>, <value>)]
  m (Dynamic t [Dynamic t WorkingOverride])
envVarsInput (traceDyn "values" -> values) lookupOverride ovs = divClass "padded" $ mdo
  let initialConfig = CT.markValues ovs
  clickEv <-
    dashButton
      DashButtonConfig
        { buttonText = "Add an override"
        , buttonEnabled = addingIsEnabled
        , buttonType = Just AddDashButtonType
        , buttonStyle = OverridesDashButtonStyle
        }
  let emptyItem = ("", CustomConfigValue $ Left $ CustomKey "")
  addedItemsCount <- foldDyn (const (fst . insertUniqStart ())) emptyUniqKeyMap clickEv
  addedOvsListDyn <- (fmap . fmap) (fmap snd . M.toList)
    <$> list (uniqMap <$> addedItemsCount)
    $ \_ ->
      envVarInput lookupOverride values emptyItem
  treeResCfg <-
    showOverrideTree
      ((== Just True) . fmap (T.null . T.strip) . getConfigValueText . snd)
      (isn't (_Ctor' @"DefaultConfigValue") . snd)
      (\_ v -> v)
      (\a _ _ -> envVarInput lookupOverride values (trace "a" a))
      initialConfig
  let resCfg :: Dynamic t [Dynamic t (Text, ConfigValue Text)]
      resCfg = (<> treeResCfg) <$> addedOvsListDyn
      addingIsEnabled = join $ fmap and . sequenceA . (fmap . fmap) (not . T.null . fst) <$> resCfg
  pure resCfg


-- | Widget for entering a key-value pair.
envVarInput ::
  (MonadWidget t m) =>
  (Text -> Maybe Text) ->
  -- | The key values to suggest to the user
  Dynamic t [Text] ->
  -- | Current variable key and value.
  WorkingOverride ->
  m (Dynamic t WorkingOverride)
envVarInput f keys val = mdo
  value' <- holdDyn val (leftmost [buttonValue, updated inputValue])
  (keyTextDyn, valTextDyn, buttonPressEv) <- configField keys value'
  let inputValue = do
        v <- valTextDyn
        k <- keyTextDyn
        pure $
          ( k
          , case f k of
              Nothing -> CustomConfigValue $ Left $ CustomKey v
              Just x | x == v -> DefaultConfigValue v
              Just _ -> CustomConfigValue $ Right $ CustomValue v
          )
      buttonValue = flip push buttonPressEv $ \x ->
        Just <$> do
          k <- sample . current $ keyTextDyn
          (k,) <$> case x of
            ConfigFieldButtonClear -> do
              v <- case f k of
                Nothing -> sample . current $ valTextDyn
                Just v -> pure v
              pure $ CustomConfigValue $ Right $ DeletedValue $ Just v
            ConfigFieldButtonRecover -> do
              case f k of
                Nothing -> do
                  -- the value is not in the default config
                  v <- sample . current $ valTextDyn
                  pure $ CustomConfigValue $ Left $ CustomKey v
                Just v -> do
                  -- the value is in the default config
                  pure $ DefaultConfigValue v
  pure $ value'

holdDynMaybe :: (Reflex t, MonadHold t m) => Event t a -> m (Dynamic t (Maybe a))
holdDynMaybe ev = holdDyn Nothing $ fmapCheap Just ev

immediateNothing :: (PostBuild t m) => Event t a -> m (Event t (Maybe a))
immediateNothing ev = do
  pb <- getPostBuild
  pure $ leftmost [pb $> Nothing, fmapCheap Just ev]

holdClearingWith :: (Reflex t, MonadHold t m) => Event t a -> Event t () -> m (Dynamic t (Maybe a))
holdClearingWith aEv clear =
  holdDyn Nothing $ leftmost [fmapCheap Just aEv, fmapCheap (const Nothing) clear]

unitEv :: Reflex t => Dynamic t a -> Event t ()
unitEv = fmapCheap (const ()) . updated

(<&&>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
x <&&> f = (fmap . fmap) f x

debounceDyn ::
  ( PerformEvent t m
  , TriggerEvent t m
  , MonadIO (Performable m)
  , MonadHold t m
  , MonadFix m
  ) =>
  NominalDiffTime ->
  Dynamic t a ->
  m (Dynamic t a)
debounceDyn t d = do
  currD <- sample . current $ d
  ev <- debounce t (updated d)
  holdDyn currD ev

catchReturns :: (DomBuilder t m, MonadFix m) => (Event t () -> m a) -> m a
catchReturns f = mdo
  (divEl, a) <- el' "div" $ f $ ($> ()) . ffilter (== 13) $ domEvent Keypress divEl
  pure a
