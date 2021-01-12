{-|
Module      : Page.Deployments
Description : Deployments table page.

This module contains the definition of the deployments table page.
-}

module Page.Deployments
  ( deploymentsPage ) where

import           Control.Lens
import           Control.Lens.Extras
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Coerce
import           Data.Generics.Product (field)
import           Data.Generics.Sum (_Ctor)
import           Data.List as L (filter, null, partition, sortBy)
import           Data.Text as T (Text, isPrefixOf, toCaseFold)
import           Data.Time (diffUTCTime, getCurrentTime)
import           GHC.Generics (Generic)
import           Obelisk.Route.Frontend
import           Reflex.Dom
import           Servant.Reflex

import           Common.Types as CT
import           Common.Utils
import           Control.Monad.Reader
import           Data.Functor
import qualified Data.Semigroup as S
import           Frontend.API
import           Frontend.Route
import           Frontend.Utils
import           Page.ClassicPopup
import           Page.Elements.Links
import           Page.Popup.EditDeployment
import           Page.Popup.NewDeployment


-- | The root widget of the deployments list page.
-- It requests data of all deployments.
-- If a request fails it shows an error, otherwise it calls 'deploymentsWidget',
-- passing a received data.
deploymentsPage
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t () -- ^ Event notifying about the need to update data.
  -> m ()
deploymentsPage updAllEv = do
  pb <- getPostBuild
  respEv <- listEndpoint pb
  let (okEv, errEv) = processResp respEv
  widgetHold_ loadingDeploymentsWidget $ leftmost
    [ deploymentsWidget updAllEv <$> okEv
    , errDeploymentsWidget <$ errEv ]

-- | Widget that's shown after initial request succeeds.
-- It contains the header with an active search field,
-- deployments list and sidebars: \"new deployment\" and \"edit deployment\".
deploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()           -- ^ Event notifying about the need to update data.
  -> [DeploymentFullInfo] -- ^ Initial deployment data.
  -> m ()
deploymentsWidget updAllEv dfis = do
  (showNewDeploymentEv, editEv) <- deploymentsWidgetWrapper $ mdo
    pageNotification $ leftmost
      [ DPMError "Deployment list update failed, deployment list\
        \ may be slightly outdated." <$ errUpdEv
      , DPMClear <$ okUpdEv ]
    (showNewDeploymentEv', termDyn) <- deploymentsHeadWidget True okUpdEv
    (okUpdEv, errUpdEv, editEv) <- deploymentsListWidget updAllEv termDyn dfis
    pure (showNewDeploymentEv', editEv)
  void $ newDeploymentPopup showNewDeploymentEv never
  void $ editDeploymentPopup editEv never

-- | Div wrappers.
deploymentsWidgetWrapper :: MonadWidget t m => m a -> m a
deploymentsWidgetWrapper m =
  divClass "page" $
    divClass "page__wrap container" m

-- | Header of the current page. It contains a timer that shows
-- how much time has passed since the last update,
-- a search field and z button that opens \"New deployment\" sidebar.
deploymentsHeadWidget
  :: MonadWidget t m
  => Bool
  -- ^ Flag showing current state of search field.
  -> Event t ()
  -- ^ Event that fires after successful list update.
  -> m (Event t (), Dynamic t Text)
  -- ^ Returns an event notifying that the \"new deployment\" sidebar
  -- should be open and the search term within it.
deploymentsHeadWidget enabledSearch okUpdEv =
  divClass "page__head" $ do
    elClass "h1" "page__heading title" $ text "All deployments"
    lastUpdateWidget okUpdEv
    termDyn <- divClass "page__action input input--search input--has-clear-type\
    \ page__action--search" $ mdo
      let
        enabledSearchAttr = if enabledSearch then mempty else "disabled" =: ""
      termDyn' <- inputElement $ def
        & initialAttributes .~
          (  "type" =: "text"
          <> "class" =: "input__widget"
          <> "placeholder" =: "Search for deployments"
          <> enabledSearchAttr)
        & inputElementConfig_setValue .~ ("" <$ domEvent Click deleteEl)
      (deleteEl, _) <- elClass' "button" "input__clear-type spot spot--cancel" $
        text "Delete"
      pure termDyn'
    (nsEl, _) <- elClass' "button"
      "page__action button button--add popup-handler" $ text "New deployment"
    pure $ (domEvent Click nsEl, value termDyn)

-- ^ Widget that shows how much time has passed since the last update.
lastUpdateWidget :: MonadWidget t m => Event t () -> m ()
lastUpdateWidget okUpdEv = do
  updTimeEv <- performEvent $ liftIO getCurrentTime <$ okUpdEv
  initTime <- liftIO getCurrentTime
  updTimeB <- hold initTime updTimeEv
  tickEv <- tickLossyFromPostBuildTime 1
  let
    diffEv = attachWith calcMins updTimeB tickEv
    calcMins lastUpd TickInfo{..} = let
      diffSec = diffUTCTime _tickInfo_lastUTC lastUpd
      diffMin = diffSec / 60
      in floor diffMin :: Int
    mkMsg ms
      | ms < 1 = "Updated just now"
      | isSingular ms = "Updated " <> (showT $ ms) <> " minute ago"
      | ms < 60 = "Updated " <> (showT $ ms) <> " minutes ago"
      | isSingular (ms `div` 60) =
        "Updated " <> (showT $ ms `div` 60) <> " hour ago"
      | otherwise = "Updated " <> (showT $ ms `div` 60) <> " hours ago"
    isSingular x = x `mod` 100 /= 11 && x `mod` 10 == 1
  diffDyn <- holdDyn 0 diffEv
  divClass "page__note" $ dynText $ mkMsg <$> diffDyn

-- | Widget with all available deployments. It updates
-- the deployment information every time when the supplied event fires.
-- If an update fails, a notification widget appears at the top of the page.
deploymentsListWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()
  -> Dynamic t Text
  -> [DeploymentFullInfo] -- ^ Initial deployment data
  -> m (Event t (), Event t (), Event t DeploymentFullInfo)
deploymentsListWidget updAllEv termDyn ds = dataWidgetWrapper $ mdo
  retryEv <- delay 10 errUpdEv
  updRespEv <- listEndpoint $ leftmost [updAllEv, () <$ retryEv]
  let
    okUpdEv = fmapMaybe reqSuccess updRespEv
    errUpdEv = fmapMaybe reqFailure updRespEv
  dsDyn <- holdDyn ds okUpdEv
  let
    isArchived = view (field @"archived")
    filteredDyn = ffor2 termDyn dsDyn $ \term ds' ->
      L.filter (searchDeployments term) ds'
    (archivedDsDyn, activeDsDyn) = splitDynPure $ L.partition isArchived
      <$> filteredDyn
  clickedEv <- elementClick
  editEv <- activeDeploymentsWidget clickedEv activeDsDyn
  archivedDeploymentsWidget clickedEv archivedDsDyn
  pure (() <$ okUpdEv, () <$ errUpdEv, editEv)

-- | Compares deployment fields (name and tag) with a search term.
searchDeployments
  :: Text               -- ^ Search term.
  -> DeploymentFullInfo -- ^ Deployment data.
  -> Bool
searchDeployments "" _ = True
searchDeployments term d = term' `isPrefixOf` dname
  || term' `isPrefixOf` dtag
  where
    term' = toCaseFold term
    dtag = d ^. field @"deployment" . field @"tag" . coerced . to toCaseFold
    dname = d ^. dfiName . coerced . to toCaseFold

-- | Table with active deployments.
activeDeploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -- ^ Event that carries the clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t [DeploymentFullInfo]
  -> m (Event t DeploymentFullInfo)
  -- ^ Returns an event carrying editable deployment
  -- to \"edit deployment\" sidebar.
activeDeploymentsWidget clickedEv dsDyn =
  divClass "data__primary" $
    tableWrapper $ \sortDyn -> do
      let
        emptyDyn' = L.null <$> dsDyn
        dsSortedDyn = zipDynWith sortDeployments dsDyn sortDyn
      emptyDyn <- holdUniqDyn emptyDyn'
      editEvEv <- dyn $ emptyDyn <&> \case
        False -> do
          editEvs <- simpleList dsSortedDyn (activeDeploymentWidget clickedEv)
          pure $ switchDyn $ leftmost <$> editEvs
        True -> do
          emptyTableBody $ noDeploymentsWidget
          pure never
      switchHold never editEvEv

-- | This type helps determine which item was selected
-- in the table row dropdown.
data DeploymentAction
  = ArchiveDeployment
  | EditDeployment
  deriving (Show, Eq, Generic)

-- | Row of active deployment.
activeDeploymentWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -- ^ Event that carries the clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t (DeploymentFullInfo)
  -> m (Event t DeploymentFullInfo)
  -- ^ Returns event carrying editable deployment
  -- that is required by \"edit deployment\" sidebar.
activeDeploymentWidget clickedEv dDyn' = do
  dDyn <- holdUniqDyn dDyn'
  editEvEv <- dyn $ ffor dDyn $ \d@DeploymentFullInfo{..} -> do
    let dname = deployment ^. field @"name"
    (linkEl, dropdownEv) <- el' "tr" $ do
      el "td" $ do
        text $ coerce dname
        statusWidget $ constDyn status
      el "td" $ divClass "listing" $ forM_ metadata (renderMetadataLink . pure)
      el "td" $
        text $ coerce $ deployment ^. field @"tag"
      el "td" $
        overridesWidget $ deployment ^. field @"appOverrides" . coerced
      el "td" $
        overridesWidget $ deployment ^. field @"deploymentOverrides" . coerced
      el "td" $
        text $ formatPosixToDate createdAt
      el "td" $
        text $ formatPosixToDate updatedAt
      el "td" $ do
        let
          disabledAttr = if isPending status
            then "disabled" =: ""
            else mempty
          elId = "deployment_row_" <> (coerce dname)
          btn = elAttr "button"
            (  "class" =: "drop__handler"
            <> "type" =: "button"
            <> "id" =: elId
            <> disabledAttr) $ text "Actions"
          body = do
            btnEditEv <- buttonClass "action action--edit" "Edit"
            btnArcEv <- buttonClass "action action--archive" "Move to archive"
            pure $
              leftmost
                [ ArchiveDeployment <$ btnArcEv, EditDeployment <$ btnEditEv ]
        dropdownWidget' clickedEv btn body
    let
      archEv = () <$ ffilter (is (_Ctor @"ArchiveDeployment")) dropdownEv
      editEv = d <$ ffilter (is (_Ctor @"EditDeployment")) dropdownEv
    delEv <- confirmArchivePopup archEv $ do
      text "Are you sure you want to archive the"
      el "br" blank
      text $ coerce dname <> " deployment?"
    void $ archiveEndpoint (constDyn $ Right $ dname) delEv
    let route = DashboardRoute :/ Just dname
    setRoute $ route <$ domEvent Dblclick linkEl
    pure editEv
  switchHold never editEvEv

-- | Table with archived deployments.
archivedDeploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -- ^ Event that carries the clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t [DeploymentFullInfo]
  -> m ()
archivedDeploymentsWidget clickedEv dsDyn = do
  showDyn <- toggleButton
  let
    classDyn = ffor showDyn $ \case
      True -> "data__archive data__archive--open"
      False -> "data__archive"
  elDynClass "div" classDyn $
    tableWrapper $ \sortDyn -> do
      let
        emptyDyn' = L.null <$> dsDyn
        dsSortedDyn = zipDynWith sortDeployments dsDyn sortDyn
      emptyDyn <- holdUniqDyn emptyDyn'
      dyn_ $ emptyDyn <&> \case
        False -> void $ simpleList dsSortedDyn
          (archivedDeploymentWidget clickedEv)
        True -> emptyTableBody $ noDeploymentsWidget

-- | Row with archived deployment.
archivedDeploymentWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -> Dynamic t DeploymentFullInfo
  -> m ()
archivedDeploymentWidget clickedEv dDyn' = do
  dDyn <- holdUniqDyn dDyn'
  dyn_ $ ffor dDyn $ \DeploymentFullInfo{..} -> do
    let dname = deployment ^. field @"name"
    (linkEl, _) <- el' "tr" $ do
      el "td" $ do
        text $ coerce dname
        divClass "status status--archived" $ text "Archived"
      el "td" $ text "..."
      el "td" $
        text $ coerce $ deployment ^. field @"tag"
      el "td" $
        overridesWidget $ deployment ^. field @"appOverrides" . coerced
      el "td" $
        overridesWidget $ deployment ^. field @"deploymentOverrides" . coerced
      el "td" $
        text $ formatPosixToDate createdAt
      el "td" $
        text $ formatPosixToDate updatedAt
      el "td" $ do
        let
          elId = "deployment_row_" <> (coerce dname)
          btn = elAttr "button"
            (  "class" =: "drop__handler"
            <> "type" =: "button"
            <> "id" =: elId) $ text "Actions"
          body = do
            btnArcEv <- buttonClass
              "action action--archive" "Restore from archive"
            pure btnArcEv
        btnEv <- dropdownWidget' clickedEv btn body
        void $ restoreEndpoint (constDyn $ Right $ dname) btnEv
    let route = DashboardRoute :/ Just dname
    setRoute $ route <$ domEvent Dblclick linkEl

-- | Sort deployments by the supplied condition.
sortDeployments
  :: [DeploymentFullInfo]
  -> Maybe SortDir
  -- ^ Sorting condition.
  -> [DeploymentFullInfo]
sortDeployments items s =  L.sortBy sortFunc items
  where
    sortFunc a b = case s of
      Just (SortAsc get) -> compare (a ^. get) (b ^. get)
      Just (SortDesc get) -> compare (b ^. get) (a ^. get)
      Nothing -> EQ

-- | Each constructor contains a getter
-- that extracts the field that is used for sorting.
data SortDir where
  SortAsc  :: Ord a => Getter DeploymentFullInfo a -> SortDir
  SortDesc :: Ord a => Getter DeploymentFullInfo a -> SortDir
  deriving Semigroup via (S.Last SortDir)

-- | Sorting toggler.
toggleSort :: SortDir -> SortDir
toggleSort = \case
  SortAsc x -> SortDesc x
  SortDesc x -> SortAsc x

-- | Header for a deployments table.
-- \"Name\",\"created\" and \"udpated\" columns support sorting.
tableHeader :: (MonadWidget t m, SortableTableGroup t m) => m ()
tableHeader = do
  el "thead" $
    el "tr" $ do
      sortHeader dfiName "Name"
      el "th" $ text "Links"
      el "th" $ text "Tag"
      el "th" $ text "App overrides"
      el "th" $ text "Deployment overrides"
      sortHeader (field @"createdAt") "Created"
      sortHeader (field @"updatedAt") "Changed"
      el "th" $
        elClass "span" "visuallyhidden" $ text "Menu"

data SortingChanged = SortingChanged
  deriving Semigroup via (S.Last SortingChanged)

type SortableTableGroup t m =
  ( MonadReader (Event t SortingChanged) m
  , EventWriter t SortDir m
  )

runSortableTableGroup
  :: (Reflex t, MonadFix m)
  => ReaderT (Event t SortingChanged) (EventWriterT t SortDir m) x
  -> m (x, Event t SortDir)
runSortableTableGroup m = mdo
  (a, sDyn') <- runEventWriterT . flip runReaderT (sDyn' $> SortingChanged) $ m
  return (a, sDyn')

-- | Special column header with a sort button.
sortHeader
  :: (MonadWidget t m, Ord a, SortableTableGroup t m)
  => Getter DeploymentFullInfo a
  -> Text
  -> m ()
sortHeader f l = do
  el "th" $ mdo
    sortDyn' <- foldDyn ($) (SortDesc f) $ toggleSort <$ sortBtnEv
    tellEvent $ updated sortDyn'
    sortingChanged <- ask
    let sortEv = leftmost [Just <$> updated sortDyn', sortingChanged $> Nothing]
    sortDyn <- holdDyn Nothing sortEv
    let
      classDyn = fmap ("sort " <>) $ sortDyn <&> \case
        Just (SortDesc _) -> "sort--active sort--desc"
        Just (SortAsc _) -> "sort--active sort--asc"
        Nothing -> ""
    sortBtnEv <- buttonDynClass classDyn (pure l)
    pure ()

-- | A wrapper that adds a header to the table.
tableWrapper
  :: (MonadWidget t m)
  => (Dynamic t (Maybe SortDir) -> m a)
  -- ^ Sorting direction is obtained from the table header.
  -> m a
tableWrapper ma =
  divClass "table table--deployments table--clickable table--double-click" $
    el "table" $ mdo
      ((), sDyn') <- runSortableTableGroup tableHeader
      sDyn <- holdDyn Nothing $ Just <$> sDyn'
      el "tbody" $ ma sDyn

-- | Table wrapper for a table with an \"error\" or a \"loading\ placeholder.
initTableWrapper :: MonadWidget t m => m () -> m ()
initTableWrapper ma = do
  divClass "data_primary" $
    tableWrapper $ const $
      emptyTableBody $ ma

-- | Widget with a loading spinner.
loadingDeploymentsWidget :: MonadWidget t m => m ()
loadingDeploymentsWidget =
  deploymentsWidgetWrapper $ do
    void $ deploymentsHeadWidget False never
    dataWidgetWrapper $ initTableWrapper $ loadingCommonWidget

-- | Widget with an error message.
errDeploymentsWidget :: MonadWidget t m => m ()
errDeploymentsWidget = deploymentsWidgetWrapper $ dataWidgetWrapper $
  initTableWrapper $
    errorCommonWidget

-- | Widget for an empty table with a custom message.
noDeploymentsWidget'
  :: MonadWidget t m
  => m ()
  -- ^ Text widget
  -> m ()
noDeploymentsWidget' h =
  divClass "null null--search" $ do
    elClass "b" "null__heading" h
    divClass "null__message" blank

-- | Widget for an empty table.
noDeploymentsWidget :: MonadWidget t m => m ()
noDeploymentsWidget = noDeploymentsWidget' (text "No deployments")

-- | Table body wrapper.
emptyTableBody :: MonadWidget t m => m () -> m ()
emptyTableBody msg =
  elClass "tr" "no-table" $
    elAttr "td" ("colspan" =: "8") msg

-- | Div wrappers.
dataWidgetWrapper :: MonadWidget t m => m a -> m a
dataWidgetWrapper ma = divClass "page__body" $ divClass "data" ma

-- | Button that controls visibility of the archived deployments.
toggleButton :: MonadWidget t m => m (Dynamic t Bool)
toggleButton = mdo
  showDyn <- toggle False $ domEvent Click archivedBtnEl
  let
    btnClassDyn = ffor showDyn $ \case
      True -> "data__show-archive expander expander--stand-alone expander--open"
      False -> "data__show-archive expander expander--stand-alone"
    btnAttrsDyn = ffor btnClassDyn $ \btnClass ->
      (  "class" =: btnClass
      <> "type" =: "button" )
  (archivedBtnEl, _) <- elDynAttr' "button" btnAttrsDyn $
    text "Show Archived deployments"
  pure showDyn
