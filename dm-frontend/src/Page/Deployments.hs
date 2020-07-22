{-|
Module      : Page.Deployments
Description : Stagings table page.

This module contains definition of stagings table page.
-}

module Page.Deployments
  ( deploymentsPage ) where

import Control.Lens
import Control.Lens.Extras
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Generics.Product (field)
import Data.Generics.Sum (_Ctor)
import Data.List as L (null, sortBy, filter, partition)
import Data.Text as T (Text, toCaseFold, isPrefixOf)
import Data.Time (getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)
import Obelisk.Route.Frontend
import Reflex.Dom
import Servant.Reflex

import Common.Types as CT
import Common.Utils
import Frontend.API
import Frontend.Route
import Page.ClassicPopup
import Page.Popup.EditStaging
import Page.Popup.NewStaging
import Frontend.Utils


-- | The root widget of stagings list page. It requests data about all stagings.
-- If request failures it shows error page, else it calls 'deploymentsWidget',
-- passing a recieved data.
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

-- | Widget that's shown after successful initial request. It contains header
-- with active search field, stagings list and sidebars: @new staging@ and
-- @edit staging@.
deploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()           -- ^ Event notifying about the need to update data.
  -> [DeploymentFullInfo] -- ^ Initial staging data.
  -> m ()
deploymentsWidget updAllEv dfis = do
  (showNewStagingEv, editEv) <- deploymentsWidgetWrapper $ mdo
    pageNotification $ leftmost
      [ DPMError "Staging list update failed, staging list\
        \ may be slightly outdated." <$ errUpdEv
      , DPMClear <$ okUpdEv ]
    (showNewStagingEv', termDyn) <- deploymentsHeadWidget True okUpdEv
    (okUpdEv, errUpdEv, editEv) <- deploymentsListWidget updAllEv termDyn dfis
    pure (showNewStagingEv', editEv)
  void $ newStagingPopup showNewStagingEv never
  void $ editStagingPopup editEv never

-- | Div wrappers for page.
deploymentsWidgetWrapper :: MonadWidget t m => m a -> m a
deploymentsWidgetWrapper m =
  divClass "page" $
    divClass "page__wrap container" m

-- | Header of current page. It contains a timer that shows how much time has
-- passed since the last update, search field and button that opens
-- @New staging@ sidebar.
deploymentsHeadWidget
  :: MonadWidget t m
  => Bool
  -- ^ Flag showing current state of search field.
  -> Event t ()
  -- ^ Event that fires after successful list update.
  -> m (Event t (), Dynamic t Text)
  -- ^ Returns event notifying that @new staging@ sidebar should be open and
  -- search term.
deploymentsHeadWidget enabledSearch okUpdEv =
  divClass "page__head" $ do
    elClass "h1" "page__heading title" $ text "All stagings"
    lastUpdateWidget okUpdEv
    termDyn <- divClass "page__action input input--search input--has-clear-type\
    \ page__action--search" $ mdo
      let
        enabledSearchAttr = if enabledSearch then mempty else "disabled" =: ""
      termDyn' <- inputElement $ def
        & initialAttributes .~
          (  "type" =: "text"
          <> "class" =: "input__widget"
          <> "placeholder" =: "Search for stagings"
          <> enabledSearchAttr)
        & inputElementConfig_setValue .~ ("" <$ domEvent Click deleteEl)
      (deleteEl, _) <- elClass' "button" "input__clear-type spot spot--cancel" $
        text "Delete"
      pure termDyn'
    (nsEl, _) <- elClass' "a"
      "page__action button button--add popup-handler" $ text "New staging"
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

-- | Widget with all available stagings that takes initial data. It updates
-- this data every time when passed event fires. If update failures then
-- a notification widget with appears at the top of the page.
deploymentsListWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()
  -> Dynamic t Text
  -> [DeploymentFullInfo]
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

-- | Compare stagings fields (name and tag) with search term.
searchDeployments
  :: Text               -- ^ Search term.
  -> DeploymentFullInfo -- ^ Staging data.
  -> Bool
searchDeployments "" _   = True
searchDeployments term d = term' `isPrefixOf` dname
  || term' `isPrefixOf` dtag
  where
    term' = toCaseFold term
    dtag = d ^. field @"deployment" . field @"tag" . coerced . to toCaseFold
    dname = d ^. dfiName . coerced . to toCaseFold

-- | Table with active stagings.
activeDeploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -- ^ Event that carries clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t [DeploymentFullInfo]
  -- ^ 'Map' of deployments.
  -> m (Event t DeploymentFullInfo)
  -- ^ Returns event carrying editable staging to @edit staging@ sidebar.
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
        True  -> do
          emptyTableBody $ noDeploymentsWidget
          pure never
      switchHold never editEvEv

-- | This type helps to determine item selected in dropdown in table row.
data StagingAction
  = ArchiveStaging
  | EditStaging
  deriving (Show, Eq, Generic)

-- | Row of active deployment.
activeDeploymentWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -- ^ Event that carries clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t (DeploymentFullInfo)
  -> m (Event t DeploymentFullInfo)
  -- ^ Returns event carrying editable staging to @edit staging@ sidebar.
activeDeploymentWidget clickedEv dDyn' = do
  dDyn <- holdUniqDyn dDyn'
  editEvEv <- dyn $ ffor dDyn $ \d@DeploymentFullInfo{..} -> do
    let dname = deployment ^. field @"name"
    (linkEl, dropdownEv) <- el' "tr" $ do
      el "td" $ do
        text $ coerce dname
        statusWidget $ constDyn status
      el "td" $ do
        divClass "listing" $
          forM_ urls $ \(_, url) ->
            void $ elAttr' "a"
              (  "class" =: "listing__item external bar"
              <> "href" =: ("https://" <> url)
              <> "target" =: "_blank") $ text url
      el "td" $
        text $ coerce $ deployment ^. field @"tag"
      el "td" $
        overridesWidget $ deployment ^. field @"appOverrides" . coerced
      el "td" $
        overridesWidget $ deployment ^. field @"stagingOverrides" . coerced
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
            btnArcEv <- buttonClass "action action--delete" "Move to archive"
            pure $
              leftmost [ ArchiveStaging <$ btnArcEv, EditStaging <$ btnEditEv ]
        dropdownWidget' clickedEv btn body
    let
      archEv = () <$ ffilter (is (_Ctor @"ArchiveStaging")) dropdownEv
      editEv = d <$ ffilter (is (_Ctor @"EditStaging")) dropdownEv
    delEv <- confirmDeletePopup archEv $ do
      text "Are you sure  you want to delete"
      el "br" blank
      text $ coerce dname <> " staging?"
    void $ deleteEndpoint (constDyn $ Right $ dname) delEv
    let route = DashboardRoute :/ Just dname
    setRoute $ route <$ domEvent Dblclick linkEl
    pure editEv
  switchHold never editEvEv

-- | Table with archived stagings.
archivedDeploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -- ^ Event that carries clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t [DeploymentFullInfo]
  -> m ()
archivedDeploymentsWidget clickedEv dsDyn = do
  showDyn <- toggleButton
  let
    classDyn = ffor showDyn $ \case
      True  -> "data__archive data__archive--open"
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
        True  -> emptyTableBody $ noDeploymentsWidget

-- | Row with archived staging.
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
        overridesWidget $ deployment ^. field @"stagingOverrides" . coerced
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
              "action action--delete" "Restore from archive"
            pure btnArcEv
        btnEv <- dropdownWidget' clickedEv btn body
        void $ restoreEndpoint (constDyn $ Right $ dname) btnEv
    let route = DashboardRoute :/ Just dname
    setRoute $ route <$ domEvent Dblclick linkEl

-- | Sort stagings by passed condition.
sortDeployments
  :: [DeploymentFullInfo]
  -> SortDir
  -- ^ Sorting condition.
  -> [DeploymentFullInfo]
sortDeployments items s =  L.sortBy sortFunc items
  where
    sortFunc a b = case s of
      SortAsc get -> compare (a ^. get) (b ^. get)
      SortDesc get -> compare (b ^. get) (a ^. get)

-- | Each constructor contains getter that extracts field by which the sorting
-- is performed.
data SortDir where
  SortAsc  :: Ord a => Getter DeploymentFullInfo a -> SortDir
  SortDesc :: Ord a => Getter DeploymentFullInfo a -> SortDir

-- | Sorting toggler.
toggleSort :: SortDir -> SortDir
toggleSort = \case
  SortAsc x  -> SortDesc x
  SortDesc x -> SortAsc x

-- | Header for table with active and table with archived stagings. @Name@,
-- @created@ and @udpated@ columns support sorting.
tableHeader :: MonadWidget t m => m (Dynamic t SortDir)
tableHeader = do
  el "thead" $
    el "tr" $ do
      nameSortDyn <- sortHeader dfiName "Name"
      el "th" $ text "Links"
      el "th" $ text "Tag"
      el "th" $ text "App overrides"
      el "th" $ text "Staging overrides"
      createSortDyn <- sortHeader (field @"createdAt") "Created"
      updateSortDyn <- sortHeader (field @"updatedAt") "Changed"
      el "th" $
        elClass "span" "visuallyhidden" $ text "Menu"
      let sortEv = leftmost $ fmap updated [nameSortDyn, createSortDyn, updateSortDyn]
      sortDyn <- holdDyn (SortAsc dfiName) sortEv
      pure sortDyn

-- | Special column header with sort button.
sortHeader
  :: (MonadWidget t m, Ord a)
  => Getter DeploymentFullInfo a
  -> Text
  -> m (Dynamic t SortDir)
sortHeader f l = do
  el "th" $ mdo
    dateSortDyn' <- foldDyn ($) (SortDesc f)
      $ toggleSort <$ sortBtnEv
    let
      classDyn = dateSortDyn' <&> \case
        SortDesc _ -> "sort sort--active sort--desc"
        SortAsc _  -> "sort sort--active sort--asc"
    sortBtnEv <- buttonDynClass classDyn (pure l)
    pure dateSortDyn'

-- | Wrapper with table header for table body.
tableWrapper
  :: MonadWidget t m
  => (Dynamic t SortDir -> m a)
  -- ^ Sorting direction is obtained from table header.
  -> m a
tableWrapper ma =
  divClass "table table--stagings table--clickable table--double-click" $
    el "table" $ do
      sDyn <- tableHeader
      el "tbody" $ ma sDyn

-- | Table wrapper for table with error or loading placeholders.
initTableWrapper :: MonadWidget t m => m () -> m ()
initTableWrapper ma = do
  divClass "data_primary" $
    tableWrapper $ const $
      emptyTableBody $ ma

-- | Page with loading placeholder.
loadingDeploymentsWidget :: MonadWidget t m => m ()
loadingDeploymentsWidget =
  deploymentsWidgetWrapper $ do
    void $ deploymentsHeadWidget False never
    dataWidgetWrapper $ initTableWrapper $ loadingCommonWidget

-- | Page with error placeholder.
errDeploymentsWidget :: MonadWidget t m => m ()
errDeploymentsWidget = deploymentsWidgetWrapper $ dataWidgetWrapper $
  initTableWrapper $
    errorCommonWidget

-- | Widget for empty table with custom message.
noDeploymentsWidget'
  :: MonadWidget t m
  => m ()
  -- ^ Text widget
  -> m ()
noDeploymentsWidget' h =
  divClass "null null--search" $ do
    elClass "b" "null__heading" h
    divClass "null__message" blank

-- | Widget for empty table.
noDeploymentsWidget :: MonadWidget t m => m ()
noDeploymentsWidget = noDeploymentsWidget' (text "No stagings")

-- | Table body wrapper.
emptyTableBody :: MonadWidget t m => m () -> m ()
emptyTableBody msg =
  elClass "tr" "no-table" $
    elAttr "td" ("colspan" =: "8") msg

-- | Div wrappers.
dataWidgetWrapper :: MonadWidget t m => m a -> m a
dataWidgetWrapper ma = divClass "page__body" $ divClass "data" ma

-- | Button that controls visibility of archived stagings.
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
    text "Show Archived stagings"
  pure showDyn
