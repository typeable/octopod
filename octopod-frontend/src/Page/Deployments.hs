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
import           Data.Generics.Product (field)
import           Data.Generics.Sum (_Ctor)
import qualified Data.List as L
import           Data.Text (Text)
import           Data.Time (diffUTCTime, getCurrentTime)
import           GHC.Generics (Generic)
import           Obelisk.Route.Frontend
import           Reflex.Dom
import           Servant.Reflex

import           Common.Types as CT
import           Common.Utils
import           Control.Applicative
import           Control.Monad.Reader
import           Data.Align
import           Data.Char
import           Data.Functor
import qualified Data.Semigroup as S
import qualified Data.Text as T
import           Data.Text.Search
import           Data.These
import           Frontend.API
import           Frontend.GHCJS
import           Frontend.Route
import           Frontend.Utils
import           Page.ClassicPopup
import           Page.Elements.Links
import           Page.Popup.EditDeployment
import           Page.Popup.NewDeployment
import           Reflex.MultiEventWriter.Class
import           Servant.Reflex.Extra


-- | The root widget of the deployments list page.
-- It requests data of all deployments.
-- If a request fails it shows an error, otherwise it calls 'deploymentsWidget',
-- passing a received data.
deploymentsPage
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , MonadReader ProjectConfig m
    )
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
    , SetRoute t (R Routes) m
    , MonadReader ProjectConfig m
    )
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
          <> "style" =: "width: 264px;"
          <> enabledSearchAttr)
        & inputElementConfig_setValue .~ ("" <$ domEvent Click deleteEl)
      (deleteEl, _) <- elClass' "button" "input__clear-type spot spot--cancel" $
        text "Delete"
      pure termDyn'
    (nsEl, _) <- elClass' "button"
      "page__action button button--add popup-handler" $ text "New deployment"
    pure (domEvent Click nsEl, value termDyn)

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
    , SetRoute t (R Routes) m
    , MonadReader ProjectConfig m
    )
  => Event t ()
  -> Dynamic t Text
  -> [DeploymentFullInfo] -- ^ Initial deployment data
  -> m (Event t (), Event t (), Event t DeploymentFullInfo)
deploymentsListWidget updAllEv termDyn ds = dataWidgetWrapper $ mdo
  retryEv <- delay 10 errUpdEv
  updRespEv <- listEndpoint $ leftmost [updAllEv, () <$ retryEv]
  let
    okUpdEv = fmapMaybe reqSuccess updRespEv
    errUpdEv = fmapMaybe reqErrorBody updRespEv
  dsDyn <- holdDyn ds okUpdEv
  let
    isArchived = isDeploymentArchived . view #deployment
    filteredDyn = ffor2 termDyn dsDyn $ \term ds' ->
      mapMaybe (searchDeployments . T.filter (not . isSpace) $ term) ds'
    (archivedDsDyn, activeDsDyn) = splitDynPure $ L.partition isArchived
      <$> filteredDyn
    searchSorting = termDyn $> Just (SortDesc (view #score))
  clickedEv <- elementClick
  editEv <- activeDeploymentsWidget searchSorting clickedEv activeDsDyn
  archivedDeploymentsWidget searchSorting clickedEv archivedDsDyn
  pure (() <$ okUpdEv, () <$ errUpdEv, editEv)

data SearchedDeploymentFullInfo = SearchedDeploymentFullInfo
  { deployment :: DeploymentFullInfo
  , searchResult :: DeploymentSearchResult
  , score :: Int
  } deriving stock (Generic, Show, Eq)

-- | Compares deployment fields (name and tag) with a search term.
searchDeployments
  :: Text               -- ^ Search term.
  -> DeploymentFullInfo -- ^ Deployment data.
  -> Maybe SearchedDeploymentFullInfo
searchDeployments term d = case (tagResult, nameResult) of
  (Nothing, Nothing) -> Nothing
  (Just (res, score), Nothing) ->
    Just $ SearchedDeploymentFullInfo d (DeploymentTagResult res) score
  (Nothing, Just (res, score)) ->
    Just $ SearchedDeploymentFullInfo d (DeploymentNameResult res) score
  (Just (tRes, tScore), Just (nRes, nScore)) -> if tScore > nScore
    then Just $ SearchedDeploymentFullInfo d (DeploymentTagResult tRes) tScore
    else Just $ SearchedDeploymentFullInfo d (DeploymentNameResult nRes) nScore
  where
    tagResult = fuzzySearch term . unDeploymentTag $ d ^. field @"deployment" . field @"tag"
    nameResult = fuzzySearch term . unDeploymentName $ d ^. dfiName

data DeploymentSearchResult
  = DeploymentNameResult [FuzzySearchStringChunk Text]
  | DeploymentTagResult [FuzzySearchStringChunk Text]
  deriving (Show, Eq)

-- | Table with active deployments.
activeDeploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , MonadReader ProjectConfig m
    )
  => Dynamic t (Maybe (SortDir SearchedDeploymentFullInfo))
  -> Event t ClickedElement
  -- ^ Event that carries the clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t [SearchedDeploymentFullInfo]
  -> m (Event t DeploymentFullInfo)
  -- ^ Returns an event carrying editable deployment
  -- to \"edit deployment\" sidebar.
activeDeploymentsWidget searchSorting clickedEv dsDyn =
  divClass "data__primary" $
    tableWrapper (updated searchSorting $> SortingChanged) $ \sortDyn -> do
      let colSortDyn = fmap (contramap (view #deployment)) <$> sortDyn
      sorting <- holdDyn Nothing (mergeWith (<|>) [updated colSortDyn, updated searchSorting])
      let
        emptyDyn' = L.null <$> dsDyn
        dsSortedDyn = zipDynWith sortDeployments dsDyn sorting
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
    , SetRoute t (R Routes) m
    , MonadReader ProjectConfig m
    )
  => Event t ClickedElement
  -- ^ Event that carries the clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t SearchedDeploymentFullInfo
  -> m (Event t DeploymentFullInfo)
  -- ^ Returns event carrying editable deployment
  -- that is required by \"edit deployment\" sidebar.
activeDeploymentWidget clickedEv dDyn' = do
  dDyn <- holdUniqDyn dDyn'
  editEvEv <- dyn $ ffor dDyn $ \s@SearchedDeploymentFullInfo {deployment = d@DeploymentFullInfo{..}} -> do
    let
      (name, tag')  = displaySearchHighlighting s
      dName = deployment ^. #name
    (linkEl, dropdownEv) <- el' "tr" $ do
      el "td" $ do
        name
        statusWidget $ constDyn status
      el "td" $ divClass "listing" $ forM_ metadata (renderMetadataLink . pure)
      el "td" tag'
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
          enabled = not . isPending . recordedStatus $ status
          elId = "deployment_row_" <> unDeploymentName dName
          btn = elAttr "button"
            (  "class" =: "drop__handler"
            <> "type" =: "button"
            <> "id" =: elId ) $ text "Actions"
          body = do
            btnEditEv <- buttonClassEnabled' "action action--edit" "Edit" (pure enabled) "action--disabled"
            btnArcEv <- buttonClassEnabled' "action action--archive" "Move to archive" (pure enabled) "action--disabled"
            url' <- kubeDashboardUrl (view #deployment <$> dDyn)
            void . dyn $ url' <&> maybe blank (\url ->
              void $ aButtonClass' "action action--logs" "Details"
                (pure $ "href" =: url <> "target" =: "_blank")
              )
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
      text $ unDeploymentName dName <> " deployment?"
    void $ archiveEndpoint (constDyn . Right $ dName) delEv
    let route = DashboardRoute :/ Just dName
    setRoute $ route <$ domEvent Dblclick linkEl
    pure editEv
  switchHold never editEvEv

displaySearchHighlighting :: DomBuilder t m => SearchedDeploymentFullInfo -> (m (), m ())
displaySearchHighlighting
  SearchedDeploymentFullInfo {searchResult = DeploymentNameResult res, deployment = d} =
    ( markMatches res
    , text . unDeploymentTag $ d ^. #deployment . #tag
    )
displaySearchHighlighting
  SearchedDeploymentFullInfo {searchResult = DeploymentTagResult res, deployment = d} =
    ( text . unDeploymentName $ d ^. #deployment . #name
    , markMatches res
    )

markMatches :: DomBuilder t m => [FuzzySearchStringChunk Text] -> m ()
markMatches [] = blank
markMatches (NotMatched x : xs) = text x >> markMatches xs
markMatches (Matched x : xs) =
  elAttr "span" ("style" =: "text-decoration: underline;") (text x) >> markMatches xs

-- | Table with archived deployments.
archivedDeploymentsWidget
  :: forall t m.
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Dynamic t (Maybe (SortDir SearchedDeploymentFullInfo))
  -> Event t ClickedElement
  -- ^ Event that carries the clicked DOM element. This event is required by
  -- `dropdownWidget'`.
  -> Dynamic t [SearchedDeploymentFullInfo]
  -> m ()
archivedDeploymentsWidget searchSorting clickedEv dsDyn = do
  showDyn <- toggleButton
  let
    classDyn = ffor showDyn $ \case
      True -> "data__archive data__archive--open"
      False -> "data__archive"
  elDynClass "div" classDyn $
    tableWrapper (updated searchSorting $> SortingChanged) $ \sortDyn -> do
      let colSortDyn = fmap (contramap (view #deployment)) <$> sortDyn
      sorting <- holdDyn Nothing (mergeWith (<|>) [updated colSortDyn, updated searchSorting])
      let
        emptyDyn' = L.null <$> dsDyn
        dsSortedDyn = zipDynWith sortDeployments dsDyn sorting
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
  -> Dynamic t SearchedDeploymentFullInfo
  -> m ()
archivedDeploymentWidget clickedEv dDyn' = do
  dDyn <- holdUniqDyn dDyn'
  dyn_ $ ffor dDyn $ \s@SearchedDeploymentFullInfo {deployment = DeploymentFullInfo{..}} -> do
    let
      (name, tag') = displaySearchHighlighting s
      dName = deployment ^. #name
    (linkEl, _) <- el' "tr" $ do
      el "td" $ do
        name
        divClass "status status--archived" $ text "Archived"
      el "td" $ text "..."
      el "td" tag'
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
          elId = "deployment_row_" <> (unDeploymentName dName)
          btn = elAttr "button"
            (  "class" =: "drop__handler"
            <> "type" =: "button"
            <> "id" =: elId) $ text "Actions"
          body = do
            btnArcEv <- buttonClass
              "action action--archive" "Restore from archive"
            pure btnArcEv
        btnEv <- dropdownWidget' clickedEv btn body
        void $ restoreEndpoint (constDyn $ Right $ dName) btnEv
    let route = DashboardRoute :/ Just dName
    setRoute $ route <$ domEvent Dblclick linkEl

-- | Sort deployments by the supplied condition.
sortDeployments
  :: [SearchedDeploymentFullInfo]
  -> Maybe (SortDir SearchedDeploymentFullInfo)
  -- ^ Sorting condition.
  -> [SearchedDeploymentFullInfo]
sortDeployments items s =  L.sortBy sortFunc items
  where
    sortFunc a b = case s of
      Just (SortAsc get) -> compare (get a) (get b)
      Just (SortDesc get) -> compare (get b) (get a)
      Nothing -> EQ

-- | Each constructor contains a getter
-- that extracts the field that is used for sorting.
data SortDir x where
  SortAsc  :: Ord a => (x -> a) -> SortDir x
  SortDesc :: Ord a => (x -> a) -> SortDir x
  deriving Semigroup via (S.Last (SortDir x))

instance Contravariant SortDir where
  contramap f (SortAsc g) = SortAsc $ g . f
  contramap f (SortDesc g) = SortDesc $ g . f

-- | Sorting toggler.
toggleSort :: SortDir a -> SortDir a
toggleSort = \case
  SortAsc x -> SortDesc x
  SortDesc x -> SortAsc x

-- | Header for a deployments table.
-- \"Name\",\"created\" and \"udpated\" columns support sorting.
tableHeader :: (MonadWidget t m, SortableTableGroup t m) => m ()
tableHeader = do
  el "thead" $
    el "tr" $ do
      sortHeader (view dfiName) "Name" SortAsc
      el "th" $ text "Links"
      el "th" $ text "Tag"
      el "th" $ text "App overrides"
      el "th" $ text "Deployment overrides"
      sortHeader (view $ field @"createdAt") "Created" SortDesc
      sortHeaderInitially (view $ field @"updatedAt") "Changed" SortDesc
      el "th" $
        elClass "span" "visuallyhidden" $ text "Menu"

data SortingChanged = SortingChanged
  deriving Semigroup via (S.Last SortingChanged)

type SortableTableGroup t m =
  ( MonadReader (Event t SortingChanged) m
  , MultiEventWriter t (SortDir DeploymentFullInfo) m
  , MultiEventWriter t SortingChanged m
  )

-- | Group all sortable headers ('sortHeader', 'sortHeaderInitially').
-- Makes sure that only one can be active at a time.
runSortableTableGroup
  :: (Reflex t, MonadFix m)
  => Event t SortingChanged
  -> ReaderT
     (Event t SortingChanged)
     (EventWriterT t (SortDir DeploymentFullInfo) (EventWriterT t SortingChanged m))
     x
  -> m (x, Event t (SortDir DeploymentFullInfo))
runSortableTableGroup sChanged m = mdo
  ((x, sDyn), changed) <-
    runEventWriterT . runEventWriterT . flip runReaderT (leftmost [sChanged, changed]) $ m
  return (x, sDyn)

type SortingDirection a = (DeploymentFullInfo -> a) -> SortDir DeploymentFullInfo

-- | Special column header with a sort button.
sortHeader
  :: forall t m a. (MonadWidget t m, Ord a, SortableTableGroup t m)
  => (DeploymentFullInfo -> a)
  -> Text
  -> SortingDirection a -- ^ The direction to sort when clicked
  -> m ()
sortHeader f l defaultSorting =
  sortHeaderWithInitial f l defaultSorting (Nothing @(SortingDirection a))

-- | Special column header with a sort button.
sortHeaderInitially
  :: forall t m a. (MonadWidget t m, Ord a, SortableTableGroup t m)
  => (DeploymentFullInfo -> a)
  -> Text
  -> SortingDirection a -- ^ The direction to sort when clicked and when the page loads
  -> m ()
sortHeaderInitially f l defaultSorting =
  sortHeaderWithInitial f l defaultSorting (Just @(SortingDirection a) defaultSorting)

-- | Special column header with a sort button.
sortHeaderWithInitial
  :: forall t m a. (MonadWidget t m, Ord a, SortableTableGroup t m)
  => (DeploymentFullInfo -> a)
  -> Text
  -> SortingDirection a -- ^ The direction to sort when clicked
  -> Maybe (SortingDirection a) -- ^ The direction to sort when the page loads
  -> m ()
sortHeaderWithInitial f l defaultSorting initSortingM = do
  let initSorting = case initSortingM of
        Nothing -> Nothing
        Just x -> Just $ x f
  el "th" $ mdo
    sortingChanged <- ask
    sortDyn <- foldDyn ($) initSorting $ alignWith (curry $ \case
      (That SortingChanged, _) -> Nothing -- Some other column has started sorting
      (_, Nothing) -> Just $ defaultSorting f -- This column started sorting
      (_, Just x) -> Just $ toggleSort x -- This column was sorting and was clicked
      ) sortBtnEv sortingChanged
    tellMultiEvent . fmapMaybe id $ updated sortDyn
    let
      classDyn = fmap ("sort " <>) $ sortDyn <&> \case
        Just (SortDesc _) -> "sort--active sort--desc"
        Just (SortAsc _) -> "sort--active sort--asc"
        Nothing -> ""
    sortBtnEv <- buttonDynClass classDyn (pure l)
    tellMultiEvent $ sortBtnEv $> SortingChanged
    pure ()

-- | A wrapper that adds a header to the table.
tableWrapper
  :: (MonadWidget t m)
  => Event t SortingChanged
  -> (Dynamic t (Maybe (SortDir DeploymentFullInfo)) -> m a)
  -- ^ Sorting direction is obtained from the table header.
  -> m a
tableWrapper sChanged ma =
  divClass "table table--deployments table--clickable table--double-click" $
    el "table" $ mdo
      ((), sDyn') <- runSortableTableGroup sChanged tableHeader
      sDyn <- holdDyn Nothing $ Just <$> sDyn'
      el "tbody" $ ma sDyn

-- | Table wrapper for a table with an \"error\" or a \"loading\ placeholder.
initTableWrapper :: MonadWidget t m => m () -> m ()
initTableWrapper ma = do
  divClass "data_primary" $
    tableWrapper never $ const $
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
