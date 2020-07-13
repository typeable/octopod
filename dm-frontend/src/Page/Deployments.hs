module Page.Deployments
  ( deploymentsPage ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Coerce
import Data.Generics.Product (field)
import Data.List as L (null, sortBy)
import Data.Map as M (Map, fromList, partition, filter, elems)
import Data.Text as T (Text, toCaseFold, isPrefixOf, pack)
import Data.Time (getCurrentTime, diffUTCTime)
import Obelisk.Route.Frontend
import Reflex.Dom
import Servant.Reflex

import Common.Types as CT
import Common.Utils
import Frontend.API
import Frontend.Route
import Page.ClassicPopup
import Page.Popup.NewStaging
import Page.Utils


deploymentsPage
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()
  -> m ()
deploymentsPage = deploymentsWidget

deploymentsWidgetWrapper :: MonadWidget t m => m a -> m a
deploymentsWidgetWrapper m =
  divClass "page" $
    divClass "page__wrap container" m

deploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()
  -> m ()
deploymentsWidget updAllEv = do
  showNewStagingEv <- deploymentsWidgetWrapper $ mdo
    (showNewStagingEv', termDyn) <- deploymentsHeadWidget okUpdEv
    okUpdEv <- initDeploymentsListWidget updAllEv termDyn
    pure showNewStagingEv'
  void $ newStagingPopup showNewStagingEv never

deploymentsHeadWidget
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (), Dynamic t Text)
deploymentsHeadWidget okUpdEv =
  divClass "page__head" $ do
    elClass "h1" "page__heading title" $ text "All stagings"
    lastUpdateWidget okUpdEv
    termDyn <- divClass "page__action input input--search input--has-clear-type\
    \ page__action--search" $ mdo
      termDyn' <- inputElement $ def
        & initialAttributes .~
          (  "type" =: "text"
          <> "class" =: "input__widget"
          <> "placeholder" =: "Search for stagings" )
        & inputElementConfig_setValue .~ ("" <$ domEvent Click deleteEl)
      (deleteEl, _) <- elClass' "button" "input__clear-type spot spot--cancel" $
        text "Delete"
      pure termDyn'
    (nsEl, _) <- elClass' "a"
      "page__action button button--add popup-handler" $ text "New staging"
    pure $ (domEvent Click nsEl, value termDyn)

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
      | isSingular ms = "Updated " <> (show' $ ms) <> " minute ago"
      | ms < 60 = "Updated " <> (show' $ ms) <> " minutes ago"
      | isSingular (ms `div` 60) =
        "Updated " <> (show'$ ms `div` 60) <> " hour ago"
      | otherwise = "Updated " <> (show' $ ms `div` 60) <> " hours ago"
    isSingular x = x `mod` 100 /= 11 && x `mod` 10 == 1
    show' = pack . show
  diffDyn <- holdDyn 0 diffEv
  divClass "page__note" $ dynText $ mkMsg <$> diffDyn

initDeploymentsListWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()
  -> Dynamic t Text
  -> m (Event t ())
initDeploymentsListWidget updAllEv termDyn = dataWidgetWrapper $ do
  pb <- getPostBuild
  respEv <- listEndpoint pb
  let
    okEv = fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqFailure respEv
    w m = m >> pure never
  okUpdEvDyn <- widgetHold (w loadingDeploymentsWidget) $ leftmost
    [ deploymentsListWidget updAllEv termDyn <$> okEv
    , (w errDeploymentsWidget) <$ errEv ]
  pure $ switchDyn okUpdEvDyn

deploymentsListWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ()
  -> Dynamic t Text
  -> [DeploymentFullInfo]
  -> m (Event t ())
deploymentsListWidget updAllEv termDyn ds = mdo
  updRespEv <- listEndpoint updAllEv
  let
    okUpdEv = fmapMaybe reqSuccess updRespEv
    mkMap = M.fromList . fmap (\x -> (x ^. dfiName , x) )
  dsDyn <- fmap mkMap <$> holdDyn ds okUpdEv
  let
    isArchived = view (field @"archived")
    filteredDyn = ffor2 termDyn dsDyn $ \term ds' ->
      M.filter (searchDeployments term) ds'
    (archivedDsDyn, activeDsDyn) = splitDynPure $ M.partition isArchived
      <$> filteredDyn
  clickedEv <- elementClick
  activeDeploymentsWidget clickedEv activeDsDyn
  archivedDeploymentsWidget clickedEv archivedDsDyn
  pure $ () <$ okUpdEv

searchDeployments
  :: Text -> DeploymentFullInfo -> Bool
searchDeployments "" _   = True
searchDeployments term d = term' `isPrefixOf` dname
  || term' `isPrefixOf` dtag
  where
    term' = toCaseFold term
    dtag = d ^. field @"deployment" . field @"tag" . coerced . to toCaseFold
    dname = d ^. dfiName . coerced . to toCaseFold

activeDeploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -> Dynamic t
    (Map DeploymentName DeploymentFullInfo)
  -> m ()
activeDeploymentsWidget clickedEv dsDyn =
  divClass "data__primary" $
    tableWrapper $ \sortDyn -> do
      let
        emptyDyn' = L.null <$> dsDyn
        dsSortedDyn = zipDynWith sortDeployments dsDyn sortDyn
      emptyDyn <- holdUniqDyn emptyDyn'
      dyn_ $ emptyDyn <&> \case
        False -> void $ list dsSortedDyn
          (activeDeploymentWidget clickedEv)
        True  -> emptyTableBody $ noDeploymentsWidget

activeDeploymentWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -> Dynamic t (DeploymentFullInfo)
  -> m ()
activeDeploymentWidget clickedEv dDyn' = do
  dDyn <- holdUniqDyn dDyn'
  dyn_ $ ffor dDyn $ \DeploymentFullInfo{..} -> do
    let dname = deployment ^. field @"name"
    (linkEl, btnEv) <- el' "tr" $ do
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
        overridesWidget $ deployment ^. field @"envs"
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
            _ <- buttonClass "action action--edit" "Edit"
            btnArcEv <- buttonClass "action action--delete" "Move to archive"
            pure btnArcEv
        dropdownWidget' clickedEv btn body
    delEv <- confirmDeletePopup btnEv $ do
      text "Are you sure you want to delete"
      el "br" blank
      text $ coerce dname <> " staging?"
    void $ deleteEndpoint (constDyn $ Right $ dname) delEv
    let route = DashboardRoute :/ Just dname
    setRoute $ route <$ domEvent Dblclick linkEl

archivedDeploymentsWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m )
  => Event t ClickedElement
  -> Dynamic t
    (Map DeploymentName DeploymentFullInfo)
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
        False -> void $ list dsSortedDyn
          (archivedDeploymentWidget clickedEv)
        True  -> emptyTableBody $ noDeploymentsWidget

sortDeployments
  :: Map DeploymentName DeploymentFullInfo
  -> SortDir
  -> Map Int DeploymentFullInfo
sortDeployments ds s = M.fromList $ zip [1..] $ L.sortBy sortFunc items
  where
    items = M.elems ds
    sortFunc a b = case s of
      SortAsc get -> compare (a ^. get) (b ^. get)
      SortDesc get -> compare (b ^. get) (a ^. get)


data SortDir where
  SortAsc  :: Ord a => Getter DeploymentFullInfo a -> SortDir
  SortDesc :: Ord a => Getter DeploymentFullInfo a -> SortDir

toggleSort :: SortDir -> SortDir
toggleSort = \case
  SortAsc x  -> SortDesc x
  SortDesc x -> SortAsc x

tableHeader :: MonadWidget t m => m (Dynamic t SortDir)
tableHeader = do
  el "thead" $
    el "tr" $ do
      nameSortDyn <- sortHeader dfiName "Name"
      el "th" $ text "Links"
      el "th" $ text "Tag"
      el "th" $ text "Overrides"
      createSortDyn <- sortHeader (field @"createdAt") "Created"
      updateSortDyn <- sortHeader (field @"updatedAt") "Changed"
      el "th" $
        elClass "span" "visuallyhidden" $ text "Menu"
      let sortEv = leftmost $ fmap updated [nameSortDyn, createSortDyn, updateSortDyn]
      sortDyn <- holdDyn (SortAsc dfiName) sortEv
      pure sortDyn


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
        overridesWidget $ deployment ^. field @"envs"
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

tableWrapper :: MonadWidget t m => (Dynamic t SortDir -> m a) -> m a
tableWrapper ma =
  divClass "table table--stagings table--clickable" $
    el "table" $ do
      sDyn <- tableHeader
      el "tbody" $ ma sDyn

initTableWrapper :: MonadWidget t m => m () -> m ()
initTableWrapper ma = do
  divClass "data_primary" $
    tableWrapper $ const $
      emptyTableBody $ ma

loadingDeploymentsWidget :: MonadWidget t m => m ()
loadingDeploymentsWidget =
  initTableWrapper $
    loadingCommonWidget

errDeploymentsWidget :: MonadWidget t m => m ()
errDeploymentsWidget =
  initTableWrapper $
    errorCommonWidget

noDeploymentsWidget' :: MonadWidget t m => m () -> m () -> m ()
noDeploymentsWidget' h b =
  divClass "null null--search" $ do
    elClass "b" "null__heading" h
    divClass "null__message" b

noDeploymentsWidget :: MonadWidget t m => m ()
noDeploymentsWidget = noDeploymentsWidget' (text "No stagings") blank

_badSearchWidget :: MonadWidget t m => m ()
_badSearchWidget = do
  let
    h = text "No results found"
    b = do
      text "It seems we can’t find any results"
      el "br" blank
      text "based on your search"
  noDeploymentsWidget' h b

emptyTableBody :: MonadWidget t m => m () -> m ()
emptyTableBody msg =
  elClass "tr" "no-table" $
    elAttr "td" ("colspan" =: "7") msg

dataWidgetWrapper :: MonadWidget t m => m a -> m a
dataWidgetWrapper ma = divClass "page__body" $ divClass "data" ma

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
