module Page.Deployments
  ( deploymentsPage ) where

import Control.Lens
import Control.Monad
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Generics.Product (field)
import Data.List as L (null)
import Data.Map as M (Map, fromList, partition, toList)
import Data.Text as T (pack)
import Data.Time
import Data.Time.Clock.POSIX
import Reflex.Dom
import Servant.Reflex

import Common.Types as CT
import Frontend.API
import Page.NewStagingPopup
import Page.Utils

deploymentsPage :: MonadWidget t m => m ()
deploymentsPage = do
  headWidget
  deploymentsWidget

headWidget :: MonadWidget t m => m ()
headWidget =
  elClass "header" "header" $
    divClass "header__wrap container" $ do
      elClass "b" "header__logo" $
        text "Deployment Manager"
      elClass "div" "header__project" $ do
        pb <- getPostBuild
        respEv <- projectName pb
        nameDyn <- holdDyn "" $ uProjectName <$> fmapMaybe reqSuccess respEv
        dynText nameDyn

deploymentsWidgetWrapper :: MonadWidget t m => m a -> m a
deploymentsWidgetWrapper m =
  divClass "page" $
    divClass "page__wrap container" m

deploymentsWidget :: forall t m . MonadWidget t m => m ()
deploymentsWidget = do
  showNewStagingEv <- deploymentsWidgetWrapper $ mdo
    showNewStagingEv' <- deploymentsHeadWidget
    initDeploymentsListWidget
    pure showNewStagingEv'
  void $ newStagingPopup showNewStagingEv never

deploymentsHeadWidget :: MonadWidget t m => m (Event t ())
deploymentsHeadWidget =
  divClass "page__head" $ do
    elClass "h1" "page__heading title" $ text "All stagings"
    divClass "page__note" $ text "Updated <n> mins ago"
    divClass "page__search input input--search input--has-clear-type" $ do
      _search <- inputElement $ def
        & initialAttributes .~
          (  "type" =: "text"
          <> "class" =: "input__widget"
          <> "placeholder" =: "Search for stagings" )
      (_, _delete) <- elClass' "button" "input__clear-type spot spot--cancel" $
        text "Delete"
      blank
    (nsEl, _) <- elClass' "a" "page__add-staging button button--add popup-handler" $
      text "New staging"
    pure $ domEvent Click nsEl

initDeploymentsListWidget :: forall t m . MonadWidget t m => m ()
initDeploymentsListWidget = dataWidgetWrapper $ do
  pb <- getPostBuild
  respEv <- listEndpoint pb
  let
    okEv = fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqFailure respEv
  widgetHold_ loadingDeploymentsWidget $ leftmost
    [ deploymentsListWidget <$> okEv
    , errDeploymentsWidget <$ errEv ]

wsUpdate :: forall t m . MonadWidget t m => m (Event t ())
wsUpdate = do
  let
    wsConfig = WebSocketConfig
      { _webSocketConfig_send = (never :: Event t [ByteString])
      , _webSocketConfig_close = never
      , _webSocketConfig_reconnect = True
      , _webSocketConfig_protocols = []
      }
  ws <- webSocket "wss://dm-genfly-ws.stage.thebestagent.pro/event" wsConfig
  pure $ () <$ _webSocket_recv ws

deploymentsListWidget :: forall t m . MonadWidget t m => [DeploymentFullInfo] -> m ()
deploymentsListWidget ds = mdo
  updAllEv <- wsUpdate
  updRespEv <- listEndpoint updAllEv
  let
    okUpdEv = fmapMaybe reqSuccess updRespEv
    mkMap = M.fromList . fmap (\x -> (x ^. field @"deployment" . field @"name" , x) )
  dsDyn <- fmap mkMap <$> holdDyn ds okUpdEv
  stsDyn <- listWithKey dsDyn $ \k d -> do
    pb <- getPostBuild
    respEv <- statusEndpoint (constDyn $ Right k) $ leftmost [pb, () <$ updated d]
    let stEv = fmapMaybe reqSuccess respEv
    pure $ (, stEv) <$> d
  let dswstDyn = flatDynMapDyn stsDyn
  let
    isArchived = view (_1 . field @"archived")
    (archivedDsDyn, activeDsDyn) = splitDynPure $ M.partition isArchived
      <$> dswstDyn
  clickedEv <- elementClick
  activeDeploymentsWidget clickedEv activeDsDyn
  archivedDeploymentsWidget clickedEv archivedDsDyn

flatDynMapDyn
  :: (Reflex t, Ord k)
  => Dynamic t (Map k (Dynamic t a))
  -> Dynamic t (Map k a)
flatDynMapDyn x =
  fmap M.fromList . join $ distributeListOverDyn . fmap f . M.toList <$> x
  where
    f (k, d) = (k,) <$> d

activeDeploymentsWidget
  :: MonadWidget t m
  => Event t ClickedElement
  -> Dynamic t
    (Map DeploymentName (DeploymentFullInfo, Event t DeploymentStatus))
  -> m ()
activeDeploymentsWidget clickedEv dsDyn =
  divClass "data__primary" $
    tableWrapper $ do
      let emptyDyn' = L.null <$> dsDyn
      emptyDyn <- holdUniqDyn emptyDyn'
      dyn_ $ emptyDyn <&> \case
        False -> void $ listWithKey dsDyn (activeDeploymentWidget clickedEv)
        True  -> emptyTableBody $ noDeploymentsWidget

statusWidget :: MonadWidget t m => Event t DeploymentStatus -> m ()
statusWidget dsEv = do
  let
    loadingW = divClass "loading loading--status-alike" $ text "Loading"
  mdsDyn <- holdDyn Nothing $ Just <$> dsEv
  mdsDyn' <- holdUniqDyn mdsDyn
  let uniqEv = fmapMaybe id $ updated mdsDyn'
  widgetHold_ loadingW $ status <$> uniqEv <&> \case
    CT.Ok -> divClass "status status--success" $ text "Success"
    CT.Error -> divClass "status status--failure" $ text "Failure"

activeDeploymentWidget
  :: MonadWidget t m
  => Event t ClickedElement
  -> DeploymentName
  -> Dynamic t (DeploymentFullInfo, Event t DeploymentStatus)
  -> m ()
activeDeploymentWidget clickedEv dname dDyn' = do
  dDyn <- holdUniqDyn $ fst <$> dDyn'
  let wstDyn = switchDyn $ snd <$> dDyn'
  dyn_ $ ffor dDyn $ \DeploymentFullInfo{..} -> do
    el "tr" $ do
      el "td" $ do
        text $ coerce dname
        statusWidget wstDyn
      el "td" $ do
        divClass "listing" $
          forM_ urls $ \(_, url) ->
            elAttr "a"
              (  "class" =: "listing__item external bar"
              <> "href" =: ("https://" <> url)
              <> "target" =: "_blank") $ text url
      el "td" $
        text $ coerce $ deployment ^. field @"tag"
      el "td" $ do
        divClass "listing" $
          forM_ (deployment ^. field @"envs") $ \(var, val) ->
            divClass "listing__item bar" $ do
              el "b" $ text $ var <> ": "
              text val
      el "td" $
        text $ pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing)
          $ intToUTCTime createdAt
      el "td" $
        text $ pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing)
          $ intToUTCTime updatedAt
      el "td" $ do
        let
          elId = "deployment_row_" <> (coerce $ deployment ^. field @"name")
          btn = elAttr "button"
            (  "class" =: "drop__handler"
            <> "type" =: "button"
            <> "id" =: elId) $ text "Actions"
          body = do
            elAttr "button"
              (  "class" =: "action action--edit"
              <> "type" =: "button") $ text "Edit"
            elAttr "button"
              (  "class" =: "action action--delete"
              <> "type" =: "button") $ text "Move to archive"
        dropdownWidget' clickedEv btn body

archivedDeploymentsWidget
  :: MonadWidget t m
  => Event t ClickedElement
  -> Dynamic t
    (Map DeploymentName (DeploymentFullInfo, Event t DeploymentStatus))
  -> m ()
archivedDeploymentsWidget clickedEv dsDyn = do
  showDyn <- toggleButton
  let
    classDyn = ffor showDyn $ \case
      True  -> "data__archive data__archive--open"
      False -> "data__archive"
  elDynClass "div" classDyn $
    tableWrapper $ do
      let emptyDyn' = L.null <$> dsDyn
      emptyDyn <- holdUniqDyn emptyDyn'
      dyn_ $ emptyDyn <&> \case
        False -> void $ list dsDyn (archivedDeploymentWidget clickedEv)
        True  -> emptyTableBody $ noDeploymentsWidget

tableHeader :: MonadWidget t m => m ()
tableHeader = do
  el "thead" $
    el "tr" $ do
      el "th" $ text "Name"
      el "th" $ text "Links"
      el "th" $ text "Tag"
      el "th" $ text "Overrides"
      el "th" $ do
        _sortBtn <- elAttr "button"
          (  "class" =: "sort sort--active sort--asc"
          <> "type" =: "button" ) $ text "Created"
        blank
      el "th" $ text "Changed"
      el "th" $
        elClass "span" "visuallyhidden" $ text "Menu"

archivedDeploymentWidget
  :: MonadWidget t m
  => Event t ClickedElement
  -> Dynamic t (DeploymentFullInfo, Event t DeploymentStatus)
  -> m ()
archivedDeploymentWidget clickedEv dDyn' = do
  dDyn <- holdUniqDyn $ fst <$> dDyn'
  dyn_ $ ffor dDyn $ \DeploymentFullInfo{..} -> do
    el "tr" $ do
      el "td" $ do
        text $ coerce $ deployment ^. field @"name"
        divClass "status status--archived" $ text "Archived"
      el "td" $ text "..."
      el "td" $
        text $ coerce $ deployment ^. field @"tag"
      el "td" $ do
        divClass "listing" $
          forM_ (deployment ^. field @"envs") $ \(var, val) ->
            divClass "listing__item bar" $ do
              el "b" $ text $ var <> ": "
              text val
      el "td" $
        text $ pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing)
          $ intToUTCTime createdAt
      el "td" $
        text $ pack $ formatTime defaultTimeLocale (iso8601DateFormat Nothing)
          $ intToUTCTime updatedAt
      el "td" $ do
        let
          elId = "deployment_row_" <> (coerce $ deployment ^. field @"name")
          btn = elAttr "button"
            (  "class" =: "drop__handler"
            <> "type" =: "button"
            <> "id" =: elId) $ text "Actions"
          body = elAttr "button"
            (  "class" =: "action action--delete"
            <> "type" =: "button") $ text "Restore from archive"
        dropdownWidget' clickedEv btn body

intToUTCTime :: Int -> UTCTime
intToUTCTime = posixSecondsToUTCTime . realToFrac

tableWrapper :: MonadWidget t m => m a -> m a
tableWrapper ma =
  divClass "stagings-table" $
    el "table" $ do
      tableHeader
      el "tbody" ma

initTableWrapper :: MonadWidget t m => m () -> m ()
initTableWrapper ma = do
  divClass "data_primary" $
    tableWrapper $
      emptyTableBody $ ma

loadingDeploymentsWidget :: MonadWidget t m => m ()
loadingDeploymentsWidget =
  initTableWrapper $
    divClass "loading loading--enlarged loading--alternate" $
      text "Loading..."

errDeploymentsWidget :: MonadWidget t m => m ()
errDeploymentsWidget =
  initTableWrapper $
    divClass "null null--data" $ do
      elClass "b" "null__heading" $
        text "Cannot retrieve the data"
      divClass "null__message" $
        text "Try to reload page"

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
  elClass "tr" "no-stagings-table" $
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
