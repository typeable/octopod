module Page.Deployments
  ( deploymentsPage ) where

import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Generics.Product
import Data.List as L (null, filter)
import Data.Text as T (pack)
import Data.Time
import Data.Time.Clock.POSIX
import Reflex.Dom
import Servant.Reflex

import Common.Types
import Frontend.API
import Page.Utils

deploymentsPage :: MonadWidget t m => m ()
deploymentsPage = do
  headWidget
  bodyLoadWidget

headWidget :: MonadWidget t m => m ()
headWidget =
  elClass "header" "header" $
    divClass "header__wrap container" $ do
      elClass "b" "header__logo" $
        text "Deployment Manager"
      elClass "div" "header__project" $
        text "<Project name>"

bodyLoadWidget :: MonadWidget t m => m ()
bodyLoadWidget = do
  pb <- getPostBuild
  respEv <- listEndpoint pb
  let
    okEv = fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqSuccess respEv
  widgetHold_ blank $ leftmost
    [ deploymentsWidget <$> okEv
    , errDeploymentsWidget <$ errEv ]
  blank

errDeploymentsWidget :: MonadWidget t m => m ()
errDeploymentsWidget =
  divClass "no-page" $
    divClass "no-page__inner" $
      divClass "null null--data" $
        divClass "null__content" $
          divClass "null__heading" $
            text "Cannot retrieve the data"

deploymentsWidgetWrapper :: MonadWidget t m => m a -> m a
deploymentsWidgetWrapper m =
  divClass "page" $
    divClass "page__wrap container" m

deploymentsWidget :: MonadWidget t m => [DeploymentFullInfo] -> m ()
deploymentsWidget ds = deploymentsWidgetWrapper $ do
  dsDyn <- holdDyn ds never
  deploymentsHeadWidget
  deploymentsListWidget dsDyn

deploymentsHeadWidget :: MonadWidget t m => m ()
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
    elClass "a" "page__add-staging button button--add popup-handler" $
      text "New staging"

deploymentsListWidget
  :: MonadWidget t m
  => Dynamic t [DeploymentFullInfo]
  -> m ()
deploymentsListWidget dsDyn = do
  let
    emptyDyn' = L.null <$> dsDyn
  emptyDyn <- holdUniqDyn emptyDyn'
  divClass "page__body" $
    dyn_ $ ffor emptyDyn $ \case
      True  -> noDeploymentsWidget
      False -> deploymentsListWidget' dsDyn

noDeploymentsWidget :: MonadWidget t m => m ()
noDeploymentsWidget =
  divClass "no-data" $
    divClass "null null--search" $ do
      elClass "b" "null__heading" $ text "No results found"
      divClass "null__message" $ do
        text "It seems we can’t find any results"
        el "br" blank
        text "based on your search"


deploymentsListWidget'
  :: MonadWidget t m
  => Dynamic t [DeploymentFullInfo]
  -> m ()
deploymentsListWidget' dsDyn = divClass "data" $ mdo
  let
    isArchived = view (field @"archived")
    activeDsDyn = L.filter (not . isArchived) <$> dsDyn
    archivedDsDyn = L.filter isArchived <$> dsDyn
  activeDeploymentsWidget activeDsDyn
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
  archivedDeploymentsWidget showDyn archivedDsDyn



activeDeploymentsWidget
  :: MonadWidget t m
  => Dynamic t [DeploymentFullInfo]
  -> m ()
activeDeploymentsWidget dsDyn =
  divClass "data__primary" $
    elClass "table" "stagings-table panel" $ do
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
      el "tbody" $
        void $ simpleList dsDyn activeDeploymentWidget

activeDeploymentWidget
  :: MonadWidget t m
  => Dynamic t DeploymentFullInfo
  -> m ()
activeDeploymentWidget dDyn' = do
  dDyn <- holdUniqDyn dDyn'
  dyn_ $ ffor dDyn $ \DeploymentFullInfo{..} -> do
    el "tr" $ do
      el "td" $ do
        text $ coerce $ deployment ^. field @"name"
        divClass "status status--success" $ text "<Status>"
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
        dropdownWidget elId btn body

intToUTCTime :: Int -> UTCTime
intToUTCTime = posixSecondsToUTCTime . realToFrac

archivedDeploymentsWidget
  :: MonadWidget t m
  => Dynamic t Bool
  -> Dynamic t [DeploymentFullInfo]
  -> m ()
archivedDeploymentsWidget showDyn dsDyn = do
  let
    classDyn = ffor showDyn $ \case
      True  -> "data__archive data__archive--open"
      False -> "data__archive"
  elDynClass "div" classDyn $
    elClass "table" "stagings-table panel" $ do
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
      el "tbody" $
        void $ simpleList dsDyn archivedDeploymentWidget

archivedDeploymentWidget
  :: MonadWidget t m
  => Dynamic t DeploymentFullInfo
  -> m ()
archivedDeploymentWidget dDyn' = do
  dDyn <- holdUniqDyn dDyn'
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
        dropdownWidget elId btn body
