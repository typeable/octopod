{-|
Module      : Page.Deployment
Description : Staging page.

This module contains the definition of a staging page.
-}

module Page.Deployment (deploymentPage) where

import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Generics.Product (field)
import Data.Text as T (Text)
import Obelisk.Route.Frontend
import Reflex.Dom as R
import Servant.Reflex

import Common.Types as CT
import Common.Utils
import Frontend.API
import Frontend.Route
import Frontend.Utils
import Page.ClassicPopup
import Page.Popup.EditStaging

-- | The root widget of a staging page. It requests the staging data. If the
-- request fails it shows an error, otherwise it calls 'deploymentWidget', passing
-- the received data.
deploymentPage
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m )
  => Event t ()     -- ^ Event notifying about the need to update data.
  -> DeploymentName -- ^ Name of current deployment.
  -> m ()
deploymentPage updAllEv dname = do
  pb <- getPostBuild
  respEv <- fullInfoEndpoint (constDyn $ Right dname) pb
  let (okEv, errEv) = processResp respEv
  widgetHold_ (loadingWidget dname) $ leftmost
    [ errorWidget dname <$ errEv
    , deploymentWidget updAllEv <$> okEv ]

-- | Staging page widget that takes the initial staging data. It updates this data
-- every time when the passed event fires. If an update fails, a notification
-- widget appears at the top of the page.
deploymentWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m)
  => Event t ()         -- ^ Event notifying about the need to update data.
  -> DeploymentFullInfo -- ^ Initial staging data.
  -> m ()
deploymentWidget updEv dfi = mdo
  editEv <- pageWrapper $ mdo
    retryEv <- delay 10 errEv
    respEv <- fullInfoEndpoint (constDyn $ Right $ dfi ^. dfiName)
      $ leftmost [ updEv, retryEv ]
    let (okEv, errEv) = processResp respEv
    dfiDyn <- holdDyn dfi okEv
    editEv' <- deploymentHead dfiDyn sentEv
    pageNotification $ leftmost
      [ DPMError "Couldn't update status of staging" <$ errEv
      , DPMClear <$ okEv ]
    deploymentBody updEv dfiDyn
    pure (editEv')
  sentEv <- editStagingPopup editEv never
  blank

-- | The header of a staging page. It contains the stating name and control
-- buttons that depend on the status of the staging:
--  * \"Archived\" status: a \"restore\" button.
--  * \"Running\" status: \"archive staging\" and \"edit staging\" buttons.
-- If the status is pending (\"Creating\", \"Updating\", etc) then all buttons
-- are inactive.
deploymentHead
  :: MonadWidget t m
  => Dynamic t DeploymentFullInfo
  -- ^ Staging data.
  -> Event t Bool
  -- ^ Event with a flag showing the current state of the request.
  -> m (Event t DeploymentFullInfo)
  -- ^ \"Edit\" event.
deploymentHead dfiDyn sentEv =
  divClass "page__head" $ do
    let dname = dfiDyn <^.> dfiName . coerced
    elClass "h1" "page__heading title" $ dynText dname
    editEvEv <- dyn $ dfiDyn <&> \dfi -> case dfi ^. field @"archived" of
      True -> mdo
        let btnState = not $ isPending $ dfi ^. field @"status"
        btnEnabledDyn <- holdDyn btnState $ leftmost [ False <$ btnEv, sentEv ]
        btnEv <- aButtonClassEnabled
          "page__action button button--secondary button--restore classic-popup-handler"
          "Recover from archive"
          btnEnabledDyn
        void $ restoreEndpoint (Right . coerce <$> dname) btnEv
        pure never
      False -> mdo
        let btnState = not $ isPending $ dfi ^. field @"status"
        btnEnabledDyn <- holdDyn btnState $ not <$> sentEv
        editEv <- aButtonClassEnabled
          "page__action button button--edit popup-handler"
          "Edit staging"
          btnEnabledDyn
        archEv <- aButtonClassEnabled
          "page__action button button--secondary button--delete \
          \classic-popup-handler"
          "Move to archive"
          btnEnabledDyn
        delEv <- confirmDeletePopup archEv $ do
          text "Are you sure you want to delete"
          el "br" blank
          dynText dname
          text " staging?"
        void $ deleteEndpoint (Right . coerce <$> dname) delEv
        pure $ R.tag (current dfiDyn) editEv
    switchHold never editEvEv

-- | Div wrappers.
deploymentBodyWrapper :: MonadWidget t m => m a -> m a
deploymentBodyWrapper m = divClass "page__body" $ divClass "staging" $ m

-- | Body of a staging page.
deploymentBody
  :: MonadWidget t m
  => Event t ()
  -- ^ Event notifying about the need to update data.
  -> Dynamic t DeploymentFullInfo
  -- ^ Staging data.
  -> m ()
deploymentBody updEv dfiDyn = deploymentBodyWrapper $ do
  let nameDyn = dfiDyn <^.> dfiName
  divClass "staging__summary" $ do
    divClass "staging__stat" $ do
      elClass "b" "staging__param" $ text "Status"
      divClass "staging__value" $ do
        statusWidget $ dfiDyn <^.> field @"status"
    divClass "staging__stat" $ do
      elClass "b" "staging__param" $ text "Created"
      divClass "staging__value" $ do
        let createdAtDyn = dfiDyn <^.> field @"createdAt"
        dynText $ formatPosixToDate <$> createdAtDyn
    divClass "staging__stat" $ do
      elClass "b" "staging__param" $ text "Changed"
      divClass "staging__value" $ do
        let createdAtDyn = dfiDyn <^.> field @"updatedAt"
        dynText $ formatPosixToDate <$> createdAtDyn
  elClass "section" "staging__section" $ do
    let tagDyn = dfiDyn <^.> field @"deployment" . field @"tag" . coerced
    elClass "h3" "staging__sub-heading" $ text "Tag"
    divClass "staging__widget" $ dynText tagDyn
  elClass "section" "staging__section" $ do
    let urlsDyn = dfiDyn <^.> field @"urls"
    elClass "h3" "staging__sub-heading" $ text "Links"
    divClass "staging__widget" $
      divClass "listing" $
        void $ simpleList urlsDyn $ \urlDyn' -> do
          let
            urlDyn = snd <$> urlDyn'
            attrDyn = urlDyn <&> \url ->
              (  "class" =: "listing__item external bar bar--larger"
              <> "href" =: ("https://" <> url)
              <> "target" =: "_blank" )
          elDynAttr "a" attrDyn $ dynText urlDyn
  elClass "section" "staging__section" $ do
    let
      envsDyn = dfiDyn <^.> field @"deployment"
        . field @"appOverrides" . coerced
    allEnvsWidget "App overrides" envsDyn
  elClass "section" "staging__section" $ do
    let
      envsDyn = dfiDyn <^.> field @"deployment"
        . field @"stagingOverrides" . coerced
    allEnvsWidget "Staging overrides" envsDyn
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text "Actions"
    divClass "staging__widget" $
      divClass "table table--actions" $
        actionsTable updEv nameDyn

-- | Widget that shows overrides list. It does not depend on their type.
allEnvsWidget
  :: MonadWidget t m
  => Text                -- ^ Widget header.
  -> Dynamic t Overrides -- ^ Overrides list.
  -> m ()
allEnvsWidget headerText envsDyn = do
  elClass "h3" "staging__sub-heading" $ text headerText
  divClass "staging__widget" $
    divClass "listing listing--for-text listing--larger" $
      void $ simpleList envsDyn $ \envDyn -> do
        let
          varDyn = overrideKey <$> envDyn
          valDyn = overrideValue <$> envDyn
        divClass "listing__item" $ do
          el "b" $ do
            dynText varDyn
            text ": "
          dynText valDyn

-- ^ Widget with a table of actions that can be performed on a staging. It
-- requests staging data.
-- If a request fails it shows an error message, otherwise it calls 'actionsTableData',
-- passing the received data.
actionsTable
  :: MonadWidget t m
  => Event t ()
  -- ^ Event notifying about the need to update data.
  -> Dynamic t DeploymentName
  -> m ()
actionsTable updEv nameDyn = do
  pb <- getPostBuild
  respEv <- infoEndpoint (Right <$> nameDyn) pb
  let
    okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqFailure respEv
  el "table" $ do
    actionsTableHead
    widgetHold_ actionsTableLoading $ leftmost
      [ actionsTableError <$ errEv
      , actionsTableData updEv nameDyn <$> okEv ]

-- | Header of the actions table.
actionsTableHead :: MonadWidget t m => m ()
actionsTableHead =
  el "thead" $
    el "tr" $ do
      el "th" $ text "Action type"
      el "th" $ text "Image tag"
      el "th" $ text "App overrides"
      el "th" $ text "Staging overrides"
      el "th" $ text "Exit code"
      el "th" $ text "Created"
      el "th" $ text "Deployment duration"

-- | Widget with a loading spinner for the actions table.
actionsTableLoading :: MonadWidget t m => m ()
actionsTableLoading = do
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "loading loading--enlarged loading--alternate" $
          text "Loading..."

-- | Widget with an error message for the actions table.
actionsTableError:: MonadWidget t m => m ()
actionsTableError = do
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "null null--data" $ do
          elClass "b" "null__heading" $ text "Cannot retrieve the data"
          divClass "null__message" $ text "Try to reload the page"

-- | Actions table body. It updates data every time when the supplied event fires.
actionsTableData
  :: MonadWidget t m
  => Event t ()
  -- ^ Event notifying about the need to update data.
  -> Dynamic t DeploymentName
  -> [DeploymentLog]
  -- ^ Initial logs.
  -> m ()
actionsTableData updEv nameDyn initLogs = do
  respEv <- infoEndpoint (Right <$> nameDyn) updEv
  let
    okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
  logsDyn <- holdDyn initLogs okEv
  el "tbody" $
    void $ simpleList logsDyn $ \logDyn -> do
      dyn_ $ actinRow <$> logDyn

-- | Data row of the actions table.
actinRow :: MonadWidget t m => DeploymentLog -> m ()
actinRow DeploymentLog{..} = do
  el "tr" $ do
    el "td" $ do
      text $ coerce action
      let
        statusClass = "status " <>
          if exitCode == 0 then "status--success" else "status--failure"
      divClass statusClass blank
    el "td" $ text $ coerce deploymentTag
    el "td" $ overridesWidget $ coerce $ deploymentAppOverrides
    el "td" $ overridesWidget $ coerce $ deploymentStagingOverrides
    el "td" $ text $ formatPosixToDateTime createdAt
    el "td" $ text $ formatDuration duration

-- | Convert the duration of an action from milliseconds to a human readable format.
formatDuration
  :: Duration -- ^ Duration in milliseconds.
  -> Text
formatDuration (Duration d) = m <> "m " <> s <> "s"
  where
    m = showT $ d `div` (1000 * 60)
    s = showT $ d `div` (1000)

-- | Widget with a button that returns to stagings list page.
backButton
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m )
  => m ()
backButton = do
  let
    backRoute = constDyn $ DashboardRoute :/ Nothing
    attrs = constDyn $ "class" =: "page__back dash dash--back dash--smaller"
  routeLinkDynAttr attrs backRoute $ text "All stagings"

-- | Widget with a loading spinner.
loadingWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m)
  => DeploymentName
  -> m ()
loadingWidget dname = pageWrapper $ do
  divClass "page__head" $
    elClass "h1" "page__heading title" $ text $ coerce dname
  divClass "page__body" $
    divClass "no-staging" $
      loadingCommonWidget

-- | Widget with an error placeholder.
errorWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m)
  => DeploymentName
  -> m ()
errorWidget dname = pageWrapper $ do
  divClass "page__head" $
    elClass "h1" "page__heading title" $ text $ coerce dname
  divClass "page__body" $
    divClass "no-staging" $
      errorCommonWidget

-- | Div wrappers.
pageWrapper
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m)
  => m a
  -> m a
pageWrapper m = divClass "page" $ divClass "page__wrap container" $ do
  backButton
  m
