-- |
--Module      : Page.Deployment
--Description : Deployment page.
--
--This module contains the definition of a deployment page.
module Page.Deployment (deploymentPage) where

import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Generics.Product (field)
import Data.Text as T (Text, pack)
import Obelisk.Route.Frontend
import Reflex.Dom as R
import Servant.Reflex

import Common.Types as CT
import Common.Utils
import Control.Monad.Reader
import Data.Align
import Data.Generics.Labels ()
import qualified Data.Map.Ordered.Strict as OM
import Data.Time
import Frontend.API
import Frontend.GHCJS
import Frontend.Route
import Frontend.Utils
import Page.ClassicPopup
import Page.Elements.Links
import Page.Popup.EditDeployment
import Servant.Reflex.Extra

-- | The root widget of a deployment page. It requests the deployment data.
-- If the request fails it shows an error,
-- otherwise it calls 'deploymentWidget', passing the received data.
deploymentPage ::
  ( MonadWidget t m
  , RouteToUrl (R Routes) m
  , SetRoute t (R Routes) m
  , Prerender js t m
  , MonadReader ProjectConfig m
  ) =>
  -- | Event notifying about the need to update data.
  Event t () ->
  -- | Name of current deployment.
  DeploymentName ->
  m ()
deploymentPage updAllEv dname = do
  pb <- getPostBuild
  respEv <- fullInfoEndpoint (constDyn $ Right dname) pb
  let (okEv, errEv) = processResp respEv
  widgetHold_ (loadingWidget dname) $
    leftmost
      [ errorWidget dname <$ errEv
      , deploymentWidget updAllEv <$> okEv
      ]

-- | Deployment page widget that takes the initial deployment data.
-- It updates this data every time when the passed event fires.
-- If an update fails, a notification widget appears at the top of the page.
deploymentWidget ::
  ( MonadWidget t m
  , RouteToUrl (R Routes) m
  , SetRoute t (R Routes) m
  , Prerender js t m
  , MonadReader ProjectConfig m
  ) =>
  -- | Event notifying about the need to update data.
  Event t () ->
  -- | Initial deployment data.
  DeploymentFullInfo ->
  m ()
deploymentWidget updEv dfi = mdo
  editEv <- pageWrapper $ mdo
    retryEv <- delay 10 errEv
    respEv <-
      fullInfoEndpoint (constDyn $ Right $ dfi ^. dfiName) $
        leftmost [updEv, retryEv]
    let (okEv, errEv) = processResp respEv
    dfiDyn <- holdDyn dfi okEv
    editEv' <- deploymentHead dfiDyn sentEv
    pageNotification $
      leftmost
        [ DPMError "Couldn't update status of deployment" <$ errEv
        , DPMClear <$ okEv
        ]
    deploymentBody updEv dfiDyn
    pure editEv'
  sentEv <- editDeploymentPopup editEv never
  blank

-- | The header of a deployment page. It contains the stating name
-- and control buttons that depend on the status of the deployment:
--  * \"Archived\" status: a \"restore\" button.
--  * \"Running\" status: \"archive deployment\"
--    and \"edit deployment\" buttons.
-- If the status is pending (\"Creating\", \"Updating\", etc)
-- then all buttons are inactive.
deploymentHead ::
  (MonadWidget t m, MonadReader ProjectConfig m) =>
  -- | Deployment data.
  Dynamic t DeploymentFullInfo ->
  -- | Event with a flag showing the current state of the request.
  Event t Bool ->
  -- | \"Edit\" event.
  m (Event t DeploymentFullInfo)
deploymentHead dfiDyn sentEv =
  divClass "page__head" $ do
    let dname = dfiDyn <^.> dfiName . coerced
    elClass "h1" "page__heading title" $ dynText dname
    (editEv, archEv) <-
      hold2 . dyn $
        dfiDyn <&> \dfi ->
          if isDeploymentArchived dfi
            then mdo
              let btnState = not $ isPending . recordedStatus $ dfi ^. field @"status"
              btnEnabledDyn <- holdDyn btnState $ leftmost [False <$ btnEv, sentEv]
              btnEv <-
                aButtonClassEnabled
                  "page__action button button--secondary button--restore \
                  \classic-popup-handler"
                  "Recover from archive"
                  btnEnabledDyn
              void $ restoreEndpoint (Right . coerce <$> dname) btnEv
              pure (never, never)
            else mdo
              let btnState = not $ isPending . recordedStatus $ dfi ^. field @"status"
              btnEnabledDyn <- holdDyn btnState $ not <$> sentEv
              editEv <-
                buttonClassEnabled'
                  "page__action button button--edit popup-handler"
                  "Edit deployment"
                  btnEnabledDyn
                  "button--disabled"
              archEv <-
                buttonClassEnabled'
                  "page__action button button--secondary button--archive \
                  \classic-popup-handler"
                  "Move to archive"
                  btnEnabledDyn
                  "button--disabled"
              pure (R.tag (current dfiDyn) editEv, archEv)
    url' <- kubeDashboardUrl dfiDyn
    void . dyn $
      url'
        <&> maybe
          blank
          ( \url ->
              void $
                aButtonDynClass'
                  "page__action button button--secondary button--logs"
                  "Details"
                  (pure $ "href" =: url <> "target" =: "_blank")
          )
    delEv <- confirmArchivePopup archEv $ do
      text "Are you sure you want to archive the"
      el "br" blank
      dynText dname
      text " deployment?"
    void $ archiveEndpoint (Right . coerce <$> dname) delEv
    return editEv

hold2 ::
  (MonadHold t m, Reflex t) =>
  m (Event t (Event t a, Event t b)) ->
  m (Event t a, Event t b)
hold2 = (>>= fmap fanThese . switchHold never . fmap (uncurry align))

-- | Div wrappers.
deploymentBodyWrapper :: MonadWidget t m => m a -> m a
deploymentBodyWrapper m = divClass "page__body" $ divClass "deployment" $ m

-- | Body of a deployment page.
deploymentBody ::
  MonadWidget t m =>
  -- | Event notifying about the need to update data.
  Event t () ->
  -- | Deployment data.
  Dynamic t DeploymentFullInfo ->
  m ()
deploymentBody updEv dfiDyn = deploymentBodyWrapper $ do
  let nameDyn = dfiDyn <^.> dfiName
      cfg = dfiDyn <&> getDeploymentConfig
  divClass "deployment__summary" $ do
    divClass "deployment__stat" $ do
      elClass "b" "deployment__param" $ text "Status"
      divClass "deployment__value" $ do
        statusWidget $ dfiDyn <^.> field @"status"
    divClass "deployment__stat" $ do
      elClass "b" "deployment__param" $ text "Created"
      divClass "deployment__value" $ do
        let createdAtDyn = dfiDyn <^.> field @"createdAt"
        dynText $ formatPosixToDate <$> createdAtDyn
    divClass "deployment__stat" $ do
      elClass "b" "deployment__param" $ text "Changed"
      divClass "deployment__value" $ do
        let createdAtDyn = dfiDyn <^.> field @"updatedAt"
        dynText $ formatPosixToDate <$> createdAtDyn
  elClass "section" "deployment__section" $ do
    let tagDyn = dfiDyn <^.> field @"deployment" . field @"tag" . coerced
    elClass "h3" "deployment__sub-heading" $ text "Tag"
    divClass "deployment__widget" $ dynText tagDyn
  elClass "section" "deployment__section" $ do
    let urlsDyn = dfiDyn <^.> field @"metadata" . to unDeploymentMetadata
    elClass "h3" "deployment__sub-heading" $ text "Links"
    divClass "deployment__widget" $
      divClass "listing" $
        void $ simpleList urlsDyn renderMetadataLink
  elClass "section" "deployment__section" $ do
    let envsDyn = cfg <^.> #appConfig
    allEnvsWidget "App overrides" envsDyn
  elClass "section" "deployment__section" $ do
    let envsDyn = cfg <^.> #depConfig
    allEnvsWidget "Deployment overrides" envsDyn
  elClass "section" "deployment__section" $ do
    elClass "h3" "deployment__sub-heading" $ text "Actions"
    divClass "deployment__widget" $
      divClass "table table--actions" $
        actionsTable updEv nameDyn

-- | Widget that shows overrides list. It does not depend on their type.
allEnvsWidget ::
  MonadWidget t m =>
  -- | Widget header.
  Text ->
  -- | Overrides list.
  Dynamic t (Config l) ->
  m ()
allEnvsWidget headerText envsDyn = do
  elClass "h3" "deployment__sub-heading" $ text headerText
  divClass "deployment__widget" $
    divClass "listing listing--for-text listing--larger" $
      void $
        simpleList (OM.assocs . unConfig <$> envsDyn) $ \envDyn -> do
          let varDyn = fst <$> envDyn
              valDyn = snd <$> envDyn
          divClass "listing__item" $ do
            el "b" $ do
              dynText varDyn
              text ": "
            dynText valDyn
-- ^ Widget with a table of actions that can be performed on a deployment.
-- It requests deployment data.
-- If a request fails it shows an error message,
-- otherwise it calls 'actionsTableData', passing the received data.

actionsTable ::
  MonadWidget t m =>
  -- | Event notifying about the need to update data.
  Event t () ->
  Dynamic t DeploymentName ->
  m ()
actionsTable updEv nameDyn = do
  pb <- getPostBuild
  respEv <- infoEndpoint (Right <$> nameDyn) pb
  let okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
      errEv = fmapMaybe reqErrorBody respEv
  el "table" $ do
    actionsTableHead
    widgetHold_ actionsTableLoading $
      leftmost
        [ actionsTableError <$ errEv
        , actionsTableData updEv nameDyn <$> okEv
        ]

-- | Header of the actions table.
actionsTableHead :: MonadWidget t m => m ()
actionsTableHead =
  el "thead" $
    el "tr" $ do
      el "th" $ text "Action type"
      el "th" $ text "Image tag"
      el "th" $ text "App overrides"
      el "th" $ text "Deployment overrides"
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
actionsTableError :: MonadWidget t m => m ()
actionsTableError = do
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "null null--data" $ do
          elClass "b" "null__heading" $ text "Cannot retrieve the data"
          divClass "null__message" $ text "Try to reload the page"

-- | Actions table body.
-- It updates data every time when the supplied event fires.
actionsTableData ::
  MonadWidget t m =>
  -- | Event notifying about the need to update data.
  Event t () ->
  Dynamic t DeploymentName ->
  -- | Initial logs.
  [DeploymentLog] ->
  m ()
actionsTableData updEv nameDyn initLogs = do
  respEv <- infoEndpoint (Right <$> nameDyn) updEv
  let okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
  logsDyn <- holdDyn initLogs okEv
  el "tbody" $
    void $
      simpleList logsDyn $ \logDyn -> do
        dyn_ $ actinRow <$> logDyn

-- | Data row of the actions table.
actinRow :: MonadWidget t m => DeploymentLog -> m ()
actinRow DeploymentLog {..} = do
  el "tr" $ do
    el "td" $ do
      text $ actionToText action
      let statusClass =
            "status "
              <> if exitCode == 0 then "status--success" else "status--failure"
      divClass statusClass blank
    el "td" $ text $ coerce deploymentTag
    el "td" $ overridesWidget $ deploymentAppOverrides
    el "td" $ overridesWidget $ deploymentDepOverrides
    el "td" $ text $ showT $ exitCode
    el "td" $ text $ formatPosixToDateTime createdAt
    el "td" $ text $ formatDuration duration

-- | Formats posix seconds to date in iso8601 with time.
formatDuration :: FormatTime t => t -> Text
formatDuration = pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%mm %Ss"))

-- | Convert the duration of an action from milliseconds
-- to a human readable format.
-- | Widget with a button that returns to deployments list page.
backButton ::
  ( MonadWidget t m
  , RouteToUrl (R Routes) m
  , SetRoute t (R Routes) m
  , Prerender js t m
  ) =>
  m ()
backButton = do
  let backRoute = constDyn $ DashboardRoute :/ Nothing
      attrs = constDyn $ "class" =: "page__back dash dash--back dash--smaller"
  routeLinkDynAttr attrs backRoute $ text "All deployments"

-- | Widget with a loading spinner.
loadingWidget ::
  ( MonadWidget t m
  , RouteToUrl (R Routes) m
  , SetRoute t (R Routes) m
  , Prerender js t m
  ) =>
  DeploymentName ->
  m ()
loadingWidget dname = pageWrapper $ do
  divClass "page__head" $
    elClass "h1" "page__heading title" $ text $ coerce dname
  divClass "page__body" $
    divClass "no-deployment" $
      loadingCommonWidget

-- | Widget with an error placeholder.
errorWidget ::
  ( MonadWidget t m
  , RouteToUrl (R Routes) m
  , SetRoute t (R Routes) m
  , Prerender js t m
  ) =>
  DeploymentName ->
  m ()
errorWidget dname = pageWrapper $ do
  divClass "page__head" $
    elClass "h1" "page__heading title" $ text $ coerce dname
  divClass "page__body" $
    divClass "no-deployment" $
      errorCommonWidget

-- | Div wrappers.
pageWrapper ::
  ( MonadWidget t m
  , RouteToUrl (R Routes) m
  , SetRoute t (R Routes) m
  , Prerender js t m
  ) =>
  m a ->
  m a
pageWrapper m = divClass "page" $
  divClass "page__wrap container" $ do
    backButton
    m
