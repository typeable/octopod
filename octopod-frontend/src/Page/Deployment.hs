-- |
--Module      : Page.Deployment
--Description : Deployment page.
--
--This module contains the definition of a deployment page.
module Page.Deployment (deploymentPage) where

import Common.Types as CT
import Common.Utils
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Data.Align
import Data.Coerce
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Product (field)
import Data.Text as T (Text, pack)
import Data.Time
import Data.UniqMap
import Data.Witherable
import Frontend.API
import Frontend.GHCJS
import Frontend.Route
import Frontend.UIKit
import Frontend.Utils
import Obelisk.Route.Frontend
import Page.ClassicPopup
import Page.Elements.Links
import Page.Popup.EditDeployment
import Reflex.Dom as R
import Reflex.Network
import Servant.Reflex
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
                largeButton $
                  def
                    & #buttonType .~~ pure (Just RestoreLargeButtonType)
                    & #buttonPriority .~~ SecondaryLargeButton
                    & #buttonStyle .~~ PageActionLargeButtonStyle
                    & #buttonText .~~ "Recover from archive"
                    & #buttonEnabled .~~ btnEnabledDyn
              void $ restoreEndpoint (Right . coerce <$> dname) $ btnEv $> ()
              pure (never, never)
            else mdo
              let btnState = not $ isPending . recordedStatus $ dfi ^. field @"status"
              btnEnabledDyn <- holdDyn btnState $ not <$> sentEv
              editEv <-
                largeButton $
                  def
                    & #buttonType .~~ pure (Just EditLargeButtonType)
                    & #buttonStyle .~~ PageActionLargeButtonStyle
                    & #buttonText .~~ "Edit deployment"
                    & #buttonEnabled .~~ btnEnabledDyn
              archEv <-
                largeButton $
                  def
                    & #buttonType .~~ pure (Just ArchiveLargeButtonType)
                    & #buttonPriority .~~ SecondaryLargeButton
                    & #buttonStyle .~~ PageActionLargeButtonStyle
                    & #buttonText .~~ "Move to archive"
                    & #buttonEnabled .~~ btnEnabledDyn
              pure (R.tag (current dfiDyn) editEv, archEv)
    url' <- kubeDashboardUrl dfiDyn
    void . dyn $
      url'
        <&> maybe
          blank
          ( \url ->
              void $
                largeButton $
                  def
                    { buttonText = "Details"
                    , buttonType = pure $ Just LogsLargeButtonType
                    , buttonPriority = SecondaryLargeButton
                    , buttonStyle = PageActionLargeButtonStyle
                    , buttonBaseTag = ATag url
                    }
          )
    delEv <- confirmArchivePopup (archEv $> ()) $ do
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
deploymentBody updEv dfiDyn = deploymentBodyWrapper $
  wrapRequestErrors $ \hReq -> do
    let nameDyn = dfiDyn <^.> dfiName
        depDyn = dfiDyn <^.> #deployment
    (defDepCfgEv, defAppCfgEv, _) <- deploymentConfigProgressiveComponents hReq (depDyn <^.> #deploymentOverrides)
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
    deploymentSection "Links" $ do
      let urlsDyn = dfiDyn <^.> field @"metadata" . to unDeploymentMetadata
      divClass "deployment__widget" $
        divClass "listing" $
          void $ simpleList urlsDyn renderMetadataLink
    deploymentSection "Deployment configuration" $ showVars defDepCfgEv $ depDyn <^.> #deploymentOverrides
    defAppMDyn <- holdDynMaybe defAppCfgEv
    let appCfgEv = catMaybes . updated $ do
          defAppM <- defAppMDyn
          appOvs <- depDyn <^.> #appOverrides
          pure $ defAppM <&> applyOverrides appOvs
    deploymentSection "App configuration" $ showVars defAppCfgEv $ depDyn <^.> #appOverrides
    deploymentSection "Actions" $
      divClass "table table--actions" $
        actionsTable hReq updEv nameDyn

showVars ::
  MonadWidget t m =>
  Event t (DefaultConfig l) ->
  Dynamic t (Overrides l) ->
  m ()
showVars defCfgEv ovsDyn = do
  workingOvsEv <- constructWorkingOverridesEv defCfgEv ovsDyn
  _ <-
    divClass "deployment__widget" $
      networkHold nonEditableLoading $
        workingOvsEv <&> \x -> do
          showNonEditableWorkingOverrideTree (pure x)
          pure ()
  pure ()

-- | Widget with a table of actions that can be performed on a deployment.
-- It requests deployment data.
-- If a request fails it shows an error message,
-- otherwise it calls 'actionsTableData', passing the received data.
actionsTable ::
  MonadWidget t m =>
  RequestErrorHandler t m ->
  -- | Event notifying about the need to update data.
  Event t () ->
  Dynamic t DeploymentName ->
  m ()
actionsTable hReq updEv nameDyn = do
  pb <- getPostBuild
  respEv <- infoEndpoint (Right <$> nameDyn) pb
  let okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
      errEv = fmapMaybe reqErrorBody respEv
  el "table" $ do
    actionsTableHead
    widgetHold_ actionsTableLoading $
      leftmost
        [ actionsTableError <$ errEv
        , actionsTableData hReq updEv nameDyn <$> okEv
        ]

-- | Header of the actions table.
actionsTableHead :: MonadWidget t m => m ()
actionsTableHead =
  el "thead" $
    el "tr" $ do
      el "th" $ text "Action type"
      el "th" $ text "Deployment configuration"
      el "th" $ text "App configuration"
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
  RequestErrorHandler t m ->
  -- | Event notifying about the need to update data.
  Event t () ->
  Dynamic t DeploymentName ->
  -- | Initial logs.
  [DeploymentLog] ->
  m ()
actionsTableData hReq updEv nameDyn initLogs = do
  respEv <- infoEndpoint (Right <$> nameDyn) updEv
  let okEv = (>>= logs) <$> fmapMaybe reqSuccess respEv
  logsDyn <- holdDyn initLogs okEv
  el "tbody" $
    void $
      simpleList logsDyn $ \logDyn -> do
        dyn_ $ actinRow hReq <$> logDyn

-- | Data row of the actions table.
actinRow :: RequestErrorHandler t m -> MonadWidget t m => DeploymentLog -> m ()
actinRow hReq DeploymentLog {..} = do
  el "tr" $ do
    el "td" $ do
      text $ actionToText action
      let statusClass =
            "status "
              <> if exitCode == 0 then "status--success" else "status--failure"
      divClass statusClass blank
    el "td" $ overridesWidget deploymentDepOverrides
    el "td" $ overridesWidget deploymentAppOverrides
    el "td" $ text $ showT $ exitCode
    el "td" $ text $ formatPosixToDateTime createdAt
    el "td" $ text $ formatDuration duration

-- | Formats posix seconds to date in iso8601 with time.
formatDuration :: FormatTime t => t -> Text
formatDuration = pack . formatTime defaultTimeLocale "%mm %Ss"

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
