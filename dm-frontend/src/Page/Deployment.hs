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
import Frontend.API
import Frontend.Route
import Page.ClassicPopup
import Page.Popup.EditStaging
import Page.Popup.StagingLogs
import Page.Utils


deploymentPage
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m )
  => Event t ()
  -> DeploymentName -> m ()
deploymentPage updAllEv dname = do
  pb <- getPostBuild
  respEv <- fullInfoEndpoint (constDyn $ Right dname) pb
  let (okEv, errEv) = processResp respEv
  widgetHold_ (loadingWidget dname) $ leftmost
    [ errorWidget dname <$ errEv
    , deploymentWidget updAllEv <$> okEv ]


deploymentWidget
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m)
  => Event t ()
  -> DeploymentFullInfo
  -> m ()
deploymentWidget updEv dfi = mdo
  (editEv, logsEv) <- pageWrapper $ mdo
    retryEv <- delay 10 errEv
    respEv <- fullInfoEndpoint (constDyn $ Right $ dfi ^. dfiName)
      $ leftmost [ updEv, retryEv ]
    let (okEv, errEv) = processResp respEv
    dfiDyn <- holdDyn dfi okEv
    editEv' <- deploymentHead dfiDyn sentEv
    pageNotification $ leftmost
      [ DPMError "Couldn't update status of staging" <$ errEv
      , DPMClear <$ okEv ]
    logsEv' <- deploymentBody updEv dfiDyn
    pure (editEv', logsEv')
  sentEv <- editStagingPopup editEv never
  void $ stagingLogsPopup logsEv never

deploymentHead
  :: MonadWidget t m
  => Dynamic t DeploymentFullInfo
  -> Event t Bool
  -> m (Event t DeploymentFullInfo)
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

deploymentBodyWrapper :: MonadWidget t m => m a -> m a
deploymentBodyWrapper m = divClass "page__body" $ divClass "staging" $ m

deploymentBody
  :: MonadWidget t m
  => Event t ()
  -> Dynamic t DeploymentFullInfo
  -> m (Event t ActionId)
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
    divClass "staging__widget" $
      divClass "bar bar--larger" $ dynText tagDyn
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
    let envsDyn = dfiDyn <^.> field @"deployment" . field @"envs"
    elClass "h3" "staging__sub-heading" $ text "Overrides"
    divClass "staging__widget" $
      divClass "listing" $
        void $ simpleList envsDyn $ \envDyn -> do
          let
            varDyn = fst <$> envDyn
            valDyn = snd <$> envDyn
          divClass "listing__item bar bar--larger" $ do
            el "b" $ do
              dynText varDyn
              text ": "
            dynText valDyn
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text "Actions"
    divClass "staging__widget" $
      divClass "table table--actions" $
        actionsTable updEv nameDyn

actionsTable
  :: MonadWidget t m
  => Event t ()
  -> Dynamic t DeploymentName
  -> m (Event t ActionId)
actionsTable updEv nameDyn = do
  pb <- getPostBuild
  respEv <- infoEndpoint (Right <$> nameDyn) pb
  let
    okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqFailure respEv
  logsEvDyn <- el "table" $ do
    actionsTableHead
    widgetHold actionsTableLoading $ leftmost
      [ actionsTableError <$ errEv
      , actionsTableData updEv nameDyn <$> okEv ]
  pure $ switchDyn logsEvDyn

actionsTableHead :: MonadWidget t m => m ()
actionsTableHead =
  el "thead" $
    el "tr" $ do
      el "th" $ text "Action type"
      el "th" $ text "Image tag"
      el "th" $ text "Overrides"
      el "th" $ text "Exit code"
      el "th" $ text "Created at"
      el "th" $ text "Deployment duration"
      el "th" $ elClass "span" "visuallyhidden" $ text "Show logs action"


actionsTableLoading :: MonadWidget t m => m (Event t ActionId)
actionsTableLoading = do
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "loading loading--enlarged loading--alternate" $
          text "Loading..."
  pure never

actionsTableError:: MonadWidget t m => m (Event t ActionId)
actionsTableError = do
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "null null--data" $ do
          elClass "b" "null__heading" $ text "Cannot retrieve the data"
          divClass "null__message" $ text "Try to reload the page"
  pure never

actionsTableData
  :: MonadWidget t m
  => Event t ()
  -> Dynamic t DeploymentName
  -> [DeploymentLog]
  -> m (Event t ActionId)
actionsTableData updEv nameDyn initLogs = do
  respEv <- infoEndpoint (Right <$> nameDyn) updEv
  let
    okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
  logsDyn <- holdDyn initLogs okEv
  logsEvDyn <- el "tbody" $
    simpleList logsDyn $ \logDyn -> do
      logsEvEv <- dyn $ actinRow <$> logDyn
      switchHold never logsEvEv
  pure $ switchDyn $ leftmost <$> logsEvDyn

actinRow :: MonadWidget t m => DeploymentLog -> m (Event t ActionId)
actinRow DeploymentLog{..} = do
  el "tr" $ do
    el "td" $ text $ coerce action
    el "td" $ text $ coerce deploymentTag
    el "td" $ overridesWidget deploymentEnvs
    el "td" $ text $ pack . show $ exitCode
    el "td" $ text $ formatPosixToDateTime createdAt
    el "td" $ text $ formatDuration duration
    el "td" $ do
      bEv <- buttonClass "dash dash--smaller dash--next popup-handler" "Logs"
      pure $ actionId <$ bEv

formatDuration :: Duration -> Text
formatDuration (Duration d) = m <> "m " <> s <> "s"
  where
    m = pack . show $ d `div` (1000 * 60)
    s = pack . show $ d `div` (1000)

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

processResp
  :: Reflex t
  => Event t (ReqResult tag DeploymentFullInfo)
  -> (Event t DeploymentFullInfo, Event t ())
processResp respEv =
  let
    respOkEv = fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqFailure respEv
  in (respOkEv, () <$ errEv)

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
