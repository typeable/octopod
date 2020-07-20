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

allEnvsWidget :: MonadWidget t m => Text -> Dynamic t Overrides -> m ()
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

actionsTable
  :: MonadWidget t m
  => Event t ()
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


actionsTableLoading :: MonadWidget t m => m ()
actionsTableLoading = do
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "loading loading--enlarged loading--alternate" $
          text "Loading..."

actionsTableError:: MonadWidget t m => m ()
actionsTableError = do
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "null null--data" $ do
          elClass "b" "null__heading" $ text "Cannot retrieve the data"
          divClass "null__message" $ text "Try to reload the page"

actionsTableData
  :: MonadWidget t m
  => Event t ()
  -> Dynamic t DeploymentName
  -> [DeploymentLog]
  -> m ()
actionsTableData updEv nameDyn initLogs = do
  respEv <- infoEndpoint (Right <$> nameDyn) updEv
  let
    okEv = join . fmap logs <$> fmapMaybe reqSuccess respEv
  logsDyn <- holdDyn initLogs okEv
  el "tbody" $
    void $ simpleList logsDyn $ \logDyn -> do
      dyn_ $ actinRow <$> logDyn

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
<<<<<<< HEAD
=======
    el "td" $ text $ showT $ exitCode
>>>>>>> frontend utils haddocks
    el "td" $ text $ formatPosixToDateTime createdAt
    el "td" $ text $ formatDuration duration

formatDuration :: Duration -> Text
formatDuration (Duration d) = m <> "m " <> s <> "s"
  where
    m = showT $ d `div` (1000 * 60)
    s = showT $ d `div` (1000)

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
