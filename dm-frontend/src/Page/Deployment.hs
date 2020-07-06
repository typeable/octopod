module Page.Deployment (deploymentPage) where

import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Generics.Product (field)
import Data.List as L (find)
import Data.Text as T (Text, pack)
import Obelisk.Route.Frontend
import Reflex.Dom as R
import Servant.Reflex

import Common.Types as CT
import Common.Utils
import Frontend.API
import Frontend.Route
import Page.EditStagingPopup
import Page.Utils


deploymentPage
  ::
    ( MonadWidget t m
    , RouteToUrl (R Routes) m
    , SetRoute t (R Routes) m
    , Prerender js t m )
  => Event t ()
  -> DeploymentName -> m ()
deploymentPage updAllEv dname = pageWrapper $ do
  backButton
  pb <- getPostBuild
  respEv <- listEndpoint pb
  let (okEv, errEv) = processResp dname respEv
  widgetHold_ (loadingWidget dname) $ leftmost
    [ errorWidget dname <$ errEv
    , deploymentWidget updAllEv <$> okEv ]


deploymentWidget :: MonadWidget t m => Event t () -> DeploymentFullInfo -> m ()
deploymentWidget updEv dfi = do
  respEv <- listEndpoint updEv
  let
    (okEv, _errEv) =
      processResp (dfi ^. dfiName) respEv
  dfiDyn <- holdDyn dfi okEv
  editEv <- deploymentHead dfiDyn
  stagingNotification never
  deploymentBody updEv dfiDyn
  void $ editStagingPopup editEv never

deploymentHead
  :: MonadWidget t m
  => Dynamic t DeploymentFullInfo
  -> m (Event t DeploymentFullInfo)
deploymentHead dfiDyn =
  divClass "page__head" $ do
    let dname = dfiDyn <^.> dfiName . coerced
    elClass "h1" "page__heading title" $ dynText dname
    editEvEv <- dyn $ dfiDyn <^.> field @"archived" <&> \case
      True -> mdo
        btnEnabledDyn <- holdDyn True $ False <$ btnEv
        btnEv <- buttonClassEnabled
          "page__action button button--secondary button--restore classic-popup-handler"
          "Recover from archive"
          btnEnabledDyn
        void $ restoreEndpoint (Right . coerce <$> dname) btnEv
        pure never
      False -> do
        (editEl, _) <- elClass' "a" "page__action button button--edit popup-handler" $
          text "Edit staging"
        elClass "a" "page__action button button--secondary button--delete classic-popup-handler" $
          text "Move to archive"
        pure $ R.tag (current dfiDyn) $ domEvent Click editEl
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
        pb <- getPostBuild
        respEv <- statusEndpoint (Right <$> nameDyn) $ leftmost [pb, updEv]
        let statusEv = fmapMaybe reqSuccess respEv
        statusDyn <- holdDyn Nothing $ Just <$> statusEv
        statusWidget statusDyn
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
  blank

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


actionsTableLoading :: MonadWidget t m => m ()
actionsTableLoading =
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "loading loading--enlarged loading--alternate" $
          text "Loading..."

actionsTableError:: MonadWidget t m => m ()
actionsTableError =
  el "tbody" $
    elClass "tr" "no-table" $
      elAttr "td" ("colspan" =: "7") $
        divClass "null null--data" $ do
          elClass "b" "null__heading" $ text "Cannot retrieve the data"
          divClass "null__message" $ text "Try to reload page"

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
    void $ simpleList logsDyn $ \logDyn ->
      dyn_ $ actinRow <$> logDyn

actinRow :: MonadWidget t m => DeploymentLog -> m ()
actinRow DeploymentLog{..} = do
  el "tr" $ do
    el "td" $ text $ coerce action
    el "td" $ text $ coerce deploymentTag
    el "td" $ divClass "listing" $ do
      forM_ deploymentEnvs $ \(var, val) ->
        divClass "listing__item bar" $ do
          el "b" $ text $ var <> ":"
          text val
    el "td" $ text $ pack . show $ exitCode
    el "td" $ text $ formatPosixToDateTime createdAt
    el "td" $ text $ pack . show . unDuration $ duration

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
  => DeploymentName
  -> Event t (ReqResult tag [DeploymentFullInfo])
  -> (Event t DeploymentFullInfo, Event t ())
processResp dname respEv =
  let
    respOkEv = fmapMaybe reqSuccess respEv
    errEv' = fmapMaybe reqFailure respEv
    findFunc dfi = dfi ^. dfiName == dname
    okEv = fmapMaybe (find findFunc) respOkEv
    errEv = leftmost [ () <$ errEv', () <$ difference respOkEv okEv ]
  in (okEv, errEv)


data DeploymentPageNotification
  = DPMOk Text
  | DPMError Text

loadingWidget :: MonadWidget t m => DeploymentName -> m ()
loadingWidget dname = do
  divClass "page__head" $
    elClass "h1" "page__heading title" $ text $ coerce dname
  divClass "page__body" $
    divClass "no-staging" $
      divClass "loading loading--enlarged loading--alternate" $
        text "Loading..."

errorWidget :: MonadWidget t m => DeploymentName -> m ()
errorWidget dname = do
  divClass "page__head" $
    elClass "h1" "page__heading title" $ text $ coerce dname
  divClass "page__body" $
    divClass "no-staging" $
      divClass "null null--data" $
        divClass "null__content" $ do
          elClass "b" "null__heading" $ text "Cannot retrieve the data"
          divClass "null__message" $ text "Try to reload page"

pageWrapper :: MonadWidget t m => m a -> m a
pageWrapper m = divClass "page" $ divClass "page__wrap container" $ m

stagingNotification
  :: MonadWidget t m => Event t DeploymentPageNotification -> m ()
stagingNotification notEv = mdo
  let
    messageWidget (DPMOk txt) = messageClassWidget txt "notification--success"
    messageWidget (DPMError txt) = messageClassWidget txt "notification--danger"
    messageClassWidget txt cl =
      divClass ("page__output notification " <> cl) $ do
        text txt
        buttonClass "notification__close" ""
    closeEv = switchDyn closeEvDyn
  closeEvDyn <- widgetHold (pure never) $ leftmost
    [ messageWidget <$> notEv
    , pure never <$ closeEv ]
  blank
