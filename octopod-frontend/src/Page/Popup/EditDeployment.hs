-- |
--Module      : Page.Popup.EditDeployment
--Description : Edit deployment sidebar.
--
--This module contains the definition of the "edit deployment" sidebar.
module Page.Popup.EditDeployment (editDeploymentPopup) where

import Control.Lens (coerced, preview, to, (^.), _2)
import Control.Monad
import Data.Coerce
import Data.Functor
import Data.Generics.Product
import Data.Generics.Sum
import Data.List (deleteFirstsBy)
import qualified Data.List as L
import Data.Map as M
import Data.Monoid
import qualified Data.Text as T
import Reflex.Dom as R
import Prelude as P

import Common.Types
import Common.Utils
import Data.Text (Text)
import Frontend.API
import Frontend.Utils
import Servant.Reflex
import Servant.Reflex.Extra

-- | The root function for \"edit deployment\" sidebar.
editDeploymentPopup ::
  MonadWidget t m =>
  -- | \"Show\" event carrying an editable sidebar.
  Event t DeploymentFullInfo ->
  -- | \"Close\" event.
  Event t () ->
  -- | Event with a flag showing the current state of the request.
  m (Event t Bool)
editDeploymentPopup showEv hideEv = sidebar showEv hideEv $ \dfi -> mdo
  divClass "popup__body" $ mdo
    let dname = dfi ^. dfiName
    (closeEv', saveEv) <- editDeploymentPopupHeader dname enabledDyn
    (deploymentDyn, validDyn) <- editDeploymentPopupBody dfi respEv
    respEv <-
      updateEndpoint
        (constDyn $ Right dname)
        (Right <$> deploymentDyn)
        saveEv
    sentDyn <-
      holdDyn False $
        leftmost
          [ True <$ saveEv
          , False <$ respEv
          ]
    let successEv =
          fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
        closeEv = leftmost [closeEv', successEv]
        enabledDyn = zipDynWith (&&) (not <$> sentDyn) validDyn
    pure (updated sentDyn, closeEv)

-- | The header of the sidebar contains the deployment name and control buttons:
-- \"Save\" and \"Close\".
editDeploymentPopupHeader ::
  MonadWidget t m =>
  -- | Name of the deployment.
  DeploymentName ->
  -- | Form validation state.
  Dynamic t Bool ->
  -- | \"Close\" event and \"Save\" click event.
  m (Event t (), Event t ())
editDeploymentPopupHeader dname validDyn =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text $ "Edit " <> coerce dname
    saveEv <-
      divClass "popup__operations" $
        buttonClassEnabled "popup__action button button--save" "Save" validDyn
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)

-- | The body of the sidebar containing the edit form. Contains a tag field and
-- an override field.
editDeploymentPopupBody ::
  MonadWidget t m =>
  -- | Full deployment data.
  DeploymentFullInfo ->
  -- | \"Edit request\" failure event.
  Event t (ReqResult tag CommandResponse) ->
  -- | Returns deployment update and validation state.
  m (Dynamic t DeploymentUpdate, Dynamic t Bool)
editDeploymentPopupBody dfi errEv = divClass "popup__content" $
  divClass "deployment" $ mdo
    let commandResponseEv = fmapMaybe commandResponse errEv
        appErrEv = R.difference (fmapMaybe reqErrorBody errEv) commandResponseEv
        dfiTag = dfi ^. field @"deployment" . field @"tag" . coerced . to Just
        dfiAppVars = dfi ^. field @"deployment" . field @"appOverrides" . coerced
        dfiDeploymentVars =
          dfi ^. field @"deployment" . field @"deploymentOverrides" . coerced
        tagErrEv = getTagError commandResponseEv tagDyn
    errorHeader appErrEv
    (tagDyn, tOkEv) <- octopodTextInput "tag" "Tag" "Tag" dfiTag tagErrEv
    appVarsDyn <- envVarsInput "App overrides" dfiAppVars
    deploymentVarsDyn <- envVarsInput "Deployment overrides" dfiDeploymentVars
    let oldAppVarDyn = coerce <$> getOldVars dfiAppVars <$> appVarsDyn
        newAppVarDyn = coerce <$> getNewVars dfiAppVars <$> appVarsDyn
        oldDeploymentVarDyn = coerce <$> getOldVars dfiDeploymentVars <$> deploymentVarsDyn
        newDeploymentVarDyn = coerce <$> getNewVars dfiDeploymentVars <$> deploymentVarsDyn
    validDyn <- holdDyn True $ updated tOkEv
    pure $
      ( DeploymentUpdate
          <$> (DeploymentTag <$> tagDyn)
          <*> newAppVarDyn
          <*> oldAppVarDyn
          <*> newDeploymentVarDyn
          <*> oldDeploymentVarDyn
      , validDyn
      )
  where
    getTagError crEv tagDyn =
      let tagErrEv' = fmapMaybe (preview (_Ctor @"ValidationError" . _2)) crEv
          tagErrEv = ffilter (/= "") $ T.intercalate ". " <$> tagErrEv'
          badTagText = "Tag should not be empty"
          badNameEv = badTagText <$ (ffilter (== "") $ updated tagDyn)
       in leftmost [tagErrEv, badNameEv]
    getOldVars i u = deleteFirstsBy cmpKey i u
    getNewVars i u = deleteFirstsBy (==) u i
    cmpKey (Override k1 _ v1) (Override k2 _ v2) = k1 == k2 && v1 == v2

-- | The widget used to display errors.
errorHeader ::
  MonadWidget t m =>
  -- | Message text.
  Event t Text ->
  m ()
errorHeader appErrEv = do
  widgetHold_ blank $
    appErrEv <&> \appErr -> do
      divClass "deployment__output notification notification--danger" $ do
        el "b" $ text "App error: "
        text appErr

-- | Widget with override fields. This widget supports adding and
-- removing key-value pairs.
envVarsInput ::
  MonadWidget t m =>
  -- | Overrides header.
  Text ->
  -- | Current deployment overrides.
  Overrides ->
  -- | Updated deployment overrides.
  m (Dynamic t Overrides)
envVarsInput overridesHeader evs = do
  elClass "section" "deployment__section" $ do
    elClass "h3" "deployment__sub-heading" $ text overridesHeader
    elClass "div" "deployment__widget" $
      elClass "div" "overrides" $ mdo
        let initEnvs = L.foldl' (\m v -> fst $ insertUniq v m) emptyUniqKeyMap evs
            emptyVar = Override "" "" Public
            addEv = clickEv $> Endo (fst . insertUniq emptyVar)
        envsDyn <- foldDyn appEndo initEnvs $ leftmost [addEv, updEv]
        (_, updEv) <- runEventWriterT $ listWithKey (uniqMap <$> envsDyn) envVarInput
        let addingIsEnabled = all ((not . T.null) . overrideKey) . elemsUniq <$> envsDyn
        clickEv <-
          buttonClassEnabled'
            "overrides__add dash dash--add"
            "Add an override"
            addingIsEnabled
            "dash--disabled"
        pure $ elemsUniq <$> envsDyn

-- | Widget for entering a key-value pair. The updated overrides list is
-- written to the 'EventWriter'.
envVarInput ::
  (EventWriter t (Endo (UniqKeyMap Override)) m, MonadWidget t m) =>
  -- | Index of variable in overrides list.
  Int ->
  -- | Current variable key and value.
  Dynamic t Override ->
  m ()
envVarInput ix epDyn = do
  ep <- sample $ current epDyn
  divClass "overrides__item" $ do
    (keyDyn, _) <-
      octopodTextInput' "overrides__key" "key" (Just $ overrideKey ep) never
    (valDyn, _) <-
      octopodTextInput' "overrides__value" "value" (Just $ overrideValue ep) never
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let envEv = updated $ zipDynWith (\k v -> Override k v Public) keyDyn valDyn
        deleteEv = Endo (deleteUniq ix) <$ closeEv
        updEv = Endo . updateUniq ix . const <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]

data UniqKeyMap v = UniqKeyMap (Map Int v) (Int)

uniqMap :: UniqKeyMap v -> Map Int v
uniqMap (UniqKeyMap m _) = m

insertUniq :: v -> UniqKeyMap v -> (UniqKeyMap v, Int)
insertUniq v (UniqKeyMap m x) = (UniqKeyMap (M.insert x v m) (x + 1), x)

deleteUniq :: Int -> UniqKeyMap v -> UniqKeyMap v
deleteUniq k (UniqKeyMap m x) = UniqKeyMap (M.delete k m) x

updateUniq :: Int -> (v -> v) -> UniqKeyMap v -> UniqKeyMap v
updateUniq k f (UniqKeyMap m x) = UniqKeyMap (M.adjust f k m) x

elemsUniq :: UniqKeyMap v -> [v]
elemsUniq (UniqKeyMap m _) = M.elems m

emptyUniqKeyMap :: UniqKeyMap v
emptyUniqKeyMap = UniqKeyMap mempty 0
