{-|
Module      : Page.Popup.EditStaging
Description : Edit staging sidebar.

This module contains the definition of the "edit staging" sidebar.
-}

module Page.Popup.EditStaging (editStagingPopup) where

import Control.Lens (preview, (^.), coerced, to, _2)
import Control.Monad
import Data.Coerce
import Data.Functor
import Data.Generics.Product
import Data.Generics.Sum
import Data.List (deleteFirstsBy)
import Data.Map as M
import Data.Monoid
import Data.Text as T (Text, intercalate)
import Prelude as P
import Reflex.Dom as R

import Common.Types
import Common.Utils
import Frontend.API
import Frontend.Utils
import Servant.Reflex

-- | The root function for \"edit staging\" sidebar.
editStagingPopup
  :: MonadWidget t m
  => Event t DeploymentFullInfo
  -- ^ \"Show\" event carrying an editable sidebar.
  -> Event t ()
  -- ^ \"Close\" event.
  -> m (Event t Bool)
  -- ^ Event with a flag showing the current state of the request.
editStagingPopup showEv hideEv = sidebar showEv hideEv $ \dfi -> mdo
  divClass "popup__body" $ mdo
    let dname = dfi ^. dfiName
    (closeEv', saveEv) <- editStagingPopupHeader dname enabledDyn
    (deploymentDyn, validDyn) <- editStagingPopupBody dfi respEv
    respEv <- updateEndpoint (constDyn $ Right dname)
      (Right <$> deploymentDyn) saveEv
    sentDyn <- holdDyn False $ leftmost
      [ True <$ saveEv
      , False <$ respEv ]
    let
      successEv =
        fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
      closeEv = leftmost [ closeEv', successEv ]
      enabledDyn = zipDynWith (&&) (not <$> sentDyn) validDyn
    pure (updated sentDyn, closeEv)

-- | The header of the sidebar contains the staging name and control buttons:
-- \"Save\" and \"Close\".
editStagingPopupHeader
  :: MonadWidget t m
  => DeploymentName             -- ^ Name of the deployment.
  -> Dynamic t Bool             -- ^ Form validation state.
  -> m (Event t (), Event t ()) -- ^ \"Close\" event and \"Save\" click event.
editStagingPopupHeader dname validDyn =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text $ "Edit " <> coerce dname
    saveEv <- divClass "popup__operations" $
      buttonClassEnabled "popup__action button button--save" "Save" validDyn
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)

-- | The body of the sidebar containing the edit form. Contains a tag field and
-- an override field.
editStagingPopupBody
  :: MonadWidget t m
  => DeploymentFullInfo
  -- ^ Full staging data.
  -> Event t (ReqResult tag CommandResponse)
  -- ^ \"Edit request\" failure event.
  -> m (Dynamic t DeploymentUpdate, Dynamic t Bool)
  -- ^ Returns deployment update and validation state.
editStagingPopupBody dfi errEv = divClass "popup__content" $
  divClass "staging" $ mdo
    let
      commandResponseEv = fmapMaybe commandResponse errEv
      appErrEv = R.difference (fmapMaybe reqFailure errEv) commandResponseEv
      dfiTag = dfi ^. field @"deployment" . field @"tag" . coerced . to Just
      dfiAppVars = dfi ^. field @"deployment" . field @"appOverrides" . coerced
      dfiStagingVars =
        dfi ^. field @"deployment" . field @"stagingOverrides" . coerced
      tagErrEv = getTagError commandResponseEv tagDyn
    errorHeader appErrEv
    (tagDyn, tOkEv) <- dmTextInput "tag" "Tag" "Tag" dfiTag tagErrEv
    appVarsDyn <- envVarsInput "App overrides" dfiAppVars
    stagingVarsDyn <- envVarsInput "Stagings overrides" dfiStagingVars
    let
      oldAppVarDyn = coerce <$> getOldVars dfiAppVars <$> appVarsDyn
      newAppVarDyn = coerce <$> getNewVars dfiAppVars <$> appVarsDyn
      oldStagingVarDyn = coerce <$> getOldVars dfiAppVars <$> stagingVarsDyn
      newStagingVarDyn = coerce <$> getNewVars dfiAppVars <$> stagingVarsDyn
    validDyn <- holdDyn True $ updated tOkEv
    pure $ (DeploymentUpdate
      <$> (DeploymentTag <$> tagDyn)
      <*> newAppVarDyn
      <*> oldAppVarDyn
      <*> newStagingVarDyn
      <*> oldStagingVarDyn, validDyn)
  where
    getTagError crEv tagDyn = let
      tagErrEv' = fmapMaybe (preview (_Ctor @"ValidationError" . _2 )) crEv
      tagErrEv = ffilter (/= "") $ T.intercalate ". " <$> tagErrEv'
      badTagText = "Tag should not be empty"
      badNameEv = badTagText <$ (ffilter (== "") $ updated tagDyn)
      in leftmost [tagErrEv, badNameEv]
    getOldVars i u = deleteFirstsBy cmpKey i u
    getNewVars i u = deleteFirstsBy (==) u i
    cmpKey (Override k1 _ v1) (Override k2 _ v2) = k1 == k2 && v1 == v2

-- | The widget used to display errors.
errorHeader
  :: MonadWidget t m
  => Event t Text -- ^ Message text.
  -> m ()
errorHeader appErrEv = do
  widgetHold_ blank $ appErrEv <&> \appErr -> do
    divClass "staging__output notification notification--danger" $ do
      el "b" $ text "App error: "
      text appErr

-- | Widget with override fields. This widget supports adding and
-- removing key-value pairs.
envVarsInput
  :: MonadWidget t m
  => Text                    -- ^ Overrides header.
  -> Overrides               -- ^ Current staging overrides.
  -> m (Dynamic t Overrides) -- ^ Updated staging overrides.
envVarsInput overridesHeader evs = do
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text overridesHeader
    elClass "div" "staging__widget" $
      elClass "div" "overrides" $ mdo
        let
          initEnvs = fromList $ zip [0..] evs
          emptyVar = Override "" "" Public
          addEv = clickEv $> Endo (\envs -> P.length envs =: emptyVar <> envs)
        envsDyn <- foldDyn appEndo initEnvs $ leftmost [ addEv, updEv ]
        (_, updEv)  <- runEventWriterT $ listWithKey envsDyn envVarInput
        let addDisabledDyn = all ( (/= "") . overrideKey ) . M.elems <$> envsDyn
        clickEv <- buttonClassEnabled'
          "overrides__add dash dash--add" "Add an override" addDisabledDyn
          "dash--disabled"
        pure $ elems <$> envsDyn

-- | Widget for entering a key-value pair. The updated overrides list is
-- written to the 'EventWriter'.
envVarInput
  :: (EventWriter t (Endo (Map Int Override)) m, MonadWidget t m)
  => Int                -- ^ Index of variable in overrides list.
  -> Dynamic t Override -- ^ Current variable key and value.
  -> m ()
envVarInput ix epDyn = do
  ep <- sample $ current epDyn
  divClass "overrides__item" $ do
    (keyDyn, _) <-
      dmTextInput' "overrides__key" "key" (Just $ overrideKey ep) never
    (valDyn, _) <-
      dmTextInput' "overrides__value" "value" (Just $ overrideValue ep) never
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let
      envEv = updated $ zipDynWith (\k v -> Override k v Public) keyDyn valDyn
      deleteEv = Endo (M.delete ix) <$ closeEv
      updEv = Endo . flip update ix . const . Just <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]
