module Page.Popup.EditStaging where

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
import Page.Utils
import Servant.Reflex


editStagingPopup
  :: MonadWidget t m
  => Event t DeploymentFullInfo
  -> Event t ()
  -> m (Event t Bool)
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

editStagingPopupHeader
  :: MonadWidget t m
  => DeploymentName
  -> Dynamic t Bool
  -> m (Event t (), Event t ())
editStagingPopupHeader dname validDyn =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text $ "Edit " <> coerce dname
    saveEv <- divClass "popup__operations" $
      buttonClassEnabled "popup__action button button--save" "Save" validDyn
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)

editStagingPopupBody
  :: MonadWidget t m
  => DeploymentFullInfo
  -> Event t (ReqResult tag CommandResponse)
  -> m (Dynamic t DeploymentUpdate, Dynamic t Bool)
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

errorHeader :: MonadWidget t m => Event t Text -> m ()
errorHeader appErrEv = do
  widgetHold_ blank $ appErrEv <&> \appErr -> do
    divClass "staging__output notification notification--danger" $ do
      el "b" $ text "App error: "
      text appErr

envVarsInput
  :: MonadWidget t m
  => Text
  -> Overrides
  -> m (Dynamic t Overrides)
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

envVarInput
  :: (EventWriter t (Endo (Map Int Override)) m, MonadWidget t m)
  => Int
  -> Dynamic t Override
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
