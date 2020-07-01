module Page.EditStagingPopup where

import Control.Lens (preview, _2, (^.), coerced, to)
import Control.Monad
import Data.Coerce
import Data.Functor
import Data.Generics.Product
import Data.Generics.Sum
import Data.Map as M
import Data.Monoid
import Data.Text as T (Text, intercalate)
import Prelude as P
import Reflex.Dom

import Common.Types
import Common.Utils
import Frontend.API
import Page.Utils
import Servant.Reflex


editStagingPopup
  :: MonadWidget t m
  => Event t DeploymentFullInfo
  -> Event t ()
  -> m (Event t ())
editStagingPopup showEv hideEv = sidebar showEv hideEv $ \dfi -> mdo
  divClass "popup__body" $ mdo
    let dname = dfi ^. dfiName
    (closeEv', saveEv) <- editStagingPopupHeader dname
    (deploymentDyn) <- editStagingPopupBody dfi respEv
    respEv <- updateEndpoint (constDyn $ Right dname)
      (Right <$> deploymentDyn) saveEv
    let
      successEv =
        fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
      closeEv = leftmost [ closeEv', successEv ]
    pure (never, closeEv)

editStagingPopupHeader
  :: MonadWidget t m
  => DeploymentName
  -> m (Event t (), Event t ())
editStagingPopupHeader dname =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text $ "Edit " <> coerce dname
    saveEv <- divClass "popup__operations" $
      buttonClass "popup__action button button--save" "Save"
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)


editStagingPopupBody
  :: MonadWidget t m
  => DeploymentFullInfo
  -> Event t (ReqResult tag CommandResponse)
  -> m (Dynamic t DeploymentUpdate)
editStagingPopupBody dfi errEv =
  divClass "staging" $ mdo
    let
      commandResponseEv = fmapMaybe commandResponse errEv
      otherFailureEv = AppError <$> fmapMaybe reqFailure errEv
      errsEv = leftmost [commandResponseEv, otherFailureEv]
      appErrEv = fmapMaybe (preview (_Ctor @"AppError")) errsEv
      tagErrEv = fmapMaybe (preview (_Ctor @"ValidationError" . _2 )) errsEv
      toMaybe [] = Nothing
      toMaybe xs = Just $ T.intercalate ". " xs
      dfiTag = dfi ^. field @"deployment" . field @"tag" . coerced . to Just
      dfiVars = dfi ^. field @"deployment" . field @"envs"
    tagErrDyn <- holdDyn Nothing $ toMaybe <$> tagErrEv
    errorHeader appErrEv
    tagDyn <- dmTextInput "tag" "Tag" "Tag" dfiTag tagErrDyn
    envVarsDyn <- envVarsInput dfiVars
    pure $ DeploymentUpdate
      <$> (DeploymentTag <$> tagDyn)
      <*> (Just <$> envVarsDyn)

errorHeader :: MonadWidget t m => Event t Text -> m ()
errorHeader appErrEv = do
  widgetHold_ blank $ appErrEv <&> \appErr -> do
    divClass "staging__output notification notification--danger" $ do
      el "b" $ text "App error: "
      text appErr

envVarsInput :: MonadWidget t m => EnvPairs -> m (Dynamic t EnvPairs)
envVarsInput evs = do
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text "Overrides"
    elClass "div" "staging__widget" $
      elClass "div" "overrides" $ mdo
        let
          initEnvs = fromList $ zip [0..] evs
          emptyVar = ("", "")
          addEv = clickEv $> Endo (\envs -> P.length envs =: emptyVar <> envs)
        envsDyn <- foldDyn appEndo initEnvs $ leftmost [ addEv, updEv ]
        (_, updEv)  <- runEventWriterT $ listWithKey envsDyn envVarInput
        clickEv <- buttonClass "overrides__add dash dash--add" "Add an override"
        pure $ elems <$> envsDyn

envVarInput
  :: (EventWriter t (Endo (Map Int EnvPair)) m, MonadWidget t m)
  => Int
  -> Dynamic t EnvPair
  -> m ()
envVarInput ix epDyn = do
  ep <- sample $ current epDyn
  divClass "overrides__item" $ do
    keyDyn <- dmTextInput' "overrides__key" "key" (Just $ fst ep) (constDyn Nothing)
    valDyn <- dmTextInput' "overrides__value" "value" (Just $ snd ep) (constDyn Nothing)
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let
      envEv = updated $ zipDyn keyDyn valDyn
      deleteEv = Endo (M.delete ix) <$ closeEv
      updEv = Endo . flip update ix . const . Just <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]
