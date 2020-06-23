module Page.NewStagingPopup where

import Control.Lens (preview)
import Control.Monad
import Data.Functor
import Data.Generics.Sum
import Data.Map as M
import Data.Monoid
import Data.Text as T (Text, intercalate)
import Reflex.Dom

import Common.Types
import Frontend.API
import Page.Utils
import Servant.Reflex


newStagingPopup
  :: MonadWidget t m
  => Event t ()
  -> Event t ()
  -> m (Event t ())
newStagingPopup showEv hideEv = sidebar showEv hideEv $ mdo
  divClass "popup__body" $ mdo
    (closeEv', saveEv) <- newStagingPopupHeader
    deploymentDyn <- newStagingPopupBody respEv
    respEv <- createEndpoint (Right <$> deploymentDyn) saveEv
    let
      successEv =
        fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
      closeEv = leftmost [ closeEv', successEv ]
    pure (never, closeEv)

newStagingPopupHeader :: MonadWidget t m => m (Event t (), Event t ())
newStagingPopupHeader =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text "Create new staging"
    saveEv <- divClass "popup__operations" $
      buttonClass "popup__action button button--save" "Save"
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)


newStagingPopupBody
  :: MonadWidget t m
  => Event t (ReqResult tag CommandResponse)
  -> m (Dynamic t Deployment)
newStagingPopupBody errEv =
  divClass "staging" $ do
    let
      commandResponseEv = fmapMaybe commandResponse errEv
      otherFailureEv = AppError <$> fmapMaybe reqFailure errEv
      errsEv = leftmost [commandResponseEv, otherFailureEv]
    widgetHold_ blank $ errsEv <&> \case
      ValidationError {..} -> do
        let errs = nameField <> tagField
        divClass "staging__output notification notification--danger" $ do
          el "b" $ text "Validation error: "
          text $ T.intercalate ". " errs
      AppError {..} ->
        divClass "staging__output notification notification--danger" $ do
          el "b" $ text "App error: "
          text errorMessage
      _ -> blank
    nameDyn <- dmTextInput "tag" "Name" "Name" (constDyn Nothing)
    tagDyn <- dmTextInput "tag" "Tag" "Tag" (constDyn Nothing)
    envVarsDyn <- envVarsInput
    pure $ Deployment
      <$> (DeploymentName <$> nameDyn)
      <*> (DeploymentTag <$> tagDyn)
      <*> envVarsDyn


dmTextInput
  :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> Dynamic t (Maybe Text)
  -> m (Dynamic t Text)
dmTextInput clss lbl placeholder _errDyn =
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text lbl
    elClass "div" "staging__widget" $
      dmTextInput' clss placeholder

dmTextInput' :: MonadWidget t m => Text -> Text -> m (Dynamic t Text)
dmTextInput' clss placeholder = do
  elClass "div" (clss <> " input") $ do
    inp <- inputElement $ def
      & initialAttributes .~
        (  "type" =: "text"
        <> "class" =: "input__widget"
        <> "placeholder" =: placeholder )
    pure $ value inp

envVarsInput :: MonadWidget t m => m (Dynamic t EnvPairs)
envVarsInput = do
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text "Overrides"
    elClass "div" "staging__widget" $
      elClass "div" "overrides" $ mdo
        let
          emptyVar = ("", "")
          addEv = clickEv $> Endo (\envs -> length envs =: emptyVar <> envs)
        envsDyn <- foldDyn appEndo mempty $ leftmost [ addEv, updEv ]
        (_, updEv)  <- runEventWriterT $ listWithKey envsDyn envVarInput
        clickEv <- buttonClass "overrides__add dash dash--add" "Add an override"
        pure $ elems <$> envsDyn

envVarInput
  :: (EventWriter t (Endo (Map Int EnvPair)) m, MonadWidget t m)
  => Int
  -> Dynamic t EnvPair
  -> m ()
envVarInput ix _ = do
  divClass "overrides__item" $ do
    keyDyn <- dmTextInput' "overrides__key" "key"
    valDyn <- dmTextInput' "overrides__value" "value"
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let
      envEv = updated $ zipDyn keyDyn valDyn
      deleteEv = Endo (M.delete ix) <$ closeEv
      updEv = Endo . flip update ix . const . Just <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]
