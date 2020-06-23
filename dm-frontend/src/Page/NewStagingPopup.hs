module Page.NewStagingPopup where

import Control.Lens (preview, _1, _2)
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
      appErrEv = fmapMaybe (preview (_Ctor @"AppError")) errsEv
      nameErrEv = fmapMaybe (preview (_Ctor @"ValidationError" . _1 )) errsEv
      tagErrEv = fmapMaybe (preview (_Ctor @"ValidationError" . _2 )) errsEv
      toMaybe [] = Nothing
      toMaybe xs = Just $ T.intercalate ". " xs
    nameErrDyn <- holdDyn Nothing $ toMaybe <$> nameErrEv
    tagErrDyn <- holdDyn Nothing $ toMaybe <$> tagErrEv
    errorHeader appErrEv
    nameDyn <- dmTextInput "tag" "Name" "Name" nameErrDyn
    tagDyn <- dmTextInput "tag" "Tag" "Tag" tagErrDyn
    envVarsDyn <- envVarsInput
    pure $ Deployment
      <$> (DeploymentName <$> nameDyn)
      <*> (DeploymentTag <$> tagDyn)
      <*> envVarsDyn

errorHeader :: MonadWidget t m => Event t Text -> m ()
errorHeader appErrEv = do
  widgetHold_ blank $ appErrEv <&> \appErr -> do
    divClass "staging__output notification notification--danger" $ do
      el "b" $ text "App error: "
      text appErr

dmTextInput
  :: MonadWidget t m
  => Text
  -> Text
  -> Text
  -> Dynamic t (Maybe Text)
  -> m (Dynamic t Text)
dmTextInput clss lbl placeholder errDyn =
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text lbl
    elClass "div" "staging__widget" $
      dmTextInput' clss placeholder errDyn

dmTextInput'
  :: MonadWidget t m
  => Text
  -> Text
  -> Dynamic t (Maybe Text)
  -> m (Dynamic t Text)
dmTextInput' clss placeholder errDyn = do
  let
    classDyn = errDyn <&> \case
      Nothing -> clss <> " input"
      Just _  -> clss <> " input input--error"
  elDynClass "div" classDyn $ do
    inp <- inputElement $ def
      & initialAttributes .~
        (  "type" =: "text"
        <> "class" =: "input__widget"
        <> "placeholder" =: placeholder )
    dyn_ $ errDyn <&> \case
      Nothing -> blank
      Just x  -> divClass "input__output" $ text x
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
    keyDyn <- dmTextInput' "overrides__key" "key" (constDyn Nothing)
    valDyn <- dmTextInput' "overrides__value" "value" (constDyn Nothing)
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let
      envEv = updated $ zipDyn keyDyn valDyn
      deleteEv = Endo (M.delete ix) <$ closeEv
      updEv = Endo . flip update ix . const . Just <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]
