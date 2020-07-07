module Page.Popup.NewStaging where

import Control.Lens (preview, _1, _2)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Functor
import Data.Generics.Sum
import Data.Map as M
import Data.Monoid
import Data.Text as T (Text, intercalate, length)
import Data.Text.Encoding as T
import Prelude as P
import Reflex.Dom
import Text.Regex.TDFA

import Common.Types
import Frontend.API
import Page.Utils
import Servant.Reflex


newStagingPopup
  :: MonadWidget t m
  => Event t ()
  -> Event t ()
  -> m (Event t ())
newStagingPopup showEv hideEv = sidebar showEv hideEv $ const $ mdo
  divClass "popup__body" $ mdo
    (closeEv', saveEv) <- newStagingPopupHeader enabledDyn
    (deploymentDyn, enabledDyn) <- newStagingPopupBody respEv
    respEv <- createEndpoint (Right <$> deploymentDyn) saveEv
    let
      successEv =
        fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
      closeEv = leftmost [ closeEv', successEv ]
    pure (never, closeEv)

newStagingPopupHeader
  :: MonadWidget t m
  => Dynamic t Bool
  -> m (Event t (), Event t ())
newStagingPopupHeader enabledDyn =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text "Create new staging"
    saveEv <- divClass "popup__operations" $
      buttonClassEnabled "popup__action button button--save" "Save" enabledDyn
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)


newStagingPopupBody
  :: MonadWidget t m
  => Event t (ReqResult tag CommandResponse)
  -> m (Dynamic t Deployment, Dynamic t Bool)
newStagingPopupBody errEv = divClass "popup__content" $
  divClass "staging" $ mdo
    let
      commandResponseEv = fmapMaybe commandResponse errEv
      otherFailureEv = AppError <$> fmapMaybe reqFailure errEv
      errsEv = leftmost [commandResponseEv, otherFailureEv]
      appErrEv = fmapMaybe (preview (_Ctor @"AppError")) errsEv
      nameErrEv = fmapMaybe (preview (_Ctor @"ValidationError" . _1 )) errsEv
      tagErrEv = fmapMaybe (preview (_Ctor @"ValidationError" . _2 )) errsEv
      toMaybe [] = Nothing
      toMaybe xs = Just $ T.intercalate ". " xs
      isNameValidDyn = nameValidation <$> nameDyn
      badNameText = Just "Name is not correct"
      badNameEv = badNameText <$ (ffilter not $ updated isNameValidDyn)
      okNameEv = Nothing <$ (ffilter id $ updated isNameValidDyn)
    nameErrDyn <- holdDyn Nothing $ leftmost
      [ toMaybe <$> nameErrEv
      , badNameEv
      , okNameEv ]
    tagErrDyn <- holdDyn Nothing $ toMaybe <$> tagErrEv
    errorHeader appErrEv
    nameDyn <- dmTextInput "tag" "Name" "Name" Nothing nameErrDyn
    tagDyn <- dmTextInput "tag" "Tag" "Tag" Nothing tagErrDyn
    envVarsDyn <- envVarsInput
    pure $ (Deployment
      <$> (DeploymentName <$> nameDyn)
      <*> (DeploymentTag <$> tagDyn)
      <*> envVarsDyn, isNameValidDyn)

errorHeader :: MonadWidget t m => Event t Text -> m ()
errorHeader appErrEv = do
  widgetHold_ blank $ appErrEv <&> \appErr -> do
    divClass "staging__output notification notification--danger" $ do
      el "b" $ text "App error: "
      text appErr

envVarsInput :: MonadWidget t m => m (Dynamic t EnvPairs)
envVarsInput = do
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text "Overrides"
    elClass "div" "staging__widget" $
      elClass "div" "overrides" $ mdo
        let
          emptyVar = ("", "")
          addEv = clickEv $> Endo (\envs -> P.length envs =: emptyVar <> envs)
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
    keyDyn <- dmTextInput' "overrides__key" "key" Nothing (constDyn Nothing)
    valDyn <- dmTextInput' "overrides__value" "value" Nothing (constDyn Nothing)
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let
      envEv = updated $ zipDyn keyDyn valDyn
      deleteEv = Endo (M.delete ix) <$ closeEv
      updEv = Endo . flip update ix . const . Just <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]

nameValidation :: Text -> Bool
nameValidation name =
  (T.length name >= 2 && T.length name <= 20)
  && (T.encodeUtf8 name =~ ("^[a-z0-9\\-]+$" :: ByteString))
