{-|
Module      : Page.Popup.NewDeployment
Description : New deployment sidebar.

This module contains the definition of \"new deployment\" sidebar.
-}


module Page.Popup.NewDeployment (newDeploymentPopup) where

import           Control.Lens (preview, _1, _2)
import           Control.Monad
import           Data.Coerce
import           Data.Functor
import           Data.Generics.Sum
import           Data.Map as M
import           Data.Monoid
import           Data.Text as T (Text, intercalate)
import           Prelude as P
import           Reflex.Dom as R

import           Common.Types
import           Common.Validation (isNameValid)
import           Frontend.API
import           Frontend.Utils
import           Servant.Reflex
import           Servant.Reflex.Extra


-- | The root function for \"new deployment\" sidebar.
newDeploymentPopup
  :: MonadWidget t m
  => Event t ()     -- ^ \"Show\" event.
  -> Event t ()     -- ^ \"Close\" event.
  -> m ()
newDeploymentPopup showEv hideEv = void $ sidebar showEv hideEv $ const $ mdo
  divClass "popup__body" $ mdo
    (closeEv', saveEv) <- newDeploymentPopupHeader enabledDyn
    (deploymentDyn, validDyn) <- newDeploymentPopupBody respEv
    respEv <- createEndpoint (Right <$> deploymentDyn) saveEv
    sentDyn <- holdDyn False $ leftmost
      [ True <$ saveEv
      , False <$ respEv ]
    let
      successEv =
        fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
      closeEv = leftmost [ closeEv', successEv ]
      enabledDyn = zipDynWith (&&) (not <$> sentDyn) validDyn
    pure (never, closeEv)

-- | The header of sidebar contains control buttons: \"Save\" and \"Close\".
newDeploymentPopupHeader
  :: MonadWidget t m
  => Dynamic t Bool
  -> m (Event t (), Event t ())
newDeploymentPopupHeader enabledDyn =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text "Create new deployment"
    saveEv <- divClass "popup__operations" $
      buttonClassEnabled "popup__action button button--save" "Save" enabledDyn
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)

-- | The body of the sidebar contains the creation form. It contains: a name field,
-- a tag field and overrides fields. The name field is validated with the regexp:
-- @^[a-z][a-z0-9\\-]{1,16}$@.
newDeploymentPopupBody
  :: MonadWidget t m
  => Event t (ReqResult tag CommandResponse)
  -- ^ Request failure event.
  -> m (Dynamic t Deployment, Dynamic t Bool)
  -- ^ Returns new deployment and validation states.
newDeploymentPopupBody errEv = divClass "popup__content" $
  divClass "deployment" $ mdo
    let
      commandResponseEv = fmapMaybe commandResponse errEv
      appErrEv = R.difference (fmapMaybe reqErrorBody errEv) commandResponseEv
      nameErrEv = getNameError commandResponseEv nameDyn
      tagErrEv = getTagError commandResponseEv tagDyn
    errorHeader appErrEv
    (nameDyn, nOkDyn) <- octopodTextInput "tag" "Name" "Name" Nothing nameErrEv
    (tagDyn, tOkDyn) <- octopodTextInput "tag" "Tag" "Tag" Nothing tagErrEv
    appVarsDyn <- envVarsInput "App overrides"
    deploymentVarsDyn <- envVarsInput "Deployment overrides"
    validDyn <- holdDyn False $ updated $ zipDynWith (&&) nOkDyn tOkDyn
    pure $ (Deployment
      <$> (DeploymentName <$> nameDyn)
      <*> (DeploymentTag <$> tagDyn)
      <*> (coerce <$> appVarsDyn)
      <*> (coerce <$> deploymentVarsDyn), validDyn)
  where
    getNameError crEv nameDyn = let
      nameErrEv' = fmapMaybe (preview (_Ctor @"ValidationError" . _1 )) crEv
      isNameValidDyn = isNameValid . DeploymentName <$> nameDyn
      badNameText = "Deployment name length should be longer than 2 characters \
      \and under 17 characters and begin with a letter."
      badNameEv = badNameText <$ (ffilter not $ updated isNameValidDyn)
      nameErrEv = ffilter (/= "") $ T.intercalate ". " <$> nameErrEv'
      in leftmost [nameErrEv, badNameEv]
    getTagError crEv tagDyn = let
      tagErrEv' = fmapMaybe (preview (_Ctor @"ValidationError" . _2 )) crEv
      tagErrEv = ffilter (/= "") $ T.intercalate ". " <$> tagErrEv'
      badTagText = "Tag should not be empty"
      badNameEv = badTagText <$ (ffilter (== "") $ updated tagDyn)
      in leftmost [tagErrEv, badNameEv]

-- | The widget used to display errors.
errorHeader
  :: MonadWidget t m
  => Event t Text -- ^ Message text.
  -> m ()
errorHeader appErrEv = do
  widgetHold_ blank $ appErrEv <&> \appErr -> do
    divClass "deployment__output notification notification--danger" $ do
      el "b" $ text "App error: "
      text appErr

-- | Widget with override fields. This widget supports adding and
-- a removing key-value pairs.
envVarsInput
  :: MonadWidget t m
  => Text -- ^ Widget header.
  -> m (Dynamic t [Override])
envVarsInput headerText = do
  elClass "section" "deployment__section" $ do
    elClass "h3" "deployment__sub-heading" $ text headerText
    elClass "div" "deployment__widget" $
      elClass "div" "overrides" $ mdo
        let
          emptyVar = Override "" "" Public
          addEv = clickEv $> Endo (\envs -> P.length envs =: emptyVar <> envs)
        envsDyn <- foldDyn appEndo mempty $ leftmost [ addEv, updEv ]
        (_, updEv)  <- runEventWriterT $ listWithKey envsDyn envVarInput
        let addDisabledDyn = all ( (/= "") . overrideKey ) . elems <$> envsDyn
        clickEv <- buttonClassEnabled'
          "overrides__add dash dash--add" "Add an override" addDisabledDyn
          "dash--disabled"
        pure $ elems <$> envsDyn

-- | Widget for a key-value pair. It returns an event carrying an update
-- of overrides list via 'EventWriter'.
envVarInput
  :: (EventWriter t (Endo (Map Int Override)) m, MonadWidget t m)
  => Int                -- ^ Index of variable in overrides list.
  -> Dynamic t Override -- ^ Current variable key and value.
  -> m ()
envVarInput ix _ = do
  divClass "overrides__item" $ do
    (keyDyn, _) <- octopodTextInput' "overrides__key" "key" Nothing never
    (valDyn, _) <- octopodTextInput' "overrides__value" "value" Nothing never
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let
      envEv = updated $ zipDynWith (\k v -> Override k v Public) keyDyn valDyn
      deleteEv = Endo (M.delete ix) <$ closeEv
      updEv = Endo . flip update ix . const . Just <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]
