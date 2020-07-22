{-|
Module      : Page.Popup.NewStaging
Description : New staging sidebar.

This module contains definition of @new staging@ sidebar.
-}


module Page.Popup.NewStaging (newStagingPopup) where

import Control.Lens (preview, _1, _2)
import Control.Monad
import Data.Coerce
import Data.Functor
import Data.Generics.Sum
import Data.Map as M
import Data.Monoid
import Data.Text as T (Text, intercalate)
import Prelude as P
import Reflex.Dom as R

import Common.Types
import Common.Validation (isNameValid)
import Frontend.API
import Frontend.Utils
import Servant.Reflex


-- | The root function for @new staging@ sidebar.
newStagingPopup
  :: MonadWidget t m
  => Event t ()     -- ^ @Show@ event.
  -> Event t ()     -- ^ @Close@ event.
  -> m ()
newStagingPopup showEv hideEv = void $ sidebar showEv hideEv $ const $ mdo
  divClass "popup__body" $ mdo
    (closeEv', saveEv) <- newStagingPopupHeader enabledDyn
    (deploymentDyn, validDyn) <- newStagingPopupBody respEv
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

-- | The header of sidebar contains control buttons: @Save@ and @Close@.
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

-- | The body of sidebar contains creation form. There are name field, tag field
-- and overrides fields. Name field is validated by next regexp:
-- @^[a-z][a-z0-9\\-]{1,16}$@.
newStagingPopupBody
  :: MonadWidget t m
  => Event t (ReqResult tag CommandResponse)
  -- ^ Request failure event.
  -> m (Dynamic t Deployment, Dynamic t Bool)
  -- ^ Returns new deployment and validation state.
newStagingPopupBody errEv = divClass "popup__content" $
  divClass "staging" $ mdo
    let
      commandResponseEv = fmapMaybe commandResponse errEv
      appErrEv = R.difference (fmapMaybe reqFailure errEv) commandResponseEv
      nameErrEv = getNameError commandResponseEv nameDyn
      tagErrEv = getTagError commandResponseEv tagDyn
    errorHeader appErrEv
    (nameDyn, nOkDyn) <- dmTextInput "tag" "Name" "Name" Nothing nameErrEv
    (tagDyn, tOkDyn) <- dmTextInput "tag" "Tag" "Tag" Nothing tagErrEv
    appVarsDyn <- envVarsInput "App overrides"
    stagingVarsDyn <- envVarsInput "Staging overrides"
    validDyn <- holdDyn False $ updated $ zipDynWith (&&) nOkDyn tOkDyn
    pure $ (Deployment
      <$> (DeploymentName <$> nameDyn)
      <*> (DeploymentTag <$> tagDyn)
      <*> (coerce <$> appVarsDyn)
      <*> (coerce <$> stagingVarsDyn), validDyn)
  where
    getNameError crEv nameDyn = let
      nameErrEv' = fmapMaybe (preview (_Ctor @"ValidationError" . _1 )) crEv
      isNameValidDyn = isNameValid . DeploymentName <$> nameDyn
      badNameText = "Staging name length should be longer than 2 characters \
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

-- | The error widget in case of request failure.
errorHeader
  :: MonadWidget t m
  => Event t Text -- ^ Message text.
  -> m ()
errorHeader appErrEv = do
  widgetHold_ blank $ appErrEv <&> \appErr -> do
    divClass "staging__output notification notification--danger" $ do
      el "b" $ text "App error: "
      text appErr

-- | Widget with fields for overrides. This widget supports an adding and
-- a removing key-value pairs.
envVarsInput
  :: MonadWidget t m
  => Text -- ^ Widget header.
  -> m (Dynamic t [Override])
envVarsInput headerText = do
  elClass "section" "staging__section" $ do
    elClass "h3" "staging__sub-heading" $ text headerText
    elClass "div" "staging__widget" $
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

-- | Widget for variable key and value. It returns an event carrying an update
-- of overrides list via 'EventWriter'.
envVarInput
  :: (EventWriter t (Endo (Map Int Override)) m, MonadWidget t m)
  => Int                -- ^ Index of variable in overrides list.
  -> Dynamic t Override -- ^ Current variable key and value.
  -> m ()
envVarInput ix _ = do
  divClass "overrides__item" $ do
    (keyDyn, _) <- dmTextInput' "overrides__key" "key" Nothing never
    (valDyn, _) <- dmTextInput' "overrides__value" "value" Nothing never
    closeEv <- buttonClass "overrides__delete spot spot--cancel" "Delete"
    let
      envEv = updated $ zipDynWith (\k v -> Override k v Public) keyDyn valDyn
      deleteEv = Endo (M.delete ix) <$ closeEv
      updEv = Endo . flip update ix . const . Just <$> envEv
    tellEvent $ leftmost [deleteEv, updEv]

