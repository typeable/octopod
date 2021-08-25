-- |
--Module      : Page.Popup.EditDeployment
--Description : Edit deployment sidebar.
--
--This module contains the definition of the "edit deployment" sidebar.
module Page.Popup.EditDeployment (editDeploymentPopup) where

import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Functor
import Data.Generics.Sum
import Data.Monoid
import Reflex.Dom as R hiding (mapMaybe)
import Prelude as P

import Common.Types
import Common.Utils
import Frontend.API
import Frontend.Utils
import Servant.Reflex

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
editDeploymentPopupBody dfi errEv = mdo
  errorHeader err
  (du, valid, err) <-
    divClass "popup__content" $
      divClass "deployment" $
        deploymentPopupBody
          (dfi ^. #deployment . #tag . coerced . to Just)
          (dfi ^. #deployment . #appOverrides)
          (dfi ^. #deployment . #deploymentOverrides)
          errEv
  pure (du, valid)
