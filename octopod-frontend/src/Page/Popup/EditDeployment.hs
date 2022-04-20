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
import Data.Generics.Labels ()
import Data.Maybe
import Frontend.API
import Frontend.UIKit
import Frontend.Utils
import Reflex.Network
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
editDeploymentPopup showEv hideEv = catchReturns $ \enterEv -> sidebar showEv hideEv $ \dfi -> mdo
  divClass "popup__body" $ mdo
    let dname = dfi ^. dfiName
    (closeEv', (enterEv <>) -> saveEv) <- editDeploymentPopupHeader dname enabledDyn sentDyn
    deploymentMDyn <- editDeploymentPopupBody dfi respEv
    respEv <-
      holdDyn (pure never) >=> networkView >=> switchHold never $
        tagMaybe (current deploymentMDyn) saveEv <&> \dep -> do
          pb <- getPostBuild
          updateEndpoint
            (constDyn $ Right dname)
            (pure $ Right dep)
            pb
    sentDyn <-
      holdDyn False $
        leftmost
          [ True <$ saveEv
          , False <$ respEv
          ]
    let successEv =
          fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
        closeEv = leftmost [closeEv', successEv]
        enabledDyn = zipDynWith (&&) (not <$> sentDyn) (isJust <$> deploymentMDyn)
    pure (updated sentDyn, closeEv)

-- | The header of the sidebar contains the deployment name and control buttons:
-- \"Save\" and \"Close\".
editDeploymentPopupHeader ::
  MonadWidget t m =>
  -- | Name of the deployment.
  DeploymentName ->
  -- | Form validation state.
  Dynamic t Bool ->
  -- | Loading
  Dynamic t Bool ->
  -- | \"Close\" event and \"Save\" click event.
  m (Event t (), Event t ())
editDeploymentPopupHeader dname validDyn loadingDyn =
  divClass "popup__head" $ do
    closeEv <- closePopupButton
    elClass "h2" "popup__project" $ text $ "Edit " <> coerce dname
    saveEv <-
      divClass "popup__operations" $
        largeButton $
          def
            & #buttonStyle .~~ PopupActionLargeButtonStyle
            & #buttonText .~~ "Save"
            & #buttonEnabled .~~ validDyn
            & #buttonType
              .~~ ( loadingDyn <&> \case
                      False -> Just SaveLargeButtonType
                      True -> Just LoadingLargeButtonType
                  )

    divClass "popup__menu drop drop--actions" blank
    pure (closeEv $> (), saveEv $> ())

-- | The body of the sidebar containing the edit form. Contains a tag field and
-- an override field.
editDeploymentPopupBody ::
  MonadWidget t m =>
  -- | Full deployment data.
  DeploymentFullInfo ->
  -- | \"Edit request\" failure event.
  Event t (ReqResult tag CommandResponse) ->
  -- | Returns deployment update and validation state.
  m (Dynamic t (Maybe DeploymentUpdate))
editDeploymentPopupBody dfi errEv = do
  divClass "popup__content" $
    divClass "deployment" $
      wrapRequestErrors $ \hReq ->
        deploymentPopupBody
          hReq
          (dfi ^. #deployment . #appOverrides)
          (dfi ^. #deployment . #deploymentOverrides)
          errEv
