-- | Most of HTML details should be implemented here with a domain API.
module Frontend.UIKit
  ( deploymentSection,
    loadingCommonWidget,
    errorCommonWidget,
    octopodTextInput',
    octopodTextInputDyn,
    loadingOverride,
    loadingOverrides,
    overrideField,
    OverrideField (..),
    popupOverlay,
    Default (..),
    module X,
    (.~~),
    (?~~),
    OverrideFieldType (..),
    deletedOverride,
    showNonEditableWorkingOverride,
    NonEditableWorkingOverrideStyle (..),
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Align
import Data.Default
import Data.Functor
import Data.Generics.Labels ()
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import Data.Maybe (isNothing, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Search
import Data.These
import Data.Witherable
import Data.WorkingOverrides
import Frontend.Classes as X
import Frontend.UIKit.Button.Action as X
import Frontend.UIKit.Button.Dash as X
import Frontend.UIKit.Button.Expander as X
import Frontend.UIKit.Button.Large as X
import Frontend.UIKit.Button.Sort as X
import Frontend.UIKit.Button.Static as X
import GHC.Generics (Generic)
import Reflex.Dom
import Reflex.Dom.AsyncEvent
import Reflex.Dom.Renderable
import Reflex.Network

(.~~) :: ASetter' s a -> a -> s -> s
(.~~) = (.~)

(?~~) :: ASetter' s (Maybe a) -> a -> s -> s
(?~~) = (?~)

deploymentSection :: DomBuilder t m => Text -> m a -> m a
deploymentSection n m = elClass "section" "deployment__section" $ do
  elClass "h3" "deployment__sub-heading" $ text n
  elDiv "deployment__widget" m

-- | Widget with a loading spinner.
loadingCommonWidget :: MonadWidget t m => m ()
loadingCommonWidget =
  divClass "loading loading--enlarged loading--alternate" $
    text "Loading..."

-- | Widget with an error message.
errorCommonWidget :: MonadWidget t m => m ()
errorCommonWidget =
  divClass "null null--data" $
    divClass "null__content" $ do
      elClass "b" "null__heading" $ text "Cannot retrieve the data"
      divClass "null__message" $ text "Try to reload the page"

data OverrideField t = OverrideField
  { fieldValue :: Dynamic t Text
  , fieldError :: Dynamic t (Maybe (NonEmpty Text))
  , fieldDisabled :: Dynamic t Bool
  , fieldType :: Dynamic t OverrideFieldType
  }
  deriving stock (Generic)

data OverrideFieldType
  = DefaultOverrideFieldType
  | EditedOverrideFieldType
  deriving stock (Show)

overrideFieldTypeClasses :: OverrideFieldType -> Classes
overrideFieldTypeClasses DefaultOverrideFieldType = "input--default"
overrideFieldTypeClasses EditedOverrideFieldType = mempty

overrideField :: MonadWidget t m => Dynamic t [Text] -> OverrideField t -> OverrideField t -> m (Dynamic t Text, Dynamic t Text, Event t ())
overrideField overrideKeyValues keyDyn valueDyn = do
  elDiv "overrides__item" $ do
    keyInp <-
      octopodTextInputDyn
        overrideKeyValues
        (keyDyn ^. #fieldDisabled)
        ( do
            t <- keyDyn ^. #fieldType
            pure $ "overrides__key" <> overrideFieldTypeClasses t
        )
        "key"
        (keyDyn ^. #fieldValue)
        (keyDyn ^. #fieldError)
    let keyTextDyn = value keyInp
    (value -> valTextDyn) <-
      octopodTextInputDyn
        (pure [])
        (valueDyn ^. #fieldDisabled)
        ( do
            t <- valueDyn ^. #fieldType
            pure $ "overrides__value" <> overrideFieldTypeClasses t
        )
        "value"
        (valueDyn ^. #fieldValue)
        (valueDyn ^. #fieldError)
    closeEv <- deleteOverrideButton
    pure (keyTextDyn, valTextDyn, closeEv)

deletedOverride :: MonadWidget t m => Text -> Maybe Text -> m (Event t ())
deletedOverride k vM = do
  elDiv "overrides__item" $ do
    elDiv "overrides__key input input--deleted" $ field k
    case vM of
      Nothing -> do
        elDiv "overrides__placeholder overrides__value" blank
        loadingOverrideSpinner
        pure never
      Just v -> do
        elDiv "overrides__value input input--deleted" $ field $ v
        undoOverrideButton
  where
    field t =
      void $
        inputElement $
          def
            & inputElementConfig_initialValue .~ t
            & initialAttributes .~ ("type" =: "text" <> "class" =: "input__widget")

loadingOverrideField :: MonadWidget t m => m ()
loadingOverrideField = elDiv "overrides__placeholder" blank

loadingOverrideSpinner :: MonadWidget t m => m ()
loadingOverrideSpinner = elDiv "overrides__delete spot spot--loader" blank

loadingOverride :: MonadWidget t m => m ()
loadingOverride = do
  elDiv "overrides__item loader" $ do
    loadingOverrideField
    loadingOverrideField
    loadingOverrideSpinner

loadingOverrides :: MonadWidget t m => m ()
loadingOverrides = do
  loadingOverride
  loadingOverride
  loadingOverride

elDiv :: DomBuilder t m => Text -> m a -> m a
elDiv = elClass "div"

-- | The only text input field that is used in project forms. This input
-- provides automatic error message hiding after user starts typing.
octopodTextInput' ::
  MonadWidget t m =>
  -- | Value to suggest
  (Dynamic t [Text]) ->
  -- | Disabled?
  Dynamic t Bool ->
  -- | Input field classes.
  Dynamic t Classes ->
  -- | Placeholder for input field.
  Text ->
  -- | Init value.
  (Dynamic t Text) ->
  -- | Event carrying the error message.
  Event t Text ->
  m (InputElement EventResult GhcjsDomSpace t, Dynamic t Bool)
octopodTextInput' valuesDyn disabledDyn clssDyn placeholder inValDyn' errEv = mdo
  errDyn <-
    holdDyn Nothing $
      leftmost
        [ Just . pure <$> errEv
        , Nothing <$ updated (value inp)
        ]
  inp <- octopodTextInputDyn valuesDyn disabledDyn clssDyn placeholder inValDyn' errDyn
  pure (inp, isNothing <$> errDyn)

-- | The only text input field that is used in project forms. This input
-- provides automatic error message hiding after user starts typing.
octopodTextInputDyn ::
  MonadWidget t m =>
  -- | Value to suggest
  (Dynamic t [Text]) ->
  -- | Disabled?
  Dynamic t Bool ->
  -- | Input field classes.
  Dynamic t Classes ->
  -- | Placeholder for input field.
  Text ->
  -- | Init value.
  (Dynamic t Text) ->
  -- | Event carrying the error message.
  Dynamic t (Maybe (NonEmpty Text)) ->
  m (InputElement EventResult GhcjsDomSpace t)
octopodTextInputDyn valuesDyn disabledDyn clssDyn placeholder inValDyn' errDyn = mdo
  inValDyn <- holdUniqDyn inValDyn'
  let valDyn = value inp
      inValEv =
        align (leftmost [selectedValue, updated inValDyn]) (updated valDyn)
          & fmapMaybe
            ( \case
                This x -> Just x
                These inV currV | inV /= currV -> Just inV
                _ -> Nothing
            )

  let errorClassesDyn = do
        err <- errDyn
        case err of
          Just _ -> "input--error"
          Nothing -> mempty

  let classDyn = do
        errClasses <- errorClassesDyn
        additionalClasses <- clssDyn
        pure . destructClasses $ "input" <> errClasses <> additionalClasses

  inVal <- sample . current $ inValDyn
  disabled <- sample . current $ disabledDyn
  (inp, selectedValue) <- elDynClass "div" classDyn $ do
    inp' <-
      inputElement $
        def
          & initialAttributes
            .~ ( "type" =: "text"
                  <> "class" =: "input__widget"
                  <> "placeholder" =: placeholder
                  <> "spellcheck" =: "false"
               )
          & inputElementConfig_setValue .~ inValEv
          & inputElementConfig_initialValue .~ inVal
          & inputElementConfig_elementConfig . elementConfig_initialAttributes
            %~ (if disabled then M.insert "disabled" "disabled" else id)
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes
            <>~ updated
              ( do
                  disabled' <- disabledDyn
                  pure $
                    M.singleton "disabled" $
                      if disabled' then Just "disabled" else Nothing
              )
    void $
      simpleList ((maybeToList >=> NE.toList) <$> errDyn) $ \err ->
        divClass "input__output" $ dynText err

    delayedFalseFocus <- delayFalse $ _inputElement_hasFocus inp'
    let searchValuesEv = catMaybes . updated $ do
          hasFocus <- delayedFalseFocus
          values <- valuesDyn
          currVal <- valDyn
          pure $ case hasFocus of
            True | (_ : _) <- values -> Just (values, currVal)
            _ -> Nothing
    searchResultEv <- asyncEventLast searchValuesEv $ \(values, currVal) ->
      fuzzySearchMany [T.unpack currVal] values
    searchResultDyn <- holdDyn [] searchResultEv
    selectedValue' <- networkView >=> switchHoldPromptly never $ do
      searchResult <- searchResultDyn
      case searchResult of
        [] -> pure $ pure never
        ress ->
          pure $ do
            elClass "ul" "overrides__search" $ do
              fmap leftmost $
                forM ress $ \(res, initialText) -> do
                  (resEl, ()) <- elClass' "li" "overrides__search-item" $
                    forM_ res $ \case
                      Matched t -> elAttr "span" ("style" =: "font-weight: bold;") $ text t
                      NotMatched t -> text t
                  pure $ domEvent Click resEl $> initialText
    pure (inp', selectedValue')
  pure inp

delayFalse :: (MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m)) => Dynamic t Bool -> m (Dynamic t Bool)
delayFalse x = do
  initVal <- sample . current $ x
  let trueEv = ffilter id $ updated x
      falseEv = ffilter not $ updated x
  delayedFalseEv <- delay 0.1 falseEv
  holdDyn initVal $ leftmost [trueEv, delayedFalseEv]

-- | Dark unclickable background for opened sidebar.
popupOverlay :: DomBuilder t m => m ()
popupOverlay =
  elAttr "div" ("class" =: "popup__overlay" <> "aria-hidden" =: "true") blank

data NonEditableWorkingOverrideStyle
  = RegularNonEditableWorkingOverrideStyle
  | LargeNonEditableWorkingOverrideStyle

nonEditableWorkingOverrideStyleClasses :: NonEditableWorkingOverrideStyle -> Classes
nonEditableWorkingOverrideStyleClasses RegularNonEditableWorkingOverrideStyle = mempty
nonEditableWorkingOverrideStyleClasses LargeNonEditableWorkingOverrideStyle = "listing--larger"

-- | Widget that shows overrides list. It does not depend on their type.
showNonEditableWorkingOverride ::
  (MonadWidget t m, Renderable te) =>
  -- | Loading?
  Bool ->
  -- | Is it fully loaded?
  Bool ->
  NonEditableWorkingOverrideStyle ->
  -- | Overrides list.
  [WorkingOverride' te] ->
  m ()
showNonEditableWorkingOverride loading loaded style cfg =
  divClass
    ( destructClasses $
        "listing" <> "listing--for-text" <> nonEditableWorkingOverrideStyleClasses style
    )
    $ do
      case cfg of
        [] ->
          divClass "listing__item" $
            elClass "span" "listing--info-text" $
              text $
                if loaded then "no configuration" else "no custom configuration"
        _ -> forM_ cfg $ \(WorkingOverrideKey keyType key, val) -> do
          let wrapper = case val of
                WorkingDeletedValue _ -> divClass "listing__item deleted"
                _ -> divClass "listing__item"
          wrapper $ do
            let keyWrapper = case keyType of
                  CustomWorkingOverrideKey -> elClass "span" "listing__key"
                  DefaultWorkingOverrideKey -> elClass "span" "listing__key default"
            keyWrapper $ do
              rndr key
              text ": "

            case val of
              WorkingCustomValue txt -> elClass "span" "listing__value" $ rndr txt
              WorkingDefaultValue txt -> elClass "span" "listing__value default" $ rndr txt
              WorkingDeletedValue (Just txt) -> elClass "span" "listing__value default" $ rndr txt
              WorkingDeletedValue Nothing -> do
                elClass "div" "listing__placeholder listing__placeholder__value" $ pure ()
                when loading $ elClass "div" "listing__spinner" $ pure ()
      when loading $
        divClass "listing__item" $ do
          elClass "div" "listing__placeholder" $ pure ()
          elClass "div" "listing__spinner" $ pure ()
