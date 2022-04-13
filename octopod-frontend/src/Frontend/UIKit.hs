-- | Most of HTML details should be implemented here with a domain API.
module Frontend.UIKit
  ( deploymentSection,
    loadingCommonWidget,
    errorCommonWidget,
    octopodTextInput',
    octopodTextInputDyn,
    loadingOverride,
    loadingOverrides,
    configField,
    popupOverlay,
    Default (..),
    module X,
    (.~~),
    (?~~),
    deletedOverride,
    untilReadyEv',
    untilReady',
    runWithReplace',
    joinEvM,
    showNonEditableWorkingOverrideTree,
    showFlatConfig,
    ConfigFieldButtonAction (..),
    showOverrideTree,
    nonEditableLoading,
  )
where

import Common.Types
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Ref
import Data.Align
import Data.ConfigTree (ConfigTree (ConfigTree))
import qualified Data.ConfigTree as CT
import Data.Default
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Generics.Labels ()
import Data.Generics.Sum
import Data.List.NonEmpty
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map.Ordered.Strict as OM
import Data.Maybe (fromMaybe, isNothing, maybeToList)
import Data.Monoid (Endo (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Search
import Data.These
import Data.Traversable
import Data.Witherable
import Data.WorkingOverrides
import Frontend.Classes as X
import Frontend.UIKit.Button.Action as X
import Frontend.UIKit.Button.Common
import Frontend.UIKit.Button.Dash as X
import Frontend.UIKit.Button.Expander as X
import Frontend.UIKit.Button.Large as X
import Frontend.UIKit.Button.Sort as X
import Frontend.UIKit.Button.Static as X
import Frontend.UIKit.Button.Tree
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

data ConfigField t = ConfigField
  { fieldValue :: Dynamic t Text
  , fieldError :: Dynamic t (Maybe (NonEmpty Text))
  , fieldType :: Dynamic t ConfigFieldType
  }
  deriving stock (Generic)

data ConfigFieldType
  = DefaultConfigFieldType
  | CustomConfigFieldType
  deriving stock (Show)

configField ::
  MonadWidget t m =>
  Dynamic t [Text] ->
  Dynamic t WorkingOverride ->
  m (Dynamic t Text, Dynamic t Text, Event t ConfigFieldButtonAction)
configField overrideKeyValues (splitDynPure -> (k, v')) = do
  let (splitDynPure -> (keyClass, valueClass)) =
        v' <&> \case
          DefaultConfigValue _ -> ("key-default-pristine", "value-pristine")
          CustomConfigValue (Right (CustomValue _)) -> ("key-default-edited", "value-edited")
          CustomConfigValue (Right (DeletedValue Just {})) -> ("key-deleted", "value-deleted")
          CustomConfigValue (Right (DeletedValue Nothing)) -> ("key-deleted", "value-unknown")
          CustomConfigValue (Left _) -> ("key-custom-edited", "value-edited")
      valDyn =
        v' <&> \case
          DefaultConfigValue v -> v
          CustomConfigValue (Right (CustomValue v)) -> v
          CustomConfigValue (Right (DeletedValue v)) -> fromMaybe "UNKNOWN" v
          CustomConfigValue (Left (CustomKey v)) -> v
      deletedDyn' = (is (_Ctor' @"CustomConfigValue" . _Right . _Ctor' @"DeletedValue")) <$> v'
  deletedDyn <- holdUniqDyn deletedDyn'
  let extraDivClasses =
        deletedDyn <&> \case
          True -> "input--deleted"
          False -> mempty
  elDiv "editable-row" $ mdo
    (value -> keyTextDyn) <-
      octopodTextInputDyn
        overrideKeyValues
        deletedDyn
        (mappend "editable-row__key" <$> extraDivClasses)
        keyClass
        "key"
        k
        (pure Nothing)
    (value -> valTextDyn) <-
      octopodTextInputDyn
        (pure [])
        deletedDyn
        (mappend "editable-row__value" <$> extraDivClasses)
        valueClass
        "value"
        valDyn
        (valTextDyn <&> \t -> if (T.null . T.strip) t then Just (pure "Value can not be empty") else Nothing)
    closeEv <-
      networkView >=> switchHold never $
        deletedDyn <&> \case
          True -> fmap (const ConfigFieldButtonRecover) <$> undoOverrideButton
          False -> fmap (const ConfigFieldButtonClear) <$> deleteOverrideButton
    pure (keyTextDyn, valTextDyn, closeEv)

data ConfigFieldButtonAction = ConfigFieldButtonClear | ConfigFieldButtonRecover
  deriving stock (Show)

deletedOverride :: MonadWidget t m => Text -> Maybe Text -> m (Event t ())
deletedOverride k vM = do
  elDiv "editable-row" $ do
    elDiv "editable-row__key input input--deleted" $ field k
    case vM of
      Nothing -> do
        elDiv "editable-row__placeholder editable-row__value" blank
        loadingOverrideSpinner
        pure never
      Just v -> do
        elDiv "editable-row__value input input--deleted" $ field $ v
        (($> ()) <$> undoOverrideButton)
  where
    field t =
      void $
        inputElement $
          def
            & inputElementConfig_initialValue .~ t
            & initialAttributes .~ ("type" =: "text" <> "class" =: "input__widget")

loadingOverrideField :: MonadWidget t m => m ()
loadingOverrideField = elDiv "editable-row__placeholder" blank

loadingOverrideSpinner :: MonadWidget t m => m ()
loadingOverrideSpinner = elDiv "overrides__delete spot spot--loader" blank

loadingOverride :: MonadWidget t m => m ()
loadingOverride = do
  elDiv "editable-row loader" $ do
    loadingOverrideField
    loadingOverrideField
    loadingOverrideSpinner

loadingOverrides :: MonadWidget t m => m ()
loadingOverrides = divClass "padded" $ do
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
  -- | Div classes.
  Dynamic t Classes ->
  -- | Input field classes.
  Dynamic t Classes ->
  -- | Placeholder for input field.
  Text ->
  -- | Init value.
  (Dynamic t Text) ->
  -- | Event carrying the error message.
  Event t Text ->
  m (InputElement EventResult GhcjsDomSpace t, Dynamic t Bool)
octopodTextInput' valuesDyn disabledDyn inpClassesDyn clssDyn placeholder inValDyn' errEv = mdo
  errDyn <-
    holdDyn Nothing $
      leftmost
        [ Just . pure <$> errEv
        , Nothing <$ updated (value inp)
        ]
  inp <- octopodTextInputDyn valuesDyn disabledDyn inpClassesDyn clssDyn placeholder inValDyn' errDyn
  pure (inp, isNothing <$> errDyn)

-- | The only text input field that is used in project forms. This input
-- provides automatic error message hiding after user starts typing.
octopodTextInputDyn ::
  MonadWidget t m =>
  -- | Value to suggest
  Dynamic t [Text] ->
  -- | Disabled?
  Dynamic t Bool ->
  -- | Div classes.
  Dynamic t Classes ->
  -- | Input field classes.
  Dynamic t Classes ->
  -- | Placeholder for input field.
  Text ->
  -- | Init value.
  Dynamic t Text ->
  -- | Event carrying the error message.
  Dynamic t (Maybe (NonEmpty Text)) ->
  m (InputElement EventResult GhcjsDomSpace t)
octopodTextInputDyn valuesDyn disabledDyn divClasses inpClassesDyn placeholder inValDyn' errDyn = mdo
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
        additionalClasses <- divClasses
        pure . destructClasses $ "input" <> errClasses <> additionalClasses

  inVal <- sample . current $ inValDyn
  disabled <- sample . current $ disabledDyn
  let actualInputClassesDyn = do
        inpClass <- inpClassesDyn
        pure $ destructClasses $ "input__widget" <> inpClass
  clss <- sample . current $ actualInputClassesDyn
  (inp, selectedValue) <- elDynClass "div" classDyn $ do
    inp' <-
      inputElement $
        def
          & initialAttributes
            .~ ( "type" =: "text"
                  <> "class" =: clss
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
                  classes <- actualInputClassesDyn
                  pure $
                    ( "disabled" =: (if disabled' then Just "disabled" else Nothing)
                        <> "class" =: Just classes
                    )
              )
    void $
      simpleList ((maybeToList >=> NE.toList) <$> errDyn) $ \err ->
        divClass "input__output" $ dynText err

    let searchValuesEv = catMaybes . updated $ do
          hasFocus <- _inputElement_hasFocus inp'
          values <- valuesDyn
          currVal <- valDyn
          pure $ case hasFocus of
            True | (_ : _) <- values -> Just (values, currVal)
            _ -> Nothing
    searchResultEv <- asyncEventLast searchValuesEv $ \(values, currVal) ->
      fuzzySearchMany [T.unpack currVal] values
    selectedValueEv <-
      (>>= joinEvM) $
        runWithReplace' (pure ()) $
          updated (_inputElement_hasFocus inp') <&> \case
            False -> pure never
            True ->
              (>>= joinEvM) $
                runWithReplace' (pure ()) $
                  searchResultEv <&> \case
                    [] -> pure never
                    searchResult -> do
                      elClass "ul" "input__dropdown" $ do
                        fmap leftmost $
                          for searchResult $ \(res, initialText) -> do
                            (resEl, ()) <- elClass' "li" "input__suggest" $
                              for_ res $ \case
                                Matched t -> elAttr "span" ("style" =: "font-weight: bold;") $ text t
                                NotMatched t -> text t
                            -- Because 'Click' would fire after the text field looses
                            -- focus and the popup disappears.
                            pure $ domEvent Mousedown resEl $> initialText

    pure (inp', selectedValueEv)
  pure inp

-- | Dark unclickable background for opened sidebar.
popupOverlay :: DomBuilder t m => m ()
popupOverlay =
  elAttr "div" ("class" =: "popup__overlay" <> "aria-hidden" =: "true") blank

showFlatConfig ::
  (MonadWidget t m, Renderable tk, Renderable tv) =>
  [(tk, OverrideValue' tv)] ->
  m ()
showFlatConfig l =
  for_ l $ \(k, v) ->
    let keyClasses = if is (_Ctor' @"ValueDeleted") v then "key-deleted" else "key-default-pristine"
     in divClass "row" $ do
          elClass "span" (destructClasses keyClasses) $ do
            rndr k
            text ": "
            pure ()
          case v of
            ValueAdded t -> elClass "span" "value-edited" $ rndr t
            ValueDeleted -> elClass "div" "listing__placeholder listing__placeholder__value" $ pure ()
          pure ()

showNonEditableWorkingOverrideTree ::
  (MonadWidget t m, Renderable tk, Renderable tv, Ord tk, Ord (ConfigValue tv)) =>
  Dynamic t (WorkingConfigTree tk tv) ->
  m ()
showNonEditableWorkingOverrideTree treeDyn = do
  memoRef <- newRef mempty
  flip runReaderT memoRef $
    untilReady (lift nonEditableLoading) $
      networkView $
        treeDyn <&> \tree ->
          untilReady (lift nonEditableLoading) $
            divClass "padded" $
              showWorkingOverrideTree'
                (is (_Ctor' @"CustomConfigValue"))
                (\_ _ -> ())
                (\_ k v -> renderRow k v $> pure ())
                tree
                (pure True)
                never
  pure ()

nonEditableLoading :: DomBuilder t m => m ()
nonEditableLoading = for_ [1 :: Int .. 3] $ \_ ->
  divClass "padded" $
    divClass "row" $ do
      elClass "div" "listing__placeholder" $ pure ()
      elClass "div" "listing__spinner" $ pure ()

showOverrideTree ::
  forall m t tv tk x.
  ( MonadWidget t m
  , Renderable tk
  , Show x
  , Ord tk
  , Ord tv
  ) =>
  -- | Value is modified
  (tv -> Bool) ->
  -- | Prepare the fixpoint data
  (tk -> tv -> x) ->
  -- | Render row
  (x -> tk -> tv -> m (Dynamic t x)) ->
  ConfigTree tk tv ->
  -- | Should we render anything
  m [Dynamic t x]
showOverrideTree isModified prepareData renderValue ct = do
  memoRef <- newRef mempty
  flip runReaderT memoRef $
    showWorkingOverrideTree' isModified prepareData ((fmap . fmap . fmap) lift renderValue) ct (pure True) never

showWorkingOverrideTree' ::
  ( MonadWidget t m
  , Renderable tk
  , Show x
  , MonadReader (Ref m (Map (ConfigTree tk tv) Bool)) m
  , Ord tv
  , Ord tk
  ) =>
  -- | Value is modified
  (tv -> Bool) ->
  -- | Prepare the fixpoint data
  (tk -> tv -> x) ->
  -- | Render row
  (x -> tk -> tv -> m (Dynamic t x)) ->
  ConfigTree tk tv ->
  -- | Parent is open
  Dynamic t Bool ->
  -- | Force open/close all subtrees
  Event t Bool ->
  m [Dynamic t x]
showWorkingOverrideTree' isModified prepareData renderValue (ConfigTree m) shouldRenderDyn' forceSubtreesEv = do
  shouldRenderDyn <- holdUniqDyn shouldRenderDyn'
  (appEndo . fold -> f) <- for (OM.assocs m) $ \(k, (mv, _)) -> do
    xMaybe <- for mv $ \v -> mdo
      let initialData = prepareData k v
      x <-
        networkView $
          shouldRenderDyn <&> \render -> do
            lastX <- sample $ current x'
            if render
              then elClass "div" "row" $ renderValue lastX k v
              else pure (pure lastX)
      x' <- (join <$> (holdDyn (pure initialData) x))
      pure $ x'
    pure $ Endo $ maybe id (:) xMaybe
  fmap (f . concat) $
    for (OM.assocs m) $ \case
      (_, (_, subtree)) | CT.null subtree -> pure []
      (k, (_, subtree)) -> mdo
        let wrapperClass = do
              open <- openDyn
              pure $
                destructClasses $
                  "collapse--project" <> "collapse"
                    <> if open
                      then "collapse--expanded"
                      else mempty
        (ovs, openDyn) <- elDynClass "div" wrapperClass $ do
          modified <- configTreeHasLeaf isModified subtree
          (traceEvent "clickEv" -> clickEv) <-
            treeButton
              TreeButtonConfig
                { buttonText = TextBuilder $ rndr k
                , subtreeHasChanges = modified
                , visible = shouldRenderDyn
                , forceState = forceSubtreesEv
                }
          buttonIsOpen <- holdDyn False $ leftmost [forceSubtreesEv, fst <$> clickEv]
          let selfForceSubtrees = fst <$> ffilter (isRight . snd) clickEv
          ovs' <-
            divClass "collapse__inner" $
              showWorkingOverrideTree'
                isModified
                prepareData
                renderValue
                subtree
                ((&&) <$> buttonIsOpen <*> shouldRenderDyn)
                (leftmost [selfForceSubtrees, forceSubtreesEv])
          pure (ovs', buttonIsOpen)
        pure ovs

renderRow ::
  (DomBuilder t m, Renderable te, Renderable k) =>
  k ->
  ConfigValue te ->
  m ()
renderRow k v = do
  case v of
    DefaultConfigValue v' -> do
      elClass "span" "key-default-pristine" $ do
        rndr k
        text ": "
        pure ()
      elClass "span" "value-pristine" $ rndr v'
      pure ()
    CustomConfigValue (Left (CustomKey v')) -> do
      elClass "span" "key-custom-edited" $ do
        rndr k
        text ": "
        pure ()
      elClass "span" "value-edited" $ rndr v'
      pure ()
    CustomConfigValue (Right (CustomValue v')) -> do
      elClass "span" "key-default-edited" $ do
        rndr k
        text ": "
        pure ()
      elClass "span" "value-edited" $ rndr v'
      pure ()
    CustomConfigValue (Right (DeletedValue mv)) -> do
      elClass "span" "key-deleted" $ do
        rndr k
        text ": "
        pure ()
      elClass "span" "value-deleted" $ rndr `traverse_` mv
      pure ()

untilReadyEv' ::
  (Adjustable t m, PostBuild t m, MonadHold t m) =>
  m a ->
  m (Event t b) ->
  m (Event t b)
untilReadyEv' m m' = do
  (_, bEvEv) <- untilReady m m'
  switchHold never bEvEv

untilReady' ::
  (Adjustable t m, PostBuild t m) =>
  m a ->
  m b ->
  m (Event t b)
untilReady' m m' = do
  (_, bEv) <- untilReady m m'
  pure bEv

runWithReplace' :: Adjustable t m => m a -> Event t (m b) -> m (Event t b)
runWithReplace' ma mbEv = do
  (_, bEv) <- runWithReplace ma mbEv
  pure bEv

joinEvM :: (MonadHold t m, Reflex t) => Event t (Event t a) -> m (Event t a)
joinEvM = switchHold never
