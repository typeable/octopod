module Data.WorkingOverrides
  ( WorkingOverrides,
    -- WorkingOverride,
    -- WorkingOverride',
    WorkingOverrideKey' (..),
    WorkingOverrideKey,
    WorkingOverrideKeyType (..),
    destructWorkingOverrides,
    constructWorkingOverrides,
    -- newWorkingOverride,
    ConfigValue (..),
    CustomKey (..),
    CustomConfigValue (..),
    HasTrie,
    WorkingConfigTree,
    WorkingOverride,
    WorkingOverride',
    configTreeHasLeaf,
    destructWorkingOverridesDyn,
  )
where

import Common.Orphans ()
import Common.Types
import qualified Data.Bifunctor as Bi
import Data.ConfigTree (ConfigTree)
import qualified Data.ConfigTree as CT
import Data.Functor
import Data.Maybe (catMaybes)
import qualified Data.Maybe as M
import Data.MemoTrie
import Data.Text (Text)
import Data.These
import GHC.Generics (Generic)
import Reflex

type WorkingConfigTree k v = ConfigTree k (ConfigValue v)

data ConfigValue te
  = DefaultConfigValue !te
  | CustomConfigValue !(Either (CustomKey te) (CustomConfigValue te))
  deriving stock (Show, Generic, Eq, Ord)

instance (HasTrie te) => HasTrie (ConfigValue te) where
  newtype ConfigValue te :->: b = ConfigValueTrie {unConfigValueTrie :: Reg (ConfigValue te) :->: b}
  trie = trieGeneric ConfigValueTrie
  untrie = untrieGeneric unConfigValueTrie
  enumerate = enumerateGeneric unConfigValueTrie

newtype CustomKey v = CustomKey v
  deriving stock (Show, Generic, Eq, Ord)

instance HasTrie te => HasTrie (CustomKey te) where
  newtype CustomKey te :->: b = CustomKeyTrie {unCustomKeyTrie :: Reg (CustomKey te) :->: b}
  trie = trieGeneric CustomKeyTrie
  untrie = untrieGeneric unCustomKeyTrie
  enumerate = enumerateGeneric unCustomKeyTrie

data CustomConfigValue te
  = CustomValue !te
  | DeletedValue !(Maybe te)
  deriving stock (Show, Generic, Eq, Ord)

instance (HasTrie te) => HasTrie (CustomConfigValue te) where
  newtype CustomConfigValue te :->: b = CustomConfigValueTrie {unCustomConfigValueTrie :: Reg (CustomConfigValue te) :->: b}
  trie = trieGeneric CustomConfigValueTrie
  untrie = untrieGeneric unCustomConfigValueTrie
  enumerate = enumerateGeneric unCustomConfigValueTrie

-- Super inefficient but i dont care
configTreeHasLeaf ::
  (te -> Bool) ->
  ConfigTree kv te ->
  Bool
configTreeHasLeaf f (CT.ConfigTree x) = flip any x $ \case
  (Just y, _) | f y -> True
  (_, ct) -> configTreeHasLeaf f ct

type WorkingOverrides = WorkingOverrides' Text

type WorkingOverrides' te = ConfigTree te (ConfigValue te)

type WorkingOverride = WorkingOverride' Text

type WorkingOverride' te = (te, ConfigValue te)

type WorkingOverrideKey = WorkingOverrideKey' Text

data WorkingOverrideKey' te = WorkingOverrideKey !WorkingOverrideKeyType !te
  deriving stock (Show)

data WorkingOverrideKeyType = CustomWorkingOverrideKey | DefaultWorkingOverrideKey
  deriving stock (Show, Eq)

destructWorkingOverridesDyn :: Reflex t => [Dynamic t WorkingOverride] -> Dynamic t (Overrides l)
destructWorkingOverridesDyn =
  fmap (Overrides . CT.fromFlatList . catMaybes) . sequenceA . (fmap . fmap) (\(k, v) -> (k,) <$> unConfigValue v)

destructWorkingOverrides :: [WorkingOverride] -> Overrides l
destructWorkingOverrides =
  Overrides . CT.fromFlatList . M.catMaybes . fmap (\(k, v) -> (k,) <$> unConfigValue v)

unConfigValue :: ConfigValue te -> Maybe (OverrideValue' te)
unConfigValue (DefaultConfigValue _) = Nothing
unConfigValue (CustomConfigValue (Left (CustomKey te))) = Just $ ValueAdded te
unConfigValue (CustomConfigValue (Right (CustomValue te))) = Just $ ValueAdded te
unConfigValue (CustomConfigValue (Right (DeletedValue _))) = Just ValueDeleted

constructWorkingOverrides ::
  Ord te =>
  Maybe (DefaultConfig' te l) ->
  Overrides' te l ->
  WorkingOverrides' te
constructWorkingOverrides Nothing (Overrides x) = go x
  where
    go :: ConfigTree _ (OverrideValue' _) -> _
    go (CT.ConfigTree ct) = CT.ConfigTree $ fmap (Bi.bimap (fmap mapValue) go) ct

    mapValue :: OverrideValue' t -> ConfigValue t
    mapValue (ValueAdded v) = CustomConfigValue (Right (CustomValue v))
    mapValue ValueDeleted = CustomConfigValue (Right (DeletedValue Nothing))
constructWorkingOverrides (Just (DefaultConfig defCT)) (Overrides newCT) =
  CT.catMaybes $
    CT.zip newCT defCT <&> \case
      That x -> Just $ DefaultConfigValue x
      This x -> case x of
        ValueDeleted -> Nothing
        ValueAdded v -> Just $ CustomConfigValue $ Left $ CustomKey v
      These ValueDeleted v -> Just $ CustomConfigValue $ Right $ DeletedValue $ Just v
      These (ValueAdded v) _ -> Just $ CustomConfigValue $ Right $ CustomValue v
