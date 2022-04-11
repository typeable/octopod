module Data.ConfigTree
  ( ConfigTree (..),
    null,
    filter,
    toList,
    singleton,
    fromList,
    lookup,
    member,
    deconstructConfigKey,
    reconstructConfigKey,
    toFlatList,
    fromFlatList,
    Data.ConfigTree.zip,
    catMaybes,
    insert,
    markValues,
  )
where

import Common.Orphans ()
import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Searchable
import Data.Aeson
import qualified Data.Bifunctor as Bi
import Data.Foldable (fold)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Ordered.Strict (OMap)
import qualified Data.Map.Ordered.Strict as OM
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.MemoTrie
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import GHC.Generics (Generic)
import Prelude hiding (filter, lookup, null)

zip :: Ord k => ConfigTree k v -> ConfigTree k h -> ConfigTree k (These v h)
zip (ConfigTree m) (ConfigTree w) =
  let intersection =
        OM.intersectionWith
          ( \_ (mv, vMap) (mh, hMap) ->
              ( case (mv, mh) of
                  (Nothing, Nothing) -> Nothing
                  (Just v, Nothing) -> Just $ This v
                  (Nothing, Just h) -> Just $ That h
                  (Just v, Just h) -> Just $ These v h
              , Data.ConfigTree.zip vMap hMap
              )
          )
          m
          w
      vCT = ConfigTree $ m OM.\\ intersection
      hCT = ConfigTree $ w OM.\\ intersection
   in fmap This vCT <> ConfigTree intersection <> fmap That hCT

newtype ConfigTree k v = ConfigTree (OMap k (Maybe v, ConfigTree k v))
  deriving newtype (Eq, Ord, Show, NFData)
  deriving stock (Functor, Generic)

instance (HasTrie k, HasTrie v) => HasTrie (ConfigTree k v) where
  newtype ConfigTree k v :->: b = ConfigTreeTrie {unConfigTreeTrie :: Reg (ConfigTree k v) :->: b}
  trie = trieGeneric ConfigTreeTrie
  untrie = untrieGeneric unConfigTreeTrie
  enumerate = enumerateGeneric unConfigTreeTrie

instance ToJSON v => ToJSON (ConfigTree Text v) where
  toJSON = toJSON . toFlatList

instance FromJSON v => FromJSON (ConfigTree Text v) where
  parseJSON = fmap fromFlatList . parseJSON

markValues :: (IsString t, Monoid t, Ord t) => ConfigTree t c -> ConfigTree t (t, c)
markValues = fromList . fmap (\(k, v) -> (k, (reconstructConfigKey k, v))) . toList
{-# INLINE markValues #-}

toFlatList :: (IsString t, Monoid t) => ConfigTree t c -> [(t, c)]
toFlatList = fmap (Bi.first reconstructConfigKey) . toList
{-# INLINE toFlatList #-}

fromFlatList :: [(Text, v)] -> ConfigTree Text v
fromFlatList = fromList . fmap (Bi.first deconstructConfigKey)
{-# INLINE fromFlatList #-}

toList :: ConfigTree k v -> [(NonEmpty k, v)]
toList (ConfigTree m) =
  OM.assocs m >>= \(k, (mv, s)) ->
    maybe id (\v -> (:) (pure k, v)) mv $ Bi.first (NE.cons k) <$> toList s
{-# INLINE toList #-}

fromList :: Ord k => [(NonEmpty k, v)] -> ConfigTree k v
fromList = foldMap $ uncurry singleton
{-# INLINE fromList #-}

singleton :: Ord k => NonEmpty k -> v -> ConfigTree k v
singleton (NE k) v = ConfigTree $ OM.singleton (k, (Just v, mempty))
singleton (NECons k ks) v = ConfigTree $ OM.singleton (k, (Nothing, singleton ks v))
{-# INLINE singleton #-}

lookup :: Ord k => NonEmpty k -> ConfigTree k v -> Maybe v
lookup (NE k) (ConfigTree m) = OM.lookup k m >>= fst
lookup (NECons k ks) (ConfigTree m) = OM.lookup k m >>= lookup ks . snd
{-# INLINE lookup #-}

member :: Ord k => NonEmpty k -> ConfigTree k v -> Bool
member (NE k) (ConfigTree m) = OM.member k m
member (NECons k ks) (ConfigTree m) = maybe False (member ks . snd) $ OM.lookup k m
{-# INLINE member #-}

null :: ConfigTree l v -> Bool
null (ConfigTree m) = OM.null m
{-# INLINE null #-}

filter :: Ord k => (v -> Bool) -> ConfigTree k v -> ConfigTree k v
filter f (ConfigTree m) =
  ConfigTree $
    OM.filter (\_ (mv, ConfigTree m') -> isJust mv || not (OM.null m'))
      . fmap (\(mv, ct) -> (mv >>= (\v -> if f v then Just v else Nothing), filter f ct))
      $ m
{-# INLINE filter #-}

instance Ord k => Semigroup (ConfigTree k v) where
  (ConfigTree lMap) <> (ConfigTree rMap) =
    ConfigTree $ OM.unionWithR (\_ (lvm, lm) (rvm, rm) -> (lvm <|> rvm, lm <> rm)) lMap rMap
  {-# INLINE (<>) #-}

instance Ord k => Monoid (ConfigTree k v) where
  mempty = ConfigTree OM.empty

instance (Searchable needle k, Searchable needle v) => Searchable needle (ConfigTree k v) where
  type
    SearchableConstraint needle (ConfigTree k v) res =
      (SearchableConstraint needle k res, SearchableConstraint needle v res, Ord (Searched k res))
  type Searched (ConfigTree k v) res = ConfigTree (Searched k res) (Searched v res)
  searchWith f (ConfigTree oMap) = do
    oMap' <- traverseOMap (searchWith f) (searchWith f) oMap
    pure $ ConfigTree oMap'
  {-# INLINE searchWith #-}

deconstructConfigKey :: Text -> NonEmpty Text
deconstructConfigKey =
  fromMaybe (pure "") . NE.nonEmpty . L.filter (not . T.null) . fmap T.strip . T.splitOn "."
{-# INLINE deconstructConfigKey #-}

reconstructConfigKey :: (IsString t, Monoid t) => NonEmpty t -> t
reconstructConfigKey = fold . NE.intersperse "."
{-# INLINE reconstructConfigKey #-}

pattern NE :: a -> NonEmpty a
pattern NE x = x NE.:| []
pattern NECons :: a -> NonEmpty a -> NonEmpty a
pattern NECons x y <- (NE.uncons -> (x, Just y))
{-# COMPLETE NE, NECons #-}

catMaybes :: Ord k => ConfigTree k (Maybe v) -> ConfigTree k v
catMaybes = fmap fromJust . filter isJust
{-# INLINE catMaybes #-}

insert :: Ord k => NonEmpty k -> v -> ConfigTree k v -> ConfigTree k v
insert (NE k) v (ConfigTree m) = ConfigTree $ (k, (Just v, subCfg)) OM.<| m
  where
    subCfg = case OM.lookup k m of
      Nothing -> mempty
      Just (_, m') -> m'
insert (NECons k ks) v (ConfigTree m) = ConfigTree $ case OM.lookup k m of
  Nothing -> (k, (Nothing, singleton ks v)) OM.<| m
  Just (v', m') -> (k, (v', insert ks v m')) OM.<| m
{-# INLINE insert #-}
