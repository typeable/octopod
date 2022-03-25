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
  )
where

import Common.Orphans ()
import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Searchable
import Data.Aeson
import Data.Map.Ordered.Strict (OMap)
import qualified Data.Map.Ordered.Strict as OM
import Data.Map.Ordered.Strict.Extra ()
import GHC.Generics (Generic)
import Prelude hiding (filter, null, lookup)
import qualified Data.Bifunctor as Bi
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L

data ConfigTree k v = ConfigTree
  { value :: Maybe v,
    subconfig :: OMap k (ConfigTree k v)
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)
  deriving anyclass (NFData)

instance ToJSON v => ToJSON (ConfigTree Text v) where
  toJSON = toJSON . toFlatList

instance FromJSON v => FromJSON (ConfigTree Text v) where
  parseJSON = fmap fromFlatList . parseJSON

toFlatList :: ConfigTree Text c -> [(Text, c)]
toFlatList = fmap (Bi.first reconstructConfigKey) . toList
{-# INLINE toFlatList #-}

fromFlatList :: [(Text, v)] -> ConfigTree Text v
fromFlatList = fromList . fmap (Bi.first deconstructConfigKey)
{-# INLINE fromFlatList #-}

toList :: ConfigTree k v -> [([k], v)]
toList (ConfigTree v subconf) =
  ( case v of
    Nothing -> id
    Just v' -> (([], v'):)
  ) $ (>>= (\(k, s) -> Bi.first (k:) <$> toList s)) $ OM.assocs subconf
{-# INLINE toList #-}

fromList :: Ord k => [([k], v)] -> ConfigTree k v
fromList = foldMap $ uncurry singleton
{-# INLINE fromList #-}

singleton :: Ord k => [k] -> v -> ConfigTree k v
singleton [] v = ConfigTree (Just v) OM.empty
singleton (k : ks) v = ConfigTree Nothing $ OM.singleton (k, singleton ks v)
{-# INLINE singleton #-}

lookup :: Ord k => [k] -> ConfigTree k v -> Maybe v
lookup [] (ConfigTree v _) = v
lookup (k : ks) (ConfigTree _ m) = OM.lookup k m >>= lookup ks
{-# INLINE lookup #-}

member :: Ord k => [k] -> ConfigTree k v -> Bool
member [] _ = True
member (k : ks) (ConfigTree _ m) = maybe False (member ks) $ OM.lookup k m
{-# INLINE member #-}

null :: ConfigTree l v -> Bool
null (ConfigTree Nothing m) = OM.null m
null _ = False
{-# INLINE null #-}

filter :: Ord k => (v -> Bool) -> ConfigTree k v -> ConfigTree k v
filter f (ConfigTree v m) =
  ConfigTree
    ( case v of
        Just v' | not $ f v' -> Just v'
        _ -> Nothing
    )
    (OM.filter (const null) . fmap (filter f) $ m)
{-# INLINE filter #-}

instance Ord k => Semigroup (ConfigTree k v) where
  (ConfigTree lv lMap) <> (ConfigTree rv rMap) =
    ConfigTree (lv <|> rv) (OM.unionWithR (\_ -> (<>)) lMap rMap)
  {-# INLINE (<>) #-}

instance Ord k => Monoid (ConfigTree k v) where
  mempty = ConfigTree Nothing OM.empty

instance (Searchable needle k, Searchable needle v) => Searchable needle (ConfigTree k v) where
  type
    SearchableConstraint needle (ConfigTree k v) res =
      (SearchableConstraint needle k res, SearchableConstraint needle v res, Ord (Searched k res))
  type Searched (ConfigTree k v) res = ConfigTree (Searched k res) (Searched v res)
  searchWith f (ConfigTree v oMap) = do
    v' <- searchWith f v
    oMap' <- traverseOMap (searchWith f) (searchWith f) oMap
    pure $ ConfigTree v' oMap'
  {-# INLINE searchWith #-}

deconstructConfigKey :: Text -> [Text]
deconstructConfigKey = L.filter (not . T.null) . fmap T.strip . T.splitOn "."
{-# INLINE deconstructConfigKey #-}

reconstructConfigKey :: [Text] -> Text
reconstructConfigKey = T.concat . L.foldr (\t acc -> t : T.singleton '.' : acc) []
{-# INLINE reconstructConfigKey #-}
