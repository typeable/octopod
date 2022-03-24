module Data.Map.Config
  ( ConfigTree (..),
    null,
    filter,
    toList,
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
import Prelude hiding (filter, null)
import qualified Data.Bifunctor as Bi

data ConfigTree k v = ConfigTree
  { value :: Maybe v,
    subconfig :: OMap k (ConfigTree k v)
  }
  deriving stock (Eq, Ord, Show, Generic, Functor)
  deriving anyclass (FromJSON, ToJSON, NFData)

toList :: ConfigTree k v -> [([k], v)]
toList (ConfigTree v subconf) =
  ( case v of
    Nothing -> id
    Just v' -> (([], v'):)
  ) $ (>>= (\(k, s) -> Bi.first (k:) <$> toList s)) $ OM.assocs subconf
null :: ConfigTree l v -> Bool
null (ConfigTree Nothing m) = OM.null m
null _ = False

filter :: Ord k => (v -> Bool) -> ConfigTree k v -> ConfigTree k v
filter f (ConfigTree v m) =
  ConfigTree
    ( case v of
        Just v' | not $ f v' -> Just v'
        _ -> Nothing
    )
    (OM.filter (const null) . fmap (filter f) $ m)

instance Ord k => Semigroup (ConfigTree k v) where
  (ConfigTree lv lMap) <> (ConfigTree rv rMap) =
    ConfigTree (lv <|> rv) (OM.unionWithR (\_ -> (<>)) lMap rMap)

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
