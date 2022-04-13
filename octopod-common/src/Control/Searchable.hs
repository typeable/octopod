{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Searchable
  ( Searchable (..),
    traverseOMap,
  )
where

import Control.Lens
import Data.Kind
import Data.Map.Ordered (OMap)
import qualified Data.Map.Ordered.Strict as OM
import Data.Text (Text)

-- | This says that you can search 'needle's in a 'haystack'. What precisely
-- "search for" means is defined by a function (the first argument) which
-- looks at every 'needle' and returns a "search result".
--
-- Essentially this is just a glorified 'Traversal' that can have extra
-- constraints and can leverage the constraint solver.
--
-- For a concrete "search" implementation see 'Data.Text.Search' from the
-- 'octopod-frontend' package.
class Searchable needle haystack where
  type SearchableConstraint needle haystack res :: Constraint
  type SearchableConstraint _ _ _ = ()
  type Searched haystack res
  searchWith ::
    SearchableConstraint needle haystack res =>
    Traversal haystack (Searched haystack res) needle res

instance Searchable Text Text where
  type Searched Text res = res
  searchWith f t = f t
  {-# INLINE searchWith #-}

instance Searchable needle x => Searchable needle [x] where
  type SearchableConstraint needle [x] res = SearchableConstraint needle x res
  type Searched [x] res = [Searched x res]
  searchWith f = traverse (searchWith f)
  {-# INLINE searchWith #-}

instance (Searchable needle a, Searchable needle b) => Searchable needle (a, b) where
  type
    SearchableConstraint needle (a, b) res =
      (SearchableConstraint needle a res, SearchableConstraint needle b res)
  type Searched (a, b) res = (Searched a res, Searched b res)
  searchWith f (a, b) = do
    a' <- searchWith f a
    b' <- searchWith f b
    pure (a', b')
  {-# INLINE searchWith #-}

instance Searchable needle haystack => Searchable needle (Maybe haystack) where
  type SearchableConstraint needle (Maybe haystack) res = SearchableConstraint needle haystack res
  type Searched (Maybe haystack) res = Maybe (Searched haystack res)
  searchWith _ Nothing = pure Nothing
  searchWith f (Just h) = Just <$> searchWith f h
  {-# INLINE searchWith #-}

traverseOMap ::
  forall h k x y.
  Ord k =>
  (forall f. Applicative f => (h -> f k) -> (x -> f y) -> OMap h x -> f (OMap k y))
traverseOMap kf vf (OM.assocs -> l) =
  fmap OM.fromList $ (\(h, x) -> (,) <$> kf h <*> vf x) `traverse` l
