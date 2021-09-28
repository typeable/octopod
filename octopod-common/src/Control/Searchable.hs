{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Searchable
  ( Searchable (..),
  )
where

import Control.Lens
import Data.Kind
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
  type SearchableConstraint needle (a, b) res = (SearchableConstraint needle a res, SearchableConstraint needle b res)
  type Searched (a, b) res = (Searched a res, Searched b res)
  searchWith f (a, b) = do
    a' <- searchWith f a
    b' <- searchWith f b
    pure (a', b')
  {-# INLINE searchWith #-}
