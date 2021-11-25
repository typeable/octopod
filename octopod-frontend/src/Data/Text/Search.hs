{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module Data.Text.Search
  ( fuzzySearch,
    fuzzySearchMany,
    searchMany,
    wrapResult,
    search,
    deSearch,
    FuzzySearchStringChunk (..),
    SearchResult (..),
  )
where

import Control.Applicative.Free.Fast
import Control.Lens
import Control.Parallel.Strategies
import Control.Searchable
import qualified Data.List as L
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord
import Data.Semigroup
import Data.Sequence (Seq (..))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom
import Reflex.Dom.Renderable
import Text.FuzzyFind

type Needle = [String]
type Haystack = Text

fuzzySearch :: Needle -> Haystack -> Maybe Alignment
fuzzySearch [] h = Just (Alignment 0 $ Result $ pure $ Gap $ T.unpack h)
fuzzySearch n h = listToMaybe $ fuzzyFind n [T.unpack h]

fuzzySearchMany :: Needle -> [Haystack] -> [(Seq FuzzySearchStringChunk, Haystack)]
fuzzySearchMany n h =
  (\(unAlignment -> (res, _), initial) -> (res, T.pack initial)) <$> fuzzyFindOn id n (T.unpack <$> h)

searchMany ::
  (Searchable Text x, SearchableConstraint Text x SearchResult) =>
  [String] ->
  [x] ->
  [Searched x SearchResult]
searchMany [] = fmap wrapResult
searchMany t =
  fmap snd . L.sortOn (Down . fst) . catMaybes
    . withStrategy (parListChunk 3 rpar)
    . fmap (search t)
{-# INLINE searchMany #-}

-- | Extract initial structure from search result.
deSearch ::
  forall x.
  (Searchable SearchResult x, SearchableConstraint SearchResult x Text) =>
  x ->
  Searched x Text
deSearch = searchWith @SearchResult @x %~ initialSearchText
{-# INLINE deSearch #-}

-- | Like 'search', but doesn't search for anything. All 'Text' are marked as not matched.
wrapResult :: (Searchable Text x, SearchableConstraint Text x SearchResult) => x -> Searched x SearchResult
wrapResult =
  runIdentity . searchWith (\t -> Identity $ SearchResult t Nothing)
{-# INLINE wrapResult #-}

search ::
  (Searchable Text x, SearchableConstraint Text x SearchResult) =>
  Needle ->
  x ->
  Maybe (Int, Searched x SearchResult)
search needle x = runSearchApplicative $ searchWith searchSingle x
  where
    searchSingle :: Text -> SearchApplicative SearchResult
    searchSingle t = case fuzzySearch needle t of
      Nothing -> pure $ SearchResult t Nothing
      Just (unAlignment -> (res, score)) -> liftAp $ TextSearch t res score
{-# INLINE search #-}

unAlignment :: Alignment -> (Seq FuzzySearchStringChunk, Score)
unAlignment (Alignment score (Result res)) = (unResultSegment <$> res, score)

unResultSegment :: ResultSegment -> FuzzySearchStringChunk
unResultSegment (Gap x) = NotMatched $ T.pack x
unResultSegment (Match x) = Matched $ T.pack x

data SearchResult = SearchResult
  { initialSearchText :: !Text
  , searchResult :: !(Maybe (Seq FuzzySearchStringChunk))
  }
  deriving stock (Eq, Ord)

instance Searchable SearchResult SearchResult where
  type Searched SearchResult res = res
  searchWith f t = f t
  {-# INLINE searchWith #-}

instance Renderable SearchResult where
  rndr (SearchResult _ (Just cs)) = rndr cs
  rndr (SearchResult t Nothing) = rndr t
  {-# INLINE rndr #-}

data SearchF x where
  TextSearch :: Text -> Seq FuzzySearchStringChunk -> !Int -> SearchF SearchResult

type SearchApplicative = Ap SearchF

runSearchApplicative :: SearchApplicative x -> Maybe (Int, x)
runSearchApplicative x = case findMaxResult x of
  Nothing -> Nothing
  Just m ->
    Just . (m,) . runIdentity $
      runAp
        ( \case
            TextSearch t res i | i == m -> Identity $ SearchResult t (Just res)
            TextSearch t _ _ -> Identity $ SearchResult t Nothing
        )
        x
  where
    findMaxResult :: SearchApplicative a -> Maybe Int
    findMaxResult = fmap getMax . runAp_ (\(TextSearch _ _ i) -> Just $ Max i)
{-# INLINE runSearchApplicative #-}

data FuzzySearchStringChunk = NotMatched !Text | Matched !Text
  deriving stock (Show, Eq, Ord)

instance Renderable FuzzySearchStringChunk where
  rndr (NotMatched a) = rndr a
  rndr (Matched a) = elAttr "span" ("style" =: "text-decoration: underline;") (rndr a)
  {-# INLINE rndr #-}
