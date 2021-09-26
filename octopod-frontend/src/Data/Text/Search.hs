{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

import Control.Applicative
import Control.Applicative.Free.Fast
import Control.Lens
import Control.Parallel.Strategies
import Control.Searchable
import Data.Bifunctor
import Data.Char
import Data.Function
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Ord
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom
import Reflex.Dom.Renderable

type Needle = Text
type Haystack = Text
type Running = Int
type Penalty = Int

fuzzySearch' :: Needle -> Haystack -> Running -> Penalty -> [([FuzzySearchStringChunk String], Int)]
fuzzySearch' needle haystack i p = case (T.uncons needle, T.uncons haystack) of
  (Just _, Nothing) -> []
  (Just (n, eedle), Just (h, aystack)) ->
    bimap (prependNotMatched h) (+ (i - p)) <$> fuzzySearch' needle aystack 0 p
      <|> if toLower n == toLower h
        then first (prependMatched h) <$> fuzzySearch' eedle aystack (i * 2 + 1) 1
        else empty
  (Nothing, Nothing) -> [([], i)]
  (Nothing, Just _) -> [([NotMatched $ T.unpack haystack], i)]

prependMatched :: Char -> [FuzzySearchStringChunk String] -> [FuzzySearchStringChunk String]
prependMatched c xs@(NotMatched _ : _) = Matched [c] : xs
prependMatched c (Matched cs : xs) = Matched (c : cs) : xs
prependMatched c [] = [Matched [c]]

prependNotMatched :: Char -> [FuzzySearchStringChunk String] -> [FuzzySearchStringChunk String]
prependNotMatched c xs@(Matched _ : _) = NotMatched [c] : xs
prependNotMatched c (NotMatched cs : xs) = NotMatched (c : cs) : xs
prependNotMatched c [] = [NotMatched [c]]

fuzzySearch :: Needle -> Haystack -> Maybe ([FuzzySearchStringChunk Text], Int)
fuzzySearch "" h = Just ([NotMatched h], 0)
fuzzySearch n h = case fuzzySearch' n h 0 0 of
  [] -> Nothing
  xs@(_ : _) -> Just . (first . fmap . fmap) T.pack $ L.maximumBy (compare `on` snd) xs

fuzzySearchMany :: Needle -> [Haystack] -> [(Haystack, [FuzzySearchStringChunk Text])]
fuzzySearchMany needle haystacks =
  fmap fst . L.sortOn (Down . snd) $
    mapMaybe
      ( \haystack ->
          fuzzySearch needle haystack
            <&> \(res, score) -> ((haystack, res), score)
      )
      haystacks

searchMany ::
  (Searchable Text x, SearchableConstraint Text x SearchResult) =>
  Text ->
  [x] ->
  [Searched x SearchResult]
searchMany "" = fmap wrapResult
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
  Text ->
  x ->
  Maybe (Int, Searched x SearchResult)
search needle x = runSearchApplicative $ searchWith searchSingle x
  where
    searchSingle :: Text -> SearchApplicative SearchResult
    searchSingle t = case fuzzySearch needle t of
      Nothing -> pure $ SearchResult t Nothing
      Just (res, score) -> liftAp $ TextSearch t res score
{-# INLINE search #-}

data SearchResult = SearchResult
  { initialSearchText :: !Text
  , searchResult :: !(Maybe [FuzzySearchStringChunk Text])
  }
  deriving stock (Eq, Ord)

instance Searchable SearchResult SearchResult where
  type Searched SearchResult res = res
  searchWith f t = f t
  {-# INLINE searchWith #-}

instance Renderable SearchResult where
  rndr (SearchResult _ (Just cs)) = rndr cs
  rndr (SearchResult t Nothing) = rndr t

data SearchF x where
  TextSearch :: Text -> [FuzzySearchStringChunk Text] -> !Int -> SearchF SearchResult

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

data FuzzySearchStringChunk a = NotMatched !a | Matched !a
  deriving stock (Show, Eq, Ord, Functor)

instance Renderable a => Renderable (FuzzySearchStringChunk a) where
  rndr (NotMatched a) = rndr a
  rndr (Matched a) = elAttr "span" ("style" =: "text-decoration: underline;") (rndr a)
