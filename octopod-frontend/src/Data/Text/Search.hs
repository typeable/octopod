module Data.Text.Search
  ( fuzzySearch,
    FuzzySearchStringChunk (..),
  )
where

import Control.Applicative
import Data.Bifunctor
import Data.Char
import Data.Function
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

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

data FuzzySearchStringChunk a = NotMatched !a | Matched !a
  deriving stock (Show, Eq, Functor)
