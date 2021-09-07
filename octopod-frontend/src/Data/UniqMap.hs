module Data.UniqMap
  ( UniqKeyMap,
    uniqMap,
    insertUniqEnd,
    insertUniqStart,
    deleteUniq,
    updateUniq,
    elemsUniq,
    emptyUniqKeyMap,
    uniqMapFromList,
    lookupUniq,
  )
where

import qualified Data.List as L
import Data.Map.Strict as M

data UniqKeyMap v = UniqKeyMap !(Map Int v) !Int !Int
  deriving stock (Show)

lookupUniq :: Int -> UniqKeyMap v -> Maybe v
lookupUniq k (UniqKeyMap m _ _) = M.lookup k m
{-# INLINE lookupUniq #-}

uniqMap :: UniqKeyMap v -> Map Int v
uniqMap (UniqKeyMap m _ _) = m
{-# INLINE uniqMap #-}

insertUniqEnd :: v -> UniqKeyMap v -> (UniqKeyMap v, Int)
insertUniqEnd v (UniqKeyMap m s e) = (UniqKeyMap (insert e v m) s (e + 1), e)
{-# INLINE insertUniqEnd #-}

insertUniqStart :: v -> UniqKeyMap v -> (UniqKeyMap v, Int)
insertUniqStart v (UniqKeyMap m s e) = (UniqKeyMap (insert s v m) (s - 1) e, s)
{-# INLINE insertUniqStart #-}

deleteUniq :: Int -> UniqKeyMap v -> UniqKeyMap v
deleteUniq k (UniqKeyMap m s e) = UniqKeyMap (delete k m) s e
{-# INLINE deleteUniq #-}

updateUniq :: Int -> (v -> v) -> UniqKeyMap v -> UniqKeyMap v
updateUniq k f (UniqKeyMap m s e) = UniqKeyMap (adjust f k m) s e
{-# INLINE updateUniq #-}

elemsUniq :: UniqKeyMap v -> [v]
elemsUniq (UniqKeyMap m _ _) = elems m
{-# INLINE elemsUniq #-}

emptyUniqKeyMap :: UniqKeyMap v
emptyUniqKeyMap = UniqKeyMap mempty 0 1
{-# INLINE emptyUniqKeyMap #-}

uniqMapFromList :: [v] -> UniqKeyMap v
uniqMapFromList = L.foldl' (\m v -> fst $ insertUniqEnd v m) emptyUniqKeyMap
{-# INLINE uniqMapFromList #-}
