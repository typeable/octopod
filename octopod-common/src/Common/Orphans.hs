{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Orphans () where

import Control.DeepSeq
import Data.Aeson
import Data.Map.Internal
import Data.Map.Ordered.Internal
import qualified Data.Map.Ordered.Strict as OM
import Data.MemoTrie
import Data.Text (Text)
import qualified Data.Text as T
import Data.These
import GHC.Generics (Generic)

deriving stock instance Generic (Map k v)

instance (HasTrie k, HasTrie v) => HasTrie (Map k v) where
  newtype Map k v :->: b = MapTrie {unMapTrie :: Reg (Map k v) :->: b}
  trie = trieGeneric MapTrie
  untrie = untrieGeneric unMapTrie
  enumerate = enumerateGeneric unMapTrie

deriving stock instance Generic (OMap k v)
deriving anyclass instance (NFData k, NFData v) => NFData (OMap k v)

instance (HasTrie k, HasTrie v) => HasTrie (OMap k v) where
  newtype OMap k v :->: b = OMapTrie {unOMapTrie :: Reg (OMap k v) :->: b}
  trie = trieGeneric OMapTrie
  untrie = untrieGeneric unOMapTrie
  enumerate = enumerateGeneric unOMapTrie

instance (ToJSON k, ToJSON v) => ToJSON (OMap k v) where
  toJSON = toJSON . OM.assocs

instance (FromJSON k, FromJSON v, Ord k) => FromJSON (OMap k v) where
  parseJSON = fmap OM.fromList . parseJSON

instance (HasTrie x, HasTrie y) => HasTrie (These x y) where
  newtype These x y :->: b = TheseTrie {unTheseTrie :: Reg (These x y) :->: b}
  trie = trieGeneric TheseTrie
  untrie = untrieGeneric unTheseTrie
  enumerate = enumerateGeneric unTheseTrie
