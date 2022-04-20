{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Orphans () where

import Control.DeepSeq
import Data.Aeson
import Data.Map.Internal
import Data.Map.Ordered.Internal
import qualified Data.Map.Ordered.Strict as OM
import GHC.Generics (Generic)

deriving stock instance Generic (Map k v)

deriving stock instance Generic (OMap k v)
deriving anyclass instance (NFData k, NFData v) => NFData (OMap k v)
instance (ToJSON k, ToJSON v) => ToJSON (OMap k v) where
  toJSON = toJSON . OM.assocs

instance (FromJSON k, FromJSON v, Ord k) => FromJSON (OMap k v) where
  parseJSON = fmap OM.fromList . parseJSON
