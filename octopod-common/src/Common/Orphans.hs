{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Orphans () where

import Control.DeepSeq
import Data.Map.Ordered

-- better than nothing
instance (NFData a, NFData b) => NFData (OMap a b) where
  rnf = rnf . toMap
