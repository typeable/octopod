{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Map.Ordered.Strict.Extra
  (
  )
where

import Data.Aeson
import Data.Map.Ordered.Strict

instance (ToJSON k, ToJSON v) => ToJSON (OMap k v) where
  toJSON = toJSON . assocs

instance (FromJSON k, FromJSON v, Ord k) => FromJSON (OMap k v) where
  parseJSON = fmap fromList . parseJSON
