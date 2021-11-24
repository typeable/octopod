module Reflex.Dom.Renderable
  ( Renderable (..),
  )
where

import Data.Text (Text)
import Reflex.Dom

class Renderable a where
  rndr :: DomBuilder t m => a -> m ()

instance Renderable Text where
  rndr = text
  {-# INLINE rndr #-}

instance Renderable a => Renderable [a] where
  rndr [] = pure ()
  rndr (a : aa) = rndr a >> rndr aa
  {-# INLINE rndr #-}
