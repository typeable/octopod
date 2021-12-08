module Reflex.Dom.Renderable
  ( Renderable (..),
  )
where

import Data.Sequence (Seq (..))
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom

class Renderable a where
  rndr :: DomBuilder t m => a -> m ()
  rndrList :: DomBuilder t m => [a] -> m ()
  rndrList [] = pure ()
  rndrList (a : aa) = rndr a >> rndr aa
  {-# INLINE rndrList #-}

instance Renderable Text where
  rndr = text
  {-# INLINE rndr #-}

instance Renderable a => Renderable [a] where
  rndr = rndrList
  {-# INLINE rndr #-}

instance Renderable Char where
  rndr = text . T.singleton
  {-# INLINE rndr #-}
  rndrList = text . T.pack
  {-# INLINE rndrList #-}

instance Renderable a => Renderable (Seq a) where
  rndr Empty = pure ()
  rndr (a :<| aa) = rndr a >> rndr aa
  {-# INLINE rndr #-}
