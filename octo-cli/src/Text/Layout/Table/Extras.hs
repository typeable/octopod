{-# OPTIONS_GHC -Wno-orphans #-}

module Text.Layout.Table.Extras
  (
  )
where

import Data.Text
import qualified Data.Text as T
import Text.Layout.Table.Cell
import Text.Layout.Table.Primitives.AlignInfo
import Text.Layout.Table.StringBuilder

instance Cell Text where
  dropLeft = T.drop
  dropRight = T.dropEnd
  visibleLength = T.length
  measureAlignment p xs = case T.break p xs of
    (ls, rs) ->
      AlignInfo (T.length ls) $
        if T.null rs
          then Nothing
          else Just $  T.length rs - 1
  buildCell = stringB . T.unpack

instance StringBuilder Text where
  stringB = T.pack
  charB = T.singleton
  replicateCharB x c = T.replicate x (T.singleton c)
