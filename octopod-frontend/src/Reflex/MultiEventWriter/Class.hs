module Reflex.MultiEventWriter.Class
  ( MultiEventWriter(..)
  ) where

import           Control.Monad.Reader (ReaderT, lift)
import           Data.Type.Equality
import           Reflex.Dom


-- | Same as 'EventWriter' but without a fundep.
class (Monad m, Semigroup w) => MultiEventWriter t w m | m -> t where
  tellMultiEvent :: Event t w -> m ()

instance MultiEventWriter t w m => MultiEventWriter t w (ReaderT r m) where
  tellMultiEvent = lift . tellMultiEvent

instance (MultiEventWriter' t w u m, Monad m, Semigroup w) => MultiEventWriter t w (EventWriterT t u m) where
  tellMultiEvent = tellMultiEvent' @t @w @u

-- Horrible hacks incoming

type MultiEventWriter' t w u m = MultiEventWriter'' t w u m (w == u)

class (u == w) ~ f => MultiEventWriter'' t w u m f where
  tellMultiEvent' :: Event t w -> EventWriterT t u m ()

instance ((w == w) ~ 'True, Reflex t, Monad m, Semigroup w)
  => MultiEventWriter'' t w w m 'True where
  tellMultiEvent' = tellEvent

instance ((u == w) ~ 'False, MultiEventWriter t w m) => MultiEventWriter'' t w u m 'False where
  tellMultiEvent' = lift . tellMultiEvent
