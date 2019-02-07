{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Lens.Micro.Extra where

import Lens.Micro.Internal
import Lens.Micro.Platform
import Data.Vector as V
import Data.Functor.Infix ((<$$>))
import Data.Monoid (Any(..))


type instance Index   (Vector a) = Int
type instance IxValue (Vector a) = a

instance At (Vector a) where
  at k = lens (V.!? k) (\v -> maybe v (\a -> v V.// [(k, a)]))

isn't :: Getting Any s a -> s -> Bool
isn't = not <$$> has
