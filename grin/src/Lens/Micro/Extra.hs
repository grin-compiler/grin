{-# LANGUAGE TypeFamilies #-}
module Lens.Micro.Extra where

import Lens.Micro.Internal
import Lens.Micro.Platform
import Data.Vector as V


type instance Index   (Vector a) = Int
type instance IxValue (Vector a) = a

instance At (Vector a) where
  at k = lens (V.!? k) (\v -> maybe v (\a -> v V.// [(k, a)]))
