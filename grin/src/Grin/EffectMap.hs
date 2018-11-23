{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Grin.EffectMap where 

import Data.Map (Map)
import Data.Set (Set)
import Data.Monoid

import qualified Data.Map as Map
import qualified Data.Set as Set

import Grin.Grin 

-- | Either the name of a function with return type of Unit, 
-- or a list of heap locations updated by the function.
data Effect
  = Effectful Name
  | Update { updateLocs :: [Int] }
  | Store { storeLocs :: [Int]}
  deriving (Eq, Show, Ord)

-- | Mapping of function names to their respective side effects.
newtype EffectMap = EffectMap (Map Name (Set Effect))
  deriving (Eq, Ord, Show, Semigroup, Monoid)