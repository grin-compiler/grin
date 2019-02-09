{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Grin.EffectMap
  ( EffectMap(..)
  , Effects(..)
  , hasPossibleSideEffect
  , storesEff
  , primopEff
  , updatesEff
  , hasTrueSideEffect
  ) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Monoid

import qualified Data.Map as Map
import qualified Data.Set as Set

import Lens.Micro.Platform

import Grin.Grin

-- | Contains the name of all the effectful primops used by the function,
-- and a list of heap locations updated by it.
data Effects
  = Effects
  { _effectfulPrimops :: Set Name
  , _updateLocs       :: Set Int
  , _storeLocs        :: Set Int
  }
  deriving (Eq, Ord, Show)

instance Semigroup Effects where
  (<>) (Effects primops1 updateLocs1 storeLocs1) (Effects primops2 updateLocs2 storeLocs2)
    = Effects (primops1 <> primops2) (updateLocs1 <> updateLocs2) (storeLocs1 <> storeLocs2)

instance Monoid Effects where
  mempty = Effects mempty mempty mempty



-- | Mapping of function names to their respective side effects.
newtype EffectMap = EffectMap { _effects :: Map Name Effects }
  deriving (Eq, Ord, Show, Semigroup, Monoid)

concat <$> mapM makeLenses [''Effects, '' EffectMap]


primopEff :: Name -> Effects
primopEff f = Effects (Set.singleton f) mempty mempty

updatesEff :: [Int] -> Effects
updatesEff locs = Effects mempty (Set.fromList locs) mempty

storesEff :: [Int] -> Effects
storesEff locs = Effects mempty mempty (Set.fromList locs)


hasSomeEffect :: (Effects -> Set a) -> Name -> EffectMap -> Bool
hasSomeEffect selectEff f (EffectMap effMap)
  | Just effects <- Map.lookup f effMap
  = not . null . selectEff $ effects
  | otherwise = False

hasSideEffectingPrimop :: Name -> EffectMap -> Bool
hasSideEffectingPrimop = hasSomeEffect _effectfulPrimops

hasUpdates :: Name -> EffectMap -> Bool
hasUpdates = hasSomeEffect _updateLocs

hasStores :: Name -> EffectMap -> Bool
hasStores = hasSomeEffect _storeLocs

-- | Checks whether a function has a true side effect
-- , meaning it calls a side-effecting primop.
hasTrueSideEffect :: Name -> EffectMap -> Bool
hasTrueSideEffect = hasSideEffectingPrimop

-- | Checks whether a function has a possible side effect
-- , meaning it either has a true side effect
-- , or it updates a location, which can cause a side effect.
hasPossibleSideEffect :: Name -> EffectMap -> Bool
hasPossibleSideEffect f effMap = hasTrueSideEffect f effMap || hasUpdates f effMap
