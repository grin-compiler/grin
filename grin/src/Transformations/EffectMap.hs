{-# LANGUAGE LambdaCase #-}
module Transformations.EffectMap where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Lens.Micro
import Data.Maybe
import Debug.Trace

import Grin.Grin
import Grin.TypeEnv
import Transformations.Util


effectMap :: (TypeEnv, Exp) -> Map Name (Set Effect)
effectMap (te, e) = effectfulFunctions $ unMMap $ snd $ cata buildEffectMap e where
  buildEffectMap :: ExpF (Set EffectWithCalls, MMap Name (Set EffectWithCalls)) -> (Set EffectWithCalls, MMap Name (Set EffectWithCalls))
  buildEffectMap = \case
    DefF name _ (effs, _) -> (mempty, MMap $ Map.singleton name effs)
    SAppF name _
      | Just () <- te ^? function . at name . _Just . _ReturnType . _T_SimpleType . _T_Unit
      -> (Set.singleton (EffectW $ Effectful name), mempty)
      | otherwise
      -> (Set.singleton (CallsW name), mempty)
    SUpdateF name _
      | Just locs <- te ^? variable . at name . _Just . _T_SimpleType . _T_Location
      -> (mempty, MMap $ Map.singleton name $ Set.singleton $ EffectW $ Update locs)
    rest -> Data.Foldable.fold rest

data EffectWithCalls
  = EffectW { toEffect :: Effect }
  | CallsW { callsFunction :: Name }
  deriving (Eq, Show, Ord)

isCall :: EffectWithCalls -> Bool
isCall = \case
  CallsW _ -> True
  _        -> False

unifyUpdates :: Set EffectWithCalls -> Set EffectWithCalls
unifyUpdates es = calls `Set.union` (Set.fromList $ map (EffectW . Update) $ map (updateLocs . toEffect) $ Set.toList updates) where
  (updates, calls) = Set.partition isCall es

-- Removes the calls information and collects all the transitive effects.
-- Returns a Map that contains only the effectful function calls.
effectfulFunctions :: Map Name (Set EffectWithCalls) -> Map Name (Set Effect)
effectfulFunctions em = removeCalls $ go em where

  removeCalls = Map.map (Set.map toEffect . unifyUpdates) . Map.filter (not . Set.null) . Map.map (Set.filter (not . isCall))

  go em0 = let em1 = flip Map.map em0 $ \es ->
                        let (calls, rest) = Set.partition isCall es
                        in Set.unions [calls, rest, mconcat $ map (fromMaybe mempty . flip Map.lookup em . callsFunction) $ Set.toList calls]
           in if em0 == em1 then em0 else go em1

-- MonoidMap
newtype MMap k m = MMap { unMMap :: Map k m }
  deriving Show

instance (Ord k, Monoid m) => Monoid (MMap k m) where
  mempty = MMap mempty
  mappend (MMap m1) (MMap m2) = MMap (Map.unionWith mappend m1 m2)
