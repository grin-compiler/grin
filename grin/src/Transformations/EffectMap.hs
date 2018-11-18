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
import Data.List
import Data.Maybe
import Debug.Trace

import Grin.Grin
import Grin.TypeEnv
import Grin.EffectMap
import Transformations.Util


effectMap :: (TypeEnv, Exp) -> EffectMap
effectMap (te, e) = unifyEffectMap . EffectMap $ effectfulFunctions $ unMMap $ snd $ para buildEffectMap e where
  buildEffectMap :: ExpF (Exp, (Set EffectWithCalls, MMap Name (Set EffectWithCalls))) -> (Set EffectWithCalls, MMap Name (Set EffectWithCalls))
  buildEffectMap = \case
    ProgramF defs -> mconcat . map snd $ defs
    DefF name _ (_,(effs, _)) -> (mempty, MMap $ Map.singleton name effs)
    EBindF (SStore _,lhs) (Var v) (_,rhs)
      | Just locs <- te ^? variable . at v . _Just . _T_SimpleType . _T_Location
      -> let storeEff = (Set.singleton $ EffectW $ Store locs, mempty)
         in lhs <> rhs <> storeEff
    EBindF (_,lhs) _ (_,rhs) -> lhs <> rhs
    ECaseF _ alts -> mconcat . map snd $ alts
    SAppF    name _
      | Just () <- te ^? function . at name . _Just . _ReturnType . _T_SimpleType . _T_Unit
      -> (Set.singleton (EffectW $ Effectful name), mempty)
      | otherwise
      -> (Set.singleton (CallsW name), mempty)
    SUpdateF name _
      | Just locs <- te ^? variable . at name . _Just . _T_SimpleType . _T_Location
      -> (Set.singleton $ EffectW $ Update locs, mempty)
    rest -> Data.Foldable.fold . fmap snd $ rest

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

instance (Ord k, Semigroup m) => Semigroup (MMap k m) where
  (MMap m1) <> (MMap m2) = MMap (Map.unionWith (<>) m1 m2)
instance (Ord k, Monoid m) => Monoid (MMap k m) where
  mempty = MMap mempty
  mappend (MMap m1) (MMap m2) = MMap (Map.unionWith mappend m1 m2)

unifyEffectMap :: EffectMap -> EffectMap 
unifyEffectMap (EffectMap effects) = EffectMap $ Map.map unifyEffectSet effects

unifyEffectSet :: Set Effect -> Set Effect
unifyEffectSet effects =  updates <> stores <> otherEffects where 
  updates   = if null updateLocs then mempty else Set.singleton (Update updateLocs)
  stores    = if null storeLocs  then mempty else Set.singleton (Store storeLocs)

  updateLocs :: [Int]
  updateLocs = nub . concat $ [ locs | (Update locs) <- Set.toList effects]

  storeLocs :: [Int]
  storeLocs  = nub . concat $ [ locs | (Store locs) <- Set.toList effects]

  otherEffects :: Set Effect
  otherEffects = Set.fromList [ eff | eff@(Effectful _) <- Set.toList effects]
