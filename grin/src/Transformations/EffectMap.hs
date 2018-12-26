{-# LANGUAGE LambdaCase, OverloadedStrings #-}
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
effectMap (te, e) = EffectMap $ effectfulFunctions $ unMMap $ snd $ para buildEffectMap e where
  buildEffectMap :: ExpF (Exp, (Set EffectWithCalls, MMap Name (Set EffectWithCalls))) -> (Set EffectWithCalls, MMap Name (Set EffectWithCalls))
  buildEffectMap = \case
    DefF name _ (_,(effs, _)) -> (mempty, MMap $ Map.singleton name effs)
    EBindF (SStore _,lhs) (Var v) (_,rhs)
      | Just locs <- te ^? variable . at v . _Just . _T_SimpleType . _T_Location
      -> let storeEff = (Set.singleton $ EffectW $ storesEff locs, mempty)
         in lhs <> rhs <> storeEff
    -- FIXME: handle effectful primops properly
    SAppF "_prim_int_print" _  -> (Set.singleton (EffectW $ primopEff "_prim_int_print"), mempty)
    SAppF "_prim_string_print" _  -> (Set.singleton (EffectW $ primopEff "_prim_string_print"), mempty)
    SAppF "_prim_usleep" _ -> (Set.singleton (EffectW $ primopEff "_prim_usleep"), mempty)
    SAppF    name _ -> (Set.singleton (CallsW name), mempty)
    SUpdateF name _
      | Just locs <- te ^? variable . at name . _Just . _T_SimpleType . _T_Location
      -> (Set.singleton $ EffectW $ updatesEff locs, mempty)
    rest -> Data.Foldable.fold . fmap snd $ rest

data EffectWithCalls
  = EffectW { toEffects :: Effects }
  | CallsW { callsFunction :: Name }
  deriving (Eq, Show, Ord)

isCall :: EffectWithCalls -> Bool
isCall = \case
  CallsW _ -> True
  _        -> False

-- Removes the calls information and collects all the transitive effects.
-- Returns a Map that contains only the effectful function calls.
effectfulFunctions :: Map Name (Set EffectWithCalls) -> Map Name Effects
effectfulFunctions em = removeCalls $ go em where

  removeCalls = Map.map (foldMap toEffects) . Map.filter (not . Set.null) . Map.map (Set.filter (not . isCall))

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
