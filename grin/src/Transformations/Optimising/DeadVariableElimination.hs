{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.DeadVariableElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Data.Functor.Foldable as Foldable
import Grin
import TypeEnv
import Transformations.Util
import qualified Data.Foldable
import Lens.Micro
import Data.Maybe
import Debug.Trace


deadVariableElimination :: (TypeEnv, Exp) -> (TypeEnv, Exp)
deadVariableElimination (typeEnv, e) = traceShow effects (typeEnv, fst $ cata folder e) where

  effects = effectMap (typeEnv, e)

  folder :: ExpF (Exp, Set Name) -> (Exp, Set Name)
  folder = \case

    exp@(EBindF (left, _) lpat right@(_, rightRef))
      | lpat /= Unit
      , vars <- foldNamesVal Set.singleton lpat
      , all ((/=) unit_t . variableType typeEnv) vars
      , all (flip Set.notMember rightRef) vars
      -> case left of
          (SApp name _) | Just _ <- Map.lookup name effects -> embedExp exp
          _                                                 -> right

    exp -> embedExp exp
    where
      embedExp :: ExpF (Exp, Set Name) -> (Exp, Set Name)
      embedExp exp0 = (embed $ fmap fst exp0, foldNameUseExpF Set.singleton exp0 `mappend` Data.Foldable.fold (fmap snd exp0))

data Effect
  = Effectful Name
  | Update [Int]
  deriving (Eq, Show, Ord)

data EffectWithCalls
  = EffectfulW Name
  | UpdateW { updateLocs :: [Int] }
  | CallsW { callsFunction :: Name }
  deriving (Eq, Show, Ord)

toEffect :: EffectWithCalls -> Effect
toEffect = \case
  EffectfulW n -> Effectful n
  UpdateW locs -> Update locs
  _            -> error "Impossible"

effectMap :: (TypeEnv, Exp) -> Map Name (Set Effect)
effectMap (te, e) = effectfulFunctions $ unMMap $ snd $ cata buildEffectMap e where
  buildEffectMap :: ExpF (Set EffectWithCalls, MMap Name (Set EffectWithCalls)) -> (Set EffectWithCalls, MMap Name (Set EffectWithCalls))
  buildEffectMap = \case
    ProgramF defs -> mconcat defs
    DefF name _ (effs, _) -> (mempty, MMap $ Map.singleton name effs)
    EBindF lhs _ rhs -> lhs <> rhs
    ECaseF _ alts -> mconcat alts
    SAppF    name _
      | Just () <- te ^? function . at name . _Just . _ReturnType . _T_SimpleType . _T_Unit
      -> (Set.singleton (EffectfulW name), mempty)
      | otherwise
      -> (Set.singleton (CallsW name), mempty)
    SUpdateF name _
      | Just locs <- te ^? variable . at name . _Just . _T_SimpleType . _T_Location
      -> (mempty, MMap $ Map.singleton name $ Set.singleton $ UpdateW locs)
    rest -> Data.Foldable.fold rest

isCall :: EffectWithCalls -> Bool
isCall = \case
  CallsW _ -> True
  _        -> False

unifyUpdates :: Set EffectWithCalls -> Set EffectWithCalls
unifyUpdates es = calls `Set.union` (Set.fromList $ map UpdateW $ map updateLocs $ Set.toList updates) where
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
