{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.DeadProcedureElimination where

import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin

deadProcedureElimination :: Program -> Program
deadProcedureElimination (Program defs) = Program [def | def@(Def name _ _) <- defs, Set.member name liveDefs] where
  defMap :: Map Name Def
  defMap = Map.fromList [(name, def) | def@(Def name _ _) <- defs]

  lookupDef :: Name -> Maybe Def
  lookupDef name = Map.lookup name defMap

  liveDefs :: Set Name
  liveDefs = fst $ until (\(live, visited) -> live == visited) visit (Set.singleton "grinMain", mempty)

  visit :: (Set Name, Set Name) -> (Set Name, Set Name)
  visit (live, visited) = (mappend live seen, mappend visited toVisit) where
    toVisit = Set.difference live visited
    seen    = foldMap (maybe mempty (cata collect) . lookupDef) toVisit

  collect :: ExpF (Set Name) -> Set Name
  collect = \case
    SAppF name _ | not (isPrimName name) -> Set.singleton name
    exp -> Data.Foldable.fold exp
