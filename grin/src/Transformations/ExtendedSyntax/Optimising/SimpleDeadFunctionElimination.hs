{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Transformations.ExtendedSyntax.Optimising.SimpleDeadFunctionElimination where


import Data.Map (Map)
import Data.Set (Set)
import Data.Functor.Foldable as Foldable

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable

import Text.Printf

import Grin.ExtendedSyntax.Grin

simpleDeadFunctionElimination :: Program -> Program
simpleDeadFunctionElimination exp@(Program exts defs) = Program exts [def | def@(Def name _ _) <- defs, Set.member name liveDefs] where
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
    SAppF name _ | not (isExternalName exts name) -> Set.singleton name
    exp -> Data.Foldable.fold exp
