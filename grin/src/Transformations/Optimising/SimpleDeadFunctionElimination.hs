{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Transformations.Optimising.SimpleDeadFunctionElimination where

import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin.Grin

simpleDeadFunctionElimination :: Program -> Program
simpleDeadFunctionElimination (Program exts defs) = Program liveExts liveDefs where

  liveExts = [ext | ext <- exts, Set.member (eName ext) liveNames]
  liveDefs = [def | def@(Def name _ _) <- defs, Set.member name liveSet]

  liveNames = cata collectAll $ Program [] liveDefs -- collect all live names

  defMap :: Map Name Def
  defMap = Map.fromList [(name, def) | def@(Def name _ _) <- defs]

  lookupDef :: Name -> Maybe Def
  lookupDef name = Map.lookup name defMap

  liveSet :: Set Name
  liveSet = fst $ until (\(live, visited) -> live == visited) visit (Set.singleton "grinMain", mempty)

  visit :: (Set Name, Set Name) -> (Set Name, Set Name)
  visit (live, visited) = (mappend live seen, mappend visited toVisit) where
    toVisit = Set.difference live visited
    seen    = foldMap (maybe mempty (cata collect) . lookupDef) toVisit

  collect :: ExpF (Set Name) -> Set Name
  collect = \case
    SAppF name _ | Map.member name defMap -> Set.singleton name
    exp -> Data.Foldable.fold exp

  collectAll :: ExpF (Set Name) -> Set Name
  collectAll = \case
    SAppF name args -> Set.singleton name
    exp -> Data.Foldable.fold exp
