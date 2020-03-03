{-# LANGUAGE LambdaCase, TupleSections, OverloadedStrings #-}
module Transformations.ExtendedSyntax.Optimising.DeadFunctionElimination where


import Data.Map (Map)
import Data.Set (Set)
import Data.Functor.Foldable as Foldable

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Foldable

import Text.Printf

import Grin.ExtendedSyntax.Grin

deadFunctionElimination :: Program -> Program
deadFunctionElimination exp@(Program exts defs) = Program liveExts liveDefs where
  liveExts :: [External]
  liveExts = [ext | ext <- exts, Set.member (eName ext) liveNames]

  liveDefs :: [Exp]
  liveDefs = [def | def@(Def name _ _) <- defs, Set.member name liveSet]

  liveNames :: Set Name
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
    seen    = foldMap (maybe mempty (cata collectFuns) . lookupDef) toVisit

  collectFuns :: ExpF (Set Name) -> Set Name
  collectFuns = \case
    SAppF name _ | Map.member name defMap -> Set.singleton name
    exp -> Data.Foldable.fold exp

  collectAll :: ExpF (Set Name) -> Set Name
  collectAll = \case
    SAppF name args -> Set.singleton name
    exp -> Data.Foldable.fold exp
