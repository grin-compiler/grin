{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.SparseCaseOptimisation where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable
import Grin
import TypeEnv

sparseCaseOptimisation :: (TypeEnv, Exp) -> (TypeEnv, Exp)
sparseCaseOptimisation (typeEnv, exp) = (typeEnv, ana builder exp) where
  builder :: Exp -> ExpF Exp
  builder = \case
    ECase val@(Var name) alts ->
      ECaseF val
        [ alt
        | alt@(Alt cpat body) <- alts
        , possible varType allPatTags cpat
        ] where varType     = variableType typeEnv name :: Type
                allPatTags  = Set.fromList [tag | Alt (NodePat tag _) _ <- alts]

    exp -> project exp

  possible :: Type -> Set Tag -> CPat -> Bool
  possible (T_NodeSet nodeSet) allPatTags cpat = case cpat of
    NodePat tag _args -> Map.member tag nodeSet
    -- HINT: the default case is redundant if normal cases fully cover the domain
    DefaultPat -> 0 < Set.size (Set.difference (Map.keysSet nodeSet) allPatTags)
    _ -> False

  possible ty@T_SimpleType{} _ cpat = case cpat of
    LitPat lit -> ty == typeOfLit lit
    DefaultPat -> True -- HINT: the value domain is unknown, it is not possible to prove if it overlaps or it is fully covered
    _ -> False

  possible _ _ _ = True -- bypass everything else
