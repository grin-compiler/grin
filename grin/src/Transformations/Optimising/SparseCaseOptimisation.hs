{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.SparseCaseOptimisation where

import qualified Data.Map as Map
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
        , possible varType cpat
        ] where varType = variableType typeEnv name

    exp -> project exp

  possible :: Type -> CPat -> Bool
  possible (T_NodeSet nodeSet) cpat = case cpat of
    NodePat tag _args -> Map.member tag nodeSet
    _ -> False

  possible ty@T_SimpleType{} cpat = case cpat of
    LitPat lit -> ty == typeOfLit lit
    _ -> False

  possible _ _ = True -- bypass everything else
