{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.ExtendedSyntax.Optimising.SparseCaseOptimisation where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable

import Control.Monad.Trans.Except

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.TypeEnv
import Transformations.ExtendedSyntax.Util

sparseCaseOptimisation :: TypeEnv -> Exp -> Either String Exp
sparseCaseOptimisation TypeEnv{..} = runExcept . anaM builder where
  builder :: Exp -> Except String (ExpF Exp)
  builder = \case
    ECase scrut alts -> do
      scrutType <- lookupExcept (notInTyEnv scrut) scrut _variable
      let alts' = filterAlts scrutType alts
      pure $ ECaseF scrut alts'
    exp -> pure . project $ exp

  notInTyEnv v = "SCO: Variable " ++ show (PP v) ++ " not found in type env"

  filterAlts :: Type -> [Exp] -> [Exp]
  filterAlts scrutTy alts =
    [ alt
    | alt@(Alt cpat _name _body) <- alts
    , possible scrutTy allPatTags cpat
    ] where allPatTags  = Set.fromList [tag | Alt (NodePat tag _) _name _body <- alts]

  possible :: Type -> Set Tag -> CPat -> Bool
  possible (T_NodeSet nodeSet) allPatTags cpat = case cpat of
    NodePat tag _args -> Map.member tag nodeSet
    -- HINT: the default case is redundant if normal cases fully cover the domain
    DefaultPat -> not $ null (Set.difference (Map.keysSet nodeSet) allPatTags)
    _ -> False

  possible ty@T_SimpleType{} _ cpat = case cpat of
    LitPat lit -> ty == typeOfLit lit
    DefaultPat -> True -- HINT: the value domain is unknown, it is not possible to prove if it overlaps or it is fully covered
    _ -> False
