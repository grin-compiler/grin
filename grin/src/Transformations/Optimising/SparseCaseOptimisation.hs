{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.Optimising.SparseCaseOptimisation where

import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor.Foldable as Foldable

import Control.Monad.Trans.Except

import Grin.Grin
import Grin.Pretty
import Grin.TypeEnv
import Transformations.Util

sparseCaseOptimisation :: TypeEnv -> Exp -> Either String Exp
sparseCaseOptimisation TypeEnv{..} = runExcept . anaM builder where
  builder :: Exp -> Except String (ExpF Exp)
  builder = \case
    -- TODO: reduce noise and redundancy
    ECase scrut@(Var name) alts -> do
      scrutType <- lookupExcept (notInTyEnv scrut) name _variable
      let alts' = filterAlts scrutType alts
      pure $ ECaseF scrut alts'
    ECase scrut@(ConstTagNode tag _) alts -> do
      let scrutType = T_NodeSet $ Map.singleton tag mempty
          alts'     = filterAlts scrutType alts
      pure $ ECaseF scrut alts'
    ECase scrut@(Lit l) alts -> do
      let scrutType = typeOfLit l
          alts'     = filterAlts scrutType alts
      pure $ ECaseF scrut alts'
    ECase scrut@(Undefined ty) alts -> do
      let alts' = filterAlts ty alts
      pure $  ECaseF scrut alts'
    ECase scrut _ -> throwE $ unsuppScrut scrut
    exp -> pure . project $ exp

  notInTyEnv v = "SCO: Variable " ++ show (PP v) ++ " not found in type env"
  unsuppScrut scrut = "SCO: Unsupported case scrutinee: " ++ show (PP scrut)

  filterAlts :: Type -> [Exp] -> [Exp]
  filterAlts scrutTy alts =
    [ alt
    | alt@(Alt cpat body) <- alts
    , possible scrutTy allPatTags cpat
    ] where allPatTags  = Set.fromList [tag | Alt (NodePat tag _) _ <- alts]

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

  possible ty _ _ = ty /= dead_t -- bypass everything else
