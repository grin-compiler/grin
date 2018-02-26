{-# LANGUAGE LambdaCase #-}
module Transformations.Simplifying.SplitFetch where

import Data.Functor.Foldable as Foldable

import Grin

splitFetch :: Exp -> Exp
splitFetch = cata folder where
  folder = \case
    EBindF (SFetch name) (ConstTagNode _ args) exp -> EBind (SBlock $ newBinds name $ zip [1..] args) Unit exp
    EBindF (SFetch name) (VarTagNode tagvar args mapping {-TODO-}) exp -> EBind (SBlock $ newBinds name $ zip [0..] $ Var tagvar : args) Unit exp
    e -> embed e

  newBinds name [] = SReturn Unit
  newBinds name ((i, var) : vars) = EBind (SFetchI name (Just i)) var $ newBinds name vars
