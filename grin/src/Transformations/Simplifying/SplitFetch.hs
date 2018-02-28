{-# LANGUAGE LambdaCase #-}
module Transformations.Simplifying.SplitFetch where

import Data.Functor.Foldable as Foldable
import Grin

splitFetch :: Exp -> Exp
splitFetch = ana builder where

  builder :: Exp -> ExpF Exp
  builder = \case
    EBind (SFetch name) (ConstTagNode _ args) exp     -> project $ newBinds name exp (zip [1..] args)
    EBind (SFetch name) (VarTagNode tagvar args) exp  -> project $ newBinds name exp (zip [0..] $ Var tagvar : args)
    exp -> project exp

  newBinds :: Name -> Exp -> [(Int, LPat)] -> Exp
  newBinds name = foldr (\(idx, lpat) exp -> EBind (SFetchI name (Just idx)) lpat exp)
