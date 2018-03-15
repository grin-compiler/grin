{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor, OverloadedStrings #-}
module Transformations.Optimising.ConstantFolding where

import Check
import Grin
import Test
import Test.Hspec
import Test.QuickCheck
import Transformations.Substitution
import Data.Functor.Foldable

import qualified Data.Map.Strict as Map

{-
Constant folding is not part of the official grin optimization pipeline.
This transformation is used for demonstrate and experiment with the
testing.
-}

constantFolding :: Exp -> Exp
constantFolding = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    EBind (SReturn v) (Var n) rest | isConstant v ->
      project $ substitution (Map.singleton n v) rest

    rest ->
      project rest
