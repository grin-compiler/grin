{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.EvaluatedCaseElimination where

import Data.Functor.Foldable as Foldable
import Grin.Grin

evaluatedCaseElimination :: Exp -> Exp
evaluatedCaseElimination = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    ECase val alts | all (altBodyEQ $ SReturn val) alts -> SReturnF val
    exp -> project exp

  altBodyEQ :: Exp -> Alt -> Bool
  altBodyEQ exp (Alt _cpat body) = exp == body
