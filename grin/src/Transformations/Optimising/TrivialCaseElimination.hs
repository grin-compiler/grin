{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.TrivialCaseElimination where

import Data.Functor.Foldable as Foldable
import Grin.Grin
import Transformations.Util

trivialCaseElimination :: Exp -> Exp
trivialCaseElimination = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    ECase val [Alt DefaultPat body] -> SBlockF body
    ECase val [Alt cpat body]       -> SBlockF $ EBind (SReturn val)  (cpatToLPat cpat) body
    exp -> project exp
