{-# LANGUAGE LambdaCase #-}
module Transformations.BindNormalisation where

import Data.Functor.Foldable as Foldable

import Grin.Grin

bindNormalisation :: Exp -> Exp
bindNormalisation = hylo alg coalg where
  alg :: ExpF Exp -> Exp
  alg (SBlockF e) = e
  alg e = embed e

  coalg :: Exp -> ExpF Exp
  coalg (EBind lhs1 pat1 rhs1)
    | SBlock (EBind lhs2 pat2 rhs2) <- lhs1
    = SBlockF $ EBind lhs2 pat2 (EBind rhs2 pat1 rhs1)
  coalg e = project e
