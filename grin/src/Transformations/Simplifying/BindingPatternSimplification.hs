{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module Transformations.Simplifying.BindingPatternSimplification where

import Control.Monad
import Data.Functor.Foldable as Foldable

import Grin.Grin
import Transformations.Util
import Transformations.Names

import Lens.Micro.Extra


newNodeName :: NameM Name
newNodeName = deriveNewName "p"

-- NOTE: This transformation can invalidate the "no left bind" invariant,
--       so we have to normalize these incorrect bindings after this transformation.
bindingPatternSimplification :: Exp -> (Exp, ExpChanges)
bindingPatternSimplification e = evalNameM e . cataM alg $ e where
  alg :: ExpF Exp -> NameM Exp
  alg = \case

    -- NOTE: <pat> <- pure <var>
    -- The above pattern does not need to be simplified.
    EBindF lhs@(SReturn Var{}) pat rhs ->
      pure $ EBind lhs pat rhs

    -- NOTE: binding to Unit?
    EBindF lhs pat rhs | isn't _ValVar pat -> do
      newVar <- fmap Var newNodeName
      pure $ EBind lhs newVar (EBind (SReturn newVar) pat rhs)

    ECaseF scrut alts | isn't _ValVar scrut -> do
      newVar <- fmap Var newNodeName
      pure $ SBlock $ EBind (SReturn scrut) newVar (ECase newVar alts)

    expf -> pure . embed $ expf
