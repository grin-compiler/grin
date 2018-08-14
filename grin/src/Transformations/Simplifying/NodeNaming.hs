{-# LANGUAGE FlexibleContexts #-}

module Transformations.Simplifying.NodeNaming where

import Control.Monad
import Data.Functor.Foldable as Foldable

import Grin.Grin
import Transformations.Names

-- TODO: remove this
import Grin.Parse
import Grin.Pretty
import Transformations.BindNormalisation

anaM :: (Corecursive t, Traversable (Base t), Monad m) => (a -> m (Base t a)) -> a -> m t
anaM coalg = x where x = (return . embed) <=< traverse x <=< coalg

newNodeName :: NameM Name
newNodeName = deriveNewName "node"

-- EBind can never have an EBind node as its left-hand side argument
-- this function enforces this invariant
normalizeLeftBinds :: Exp -> Exp
normalizeLeftBinds = ana coalg where
  coalg :: Exp -> ExpF Exp
  coalg e = case e of
    EBind lhs1@(EBind lhs2 pat2 rhs2) pat1 rhs1 -> EBindF lhs2 pat2 (EBind rhs2 pat1 rhs1)
    e -> project e


-- this transformation can invalidate the "no left bind" invariant
-- so we have to normalize these incorrect bindings
nameNodes :: Exp -> Exp
nameNodes e = normalizeLeftBinds . evalNameM e . cata alg $ e where
  alg :: ExpF (NameM Exp) -> NameM Exp
  alg e = case e of
    SStoreF    x@VarTagNode{}   -> bindVal SStore x
    SStoreF    x@ConstTagNode{} -> bindVal SStore x
    SUpdateF p x@VarTagNode{}   -> bindVal (SUpdate p) x
    SUpdateF p x@ConstTagNode{} -> bindVal (SUpdate p) x
    SReturnF   x@VarTagNode{}   -> bindVal SReturn x
    SReturnF   x@ConstTagNode{} -> bindVal SReturn x
    expf -> fmap embed . sequence $ expf

  -- binds a Val (usually a node) to a name, then performs some action on it
  bindVal :: (Val -> Exp) -> Val -> NameM Exp
  bindVal context val = do
    nodeVar <- fmap Var newNodeName
    return $ EBind (SReturn val) nodeVar (context nodeVar)
