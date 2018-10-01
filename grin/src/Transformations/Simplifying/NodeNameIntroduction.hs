{-# LANGUAGE FlexibleContexts #-}

module Transformations.Simplifying.NodeNameIntroduction where

import Control.Monad
import Data.Functor.Foldable as Foldable

import Grin.Grin
import Transformations.Names

-- TODO: remove this
import Grin.Parse
import Grin.Pretty
import Transformations.BindNormalisation

newNodeName :: NameM Name
newNodeName = deriveNewName "node"

-- this transformation can invalidate the "no left bind" invariant
-- so we have to normalize these incorrect bindings
nodeNameIntroduction :: Exp -> Exp
nodeNameIntroduction e = evalNameM e . cata alg $ e where
  alg :: ExpF (NameM Exp) -> NameM Exp
  alg e = case e of
    SStoreF    x@VarTagNode{}         -> bindVal SStore x
    SStoreF    x@ConstTagNode{}       -> bindVal SStore x
    SUpdateF p x@VarTagNode{}         -> bindVal (SUpdate p) x
    SUpdateF p x@ConstTagNode{}       -> bindVal (SUpdate p) x
    SReturnF   x@VarTagNode{}         -> bindVal SReturn x
    SReturnF   x@ConstTagNode{}       -> bindVal SReturn x
    SAppF      f args                 -> bindFromApp f args
    ECaseF     x@VarTagNode{}   altsM -> bindFromCase x altsM
    ECaseF     x@ConstTagNode{} altsM -> bindFromCase x altsM
    expf -> fmap embed . sequence $ expf

  -- binds a Val (usually a node) to a name, then performs some action on it
  bindVal :: (Val -> Exp) -> Val -> NameM Exp
  bindVal context val = do
    nodeVar <- fmap Var newNodeName
    return $ SBlock $ EBind (SReturn val) nodeVar (context nodeVar)

  -- binds the scrutinee out from a case expression
  bindFromCase :: Val -> [NameM Exp] -> NameM Exp
  bindFromCase x altsM = do
    alts <- sequence altsM
    bindVal (flip ECase alts) x

  bindFromApp :: Name -> [SimpleVal] -> NameM Exp
  bindFromApp f args = do
    nodeVar <- fmap Var newNodeName
    return $ SBlock $ EBind (SApp f args) nodeVar (SReturn nodeVar)
