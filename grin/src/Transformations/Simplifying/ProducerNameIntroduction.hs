{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Transformations.Simplifying.ProducerNameIntroduction where

import Control.Monad
import Data.Functor.Foldable as Foldable

import Grin.Grin
import Transformations.Names


newNodeName :: NameM Name
newNodeName = deriveNewName "v"

{-
  This transformation can invalidate the "no left bind" invariant,
  so we have to normalize these incorrect bindings after this transformation.

  NOTE: Binding from SApp arguments is not necessary,
        because the syntax does not allow node values to be present
        in function application arguments.
-}
producerNameIntroduction :: Exp -> (Exp, ExpChanges)
producerNameIntroduction e = evalNameM e . cata alg $ e where
  alg :: ExpF (NameM Exp) -> NameM Exp
  alg e = case e of
    SStoreF    x@VarTagNode{}   -> bindVal SStore x
    SStoreF    x@ConstTagNode{} -> bindVal SStore x
    SStoreF    x@Undefined{}    -> bindVal SStore x
    SUpdateF p x@VarTagNode{}   -> bindVal (SUpdate p) x
    SUpdateF p x@ConstTagNode{} -> bindVal (SUpdate p) x
    SUpdateF p x@Undefined{}    -> bindVal (SUpdate p) x
    SReturnF   x@VarTagNode{}   -> bindVal SReturn x
    SReturnF   x@ConstTagNode{} -> bindVal SReturn x
    SReturnF   x@Undefined{}    -> bindVal SReturn x
    expf -> fmap embed . sequence $ expf

  -- binds a Val (usually a node) to a name, then puts it into some context
  bindVal :: (Val -> Exp) -> Val -> NameM Exp
  bindVal context val = do
    nodeVar <- fmap Var newNodeName
    return $ SBlock $ EBind (SReturn val) nodeVar (context nodeVar)
