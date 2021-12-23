--A normalised AST is
-- * Bind normalised
-- * The last statement of the function is pure v
-- * Node patterns only appear only when the lhs of the bind
--   is pure.

{-# LANGUAGE LambdaCase #-}
module Transformations.ExtendedSyntax.Normalisation where

import Data.Functor.Foldable
import Transformations.Util
import Grin.ExtendedSyntax.Syntax
import Transformations.ExtendedSyntax.BindNormalisation
import Transformations.ExtendedSyntax.Names



normalise :: Exp -> Exp
normalise e = fst $ evalNameM e $ do
  e1 <- restoreNodePattern $ bindNormalisation e
  restorePureAsLast e1

restorePureAsLast :: Exp -> NameM Exp
restorePureAsLast = apoM $ \case
  -- v <- lhs
  -- pure v
  EBind lhs pat (SReturn (Var v)) ->
    pure $ EBindF (Right lhs) pat (Left (SReturn (Var v)))

  -- v1 <- lhs1
  -- v2 <- ...
  EBind lhs pat rhs@(EBind{}) ->
    pure $ EBindF (Right lhs) pat (Right rhs)

  -- v <- lhs
  -- pure val / case / store / update / fetch / call
  EBind lhs pat rhs -> do
    x <- deriveNewName "rapl"
    -- v <- lhs
    -- x <- other
    -- pure x
    pure $ EBindF (Right lhs) pat (Right (EBind rhs (VarPat x) (SReturn (Var x))))

  -- fun params =
  --   v <- lhs
  --   rhs
  Def f ps body@(EBind{}) ->
    pure $ DefF f ps $ Right body

  -- fun params = pure / store / fetch / update / case
  Def f ps body -> do
    x <- deriveNewName "rapl"
    pure $ DefF f ps $ Right $ EBind body (VarPat x) (SReturn (Var x))

  -- every alt that has a single simple expression that is a program point
  Alt pat altName body | isNamedProgramPoint body -> do
    x <- deriveNewName "rapl"
    pure $ AltF pat altName $ Left $ EBind body (VarPat x) (SReturn (Var x))

  -- rrogram / def
  other -> pure $ fmap Right $ project other

isNamedProgramPoint :: Exp -> Bool
isNamedProgramPoint = \case
  SApp{}    -> True
  SStore{}  -> True
  SFetch{}  -> True
  SUpdate{} -> True
  _         -> False

restoreNodePattern :: Exp -> NameM Exp
restoreNodePattern = apoM $ \case
  -- x @ (tag ps) <- pure y
  -- rhs
  EBind (SReturn y) x@(AsPat{}) rhs -> do
    pure $ EBindF (Left (SReturn y)) x (Right rhs)

  -- v @ (tag ps) <- case / store / update / fetch / call
  -- rhs
  EBind lhs v@(AsPat{}) rhs -> do
    x <- deriveNewName "rnp"
    -- x <- case / store / update / fetch / call
    -- v @ (tag ps) <- pure x
    -- rhs
    pure $ EBindF (Right lhs)       (VarPat x) $ Right
         $ EBind  (SReturn (Var x)) v            rhs

  other -> pure $ fmap Right $ project other
