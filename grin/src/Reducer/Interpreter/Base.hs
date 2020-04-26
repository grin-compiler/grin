{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables, DataKinds #-}
{-# LANGUAGE TypeOperators, DeriveFunctor, ViewPatterns, EmptyCase, RankNTypes #-}
module Reducer.Interpreter.Base
  ( module Reducer.Interpreter.Base
  ) where

import Control.Monad.Fail
import Control.Monad.Trans (MonadIO)
import Data.Function (fix)
import Data.Map.Strict (Map(..), fromList)
import Grin.ExtendedSyntax.Syntax hiding (Val)
import Data.Functor.Foldable
-- import Grin.Value hiding (Val)

import Reducer.Interpreter.Env (Env)
import qualified Reducer.Interpreter.Env as Env
import qualified Grin.ExtendedSyntax.Syntax as Grin (Val)
import Transformations.ExtendedSyntax.Util (anaM)


-- * Interpreter

data (:+:) f g a = Inl (f a) | Inr (g a)
  deriving Functor

data Void a

instance Functor Void where
  fmap f = \case

eval :: (Interpreter m, MonadIO m, Show v, v ~ Val m)
     => (forall a . Expr m a -> m v)
     -> Fix (ExpF :+: Expr m) -> m v
eval ev = fix (baseEval ev)

-- Open recursion and monadic interpreter.
baseEval :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
         => (forall b . Expr m b -> m v)
         -> (Fix (ExpF :+: Expr m) -> m (Val m))
         -> Fix (ExpF :+: Expr m) -> m (Val m)
baseEval evExpr ev0 = \case
  Fix (Inl (SReturnF (Var n))) -> do
    p <- askEnv
    pure $ Env.lookup p n

  -- TODO: Separate value and variable in the GRIN expressions
  Fix (Inl (SReturnF v)) -> value v

  Fix (Inl (SAppF fn ps)) -> do
    p  <- askEnv
    vs <- pure $ map (Env.lookup p) ps
    ex <- isExternal fn
    if ex
      then external fn vs
      else do
        (env, body) <- funCall fn vs
        localEnv env $ ev0 body

  Fix (Inl (SFetchF n)) -> do
    p <- askEnv
    let v = Env.lookup p n
    fetchStore v

  Fix (Inl (SUpdateF nl nn)) -> do
    p <- askEnv
    let vl = Env.lookup p nl
    let vn = Env.lookup p nn
    extStore vl vn
    unit

  Fix (Inl (ECaseF n alts)) -> do
    p <- askEnv
    v <- pure $ Env.lookup p n
    (env, alt) <- matchingVal v alts
    localEnv (Env.insertEnv p env) (ev0 alt)

  Fix (Inl (EBindF (Fix (Inl (SStoreF n))) (VarPat l) rhs)) -> do
    p <- askEnv
    let v = Env.lookup p n
    a  <- allocStore l
    extStore a v
    let p' = Env.insert l a p
    localEnv p' (ev0 rhs)

  Fix (Inl (EBindF lhs (VarPat n) rhs)) -> do
    v <- ev0 lhs
    p <- askEnv
    let p' = Env.insert n v p
    localEnv p' (ev0 rhs)

  Fix (Inl (EBindF lhs (AsPat t@(Tag{}) vs n) rhs)) -> do
    v   <- ev0 lhs
    p   <- askEnv
    p'  <- flip Env.inserts p <$> bindPattern v (t,vs)
    let p'' = Env.insert n v p'
    localEnv p'' (ev0 rhs)

  Fix (Inl (AltF _name _pat body)) -> do
    ev0 body

  other@(Fix (Inr e)) -> evExpr e

  overGenerative -> error "overGenerative"

-- Type class

class (Monad m, MonadFail m) => Interpreter m where
  type Val     m :: * -- Values that can be placed in registers/variables
  type HeapVal m :: * -- Values for the Store, Fetch, Update parameters
  type Addr    m :: * -- A type to represent an Address
  type Expr    m :: * -> *

  -- Conversions, but m type is needed for type inference
  value       :: Grin.Val     -> m (Val m)  -- Value of the given literal: Only applicable to ConstTagNode, Unit and Lit
  val2addr    :: Val m        -> m (Addr m) --
  addr2val    :: Addr m       -> m (Val m)
  heapVal2val :: HeapVal m    -> m (Val m)
  val2heapVal :: Val m        -> m (HeapVal m)
  unit        :: m (Val m) -- The unit value
  bindPattern :: Val m -> (Tag, [Name]) -> m [(Name, Val m)]

  -- Non-pure

  -- | Return the computational environment
  askEnv        :: m (Env (Val m))
  -- | Set the local environment
  localEnv      :: Env (Val m) -> m (Val m) -> m (Val m)
  isExternal    :: Name -> m Bool
  external      :: Name -> [Val m] -> m (Val m)

  matchingVal :: Val m -> [Fix (ExpF :+: Expr m)] -> m (Env (Val m), Fix (ExpF :+: Expr m))
  funCall     :: Name -> [Val m] -> m (Env (Val m), Fix (ExpF :+: Expr m))

  -- Store
  allocStore    :: Name -> m (Val m)
  fetchStore    :: Val m -> m (Val m)
  extStore      :: Val m -> Val m -> m ()

toExprF :: Exp -> Fix (ExpF :+: e)
toExprF = cata (Fix . Inl)

programToDefs :: (Fix (ExpF :+: e)) -> Map Name (Fix (ExpF :+: e))
programToDefs = \case
  (Fix (Inl (ProgramF _ defs))) -> fromList ((\d@(Fix (Inl (DefF n _ _))) -> (n,d)) <$> defs)
  _                             -> mempty
