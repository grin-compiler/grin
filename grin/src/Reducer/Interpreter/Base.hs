{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Reducer.Interpreter.Base
  ( module Reducer.Interpreter.Base
  ) where

import Control.Monad.Fail
import Control.Monad.Trans (MonadIO)
import Data.Function (fix)
import Data.Map.Strict (Map, fromList)
import Grin.ExtendedSyntax.Syntax hiding (Val)
import Data.Functor.Foldable
import Reducer.Interpreter.Env (Env)
import Data.Functor.Sum

import qualified Reducer.Interpreter.Env as Env
import qualified Grin.ExtendedSyntax.Syntax as Grin (Val)


-- * Interpreter

eval :: (Interpreter m, MonadIO m, Show v, v ~ Val m)
     => (forall a . Expr m a -> m v)
     -> Fix (Sum ExpF (Expr m)) -> m v
eval ev = fix (baseEval ev)

-- Open recursion and monadic interpreter.
baseEval :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
         => (forall b . Expr m b -> m v)
         -> (Fix (Sum ExpF (Expr m)) -> m (Val m))
         -> Fix (Sum ExpF (Expr m)) -> m (Val m)
baseEval evExpr ev0 = \case
  Fix (InL (SReturnF (Var n))) -> do
    p <- getEnv
    pure $ Env.lookup p n

  -- TODO: Separate value and variable in the GRIN expressions
  Fix (InL (SReturnF v)) -> value v

  Fix (InL (SAppF fn ps)) -> do
    p  <- getEnv
    vs <- pure $ map (Env.lookup p) ps
    ex <- isExternal fn
    if ex
      then callExternal fn vs
      else do
        (env, body) <- functionCall fn vs
        localEnv env $ ev0 body

  Fix (InL (SFetchF n)) -> do
    p <- getEnv
    let v = Env.lookup p n
    a <- valToAddr v
    n <- fetch a
    nodeToVal n

  Fix (InL (SUpdateF nl nn)) -> do
    p <- getEnv
    let vl = Env.lookup p nl
    let vn = Env.lookup p nn
    a <- valToAddr vl
    n <- valToNode vn
    update a n
    unit

  Fix (InL (ECaseF n alts)) -> do
    p <- getEnv
    v <- pure $ Env.lookup p n
    (env, alt) <- matchingVal v alts
    localEnv (Env.insertEnv p env) (ev0 alt)

  Fix (InL (EBindF (Fix (InL (SStoreF n))) (VarPat l) rhs)) -> do
    p <- getEnv
    let v = Env.lookup p n
    a <- store l
    n <- valToNode v
    update a n
    va <- addrToVal a
    let p' = Env.insert l va p
    localEnv p' (ev0 rhs)

  Fix (InL (EBindF lhs (VarPat n) rhs)) -> do
    v <- ev0 lhs
    p <- getEnv
    let p' = Env.insert n v p
    localEnv p' (ev0 rhs)

  Fix (InL (EBindF lhs (AsPat t@(Tag{}) vs n) rhs)) -> do
    v   <- ev0 lhs
    p   <- getEnv
    p'  <- flip Env.inserts p <$> bindPattern v (t,vs)
    let p'' = Env.insert n v p'
    localEnv p'' (ev0 rhs)

  Fix (InL (AltF _name _pat body)) -> do
    ev0 body

  (Fix (InR e)) -> evExpr e

  _overGenerative -> error "overGenerative"

-- Type class

class (Monad m, MonadFail m) => Interpreter m where
  type Expr m :: * -> *

  type Val  m :: * -- Values that can be placed in registers/variables
  type Node m :: * -- Values for the Store, Fetch, Update parameters
  type Addr m :: * -- A type to represent an Address

  -- Conversions, but m type is needed for type inference
  value       :: Grin.Val -> m (Val  m)  -- Value of the given literal: Only applicable to ConstTagNode, Unit and Lit
  valToAddr   :: Val  m   -> m (Addr m)
  addrToVal   :: Addr m   -> m (Val  m)
  nodeToVal   :: Node m   -> m (Val  m)
  valToNode   :: Val  m   -> m (Node m)
  unit        :: m (Val m) -- The unit value
  bindPattern :: Val m -> (Tag, [Name]) -> m [(Name, Val m)]

  -- Environment
  getEnv        :: m (Env (Val m))
  localEnv      :: Env (Val m) -> m (Val m) -> m (Val m)

  -- Function call
  isExternal    :: Name -> m Bool
  callExternal  :: Name -> [Val m] -> m (Val m)
  functionCall  :: Name -> [Val m] -> m (Env (Val m), Fix (Sum ExpF (Expr m)))

  -- Case
  matchingVal   :: Val m -> [Fix (Sum ExpF (Expr m))] -> m (Env (Val m), Fix (Sum ExpF (Expr m)))

  -- Heap
  store  :: Name -> m (Addr m)
  fetch  :: Addr m -> m (Node m)
  update :: Addr m -> Node m -> m ()

-- * Helpers

toExprF :: Exp -> Fix (Sum ExpF e)
toExprF = cata (Fix . InL)

programToDefs :: (Fix (Sum ExpF e)) -> Map Name (Fix (Sum ExpF e))
programToDefs = \case
  (Fix (InL (ProgramF _ defs))) -> fromList ((\d@(Fix (InL (DefF n _ _))) -> (n,d)) <$> defs)
  _                             -> mempty

data Void a

instance Functor Void where
  fmap _ = \case
