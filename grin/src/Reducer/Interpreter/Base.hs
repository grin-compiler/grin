{-# LANGUAGE LambdaCase, GeneralizedNewtypeDeriving, InstanceSigs, TypeFamilies, TemplateHaskell, ScopedTypeVariables, DataKinds #-}
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

eval :: (Interpreter m, MonadIO m, Show v, v ~ Val m) => Exp -> m v
eval = fix baseEval

-- Open recursion and monadic interpreter.
baseEval :: (MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
         => (Exp -> m (Val m)) -> Exp -> m (Val m)
baseEval ev0 = \case
  SReturn (Var n) -> do
    p <- askEnv
    pure $ Env.lookup p n

  -- TODO: Separate value and variable in the GRIN expressions
  SReturn v -> value v

  SApp fn ps -> do
    p  <- askEnv
    vs <- pure $ map (Env.lookup p) ps
    ex <- isExternal fn
    (if ex then external else funCall ev0) fn vs

  SFetch n -> do
    p <- askEnv
    let v = Env.lookup p n
    fetchStore v

  SUpdate nl nn -> do
    p <- askEnv
    let vl = Env.lookup p nl
    let vn = Env.lookup p nn
    extStore vl vn
    unit

  ECase n alts -> do
    p <- askEnv
    v <- pure $ Env.lookup p n
    -- Select the alternative and continue the evaluation
    evalCase ev0 v alts

  EBind (SStore n) (VarPat l) rhs -> do
    p <- askEnv
    let v = Env.lookup p n
    a  <- allocStore l
    extStore a v
    let p' = Env.insert l a p
    localEnv p' (ev0 rhs)

  EBind lhs (VarPat n) rhs -> do
    v <- ev0 lhs
    p <- askEnv
    let p' = Env.insert n v p
    localEnv p' (ev0 rhs)

  EBind lhs (AsPat t@(Tag{}) vs n) rhs -> do
    v   <- ev0 lhs
    p   <- askEnv
    p'  <- flip Env.inserts p <$> bindPattern v (t,vs)
    let p'' = Env.insert n v p'
    localEnv p'' (ev0 rhs)

  Alt _name _pat body -> do
    ev0 body

  overGenerative -> error $ show overGenerative

-- Type class

class (Monad m, MonadFail m) => Interpreter m where
  type Val     m :: * -- Values that can be placed in registers/variables
  type HeapVal m :: * -- Values for the Store, Fetch, Update parameters
  type Addr    m :: * -- A type to represent an Address

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
  lookupFun     :: Name -> m Exp
  isExternal    :: Name -> m Bool
  external      :: Name -> [Val m] -> m (Val m)

  -- Control-flow
  evalCase      :: (Exp -> m (Val m)) -> Val m -> [Alt] -> m (Val m)
  funCall       :: (Exp -> m (Val m)) -> Name -> [Val m] -> m (Val m)

  -- Store
  allocStore    :: Name -> m (Val m)
  fetchStore    :: Val m -> m (Val m)      -- TODO: Change this to Addr m??
  extStore      :: Val m -> Val m -> m ()  --

programToDefs :: Exp -> Map Name Exp
programToDefs = \case
  (Program _ defs) -> fromList ((\d@(Def n _ _) -> (n,d)) <$> defs)
  _                -> mempty


{-
evalExp
  :: (MonadFail m, MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
  => Exp -> m (Val m)
evalExp = para baseEvalF

-- Open recursion and monadic interpreter.
baseEvalF :: (MonadFail m, MonadIO m, Interpreter m, a ~ Addr m, v ~ Val m, Show v)
         => {- (ExpF (m (Val m)) -> m (Val m)) -> -} ExpF (Exp, m (Val m)) -> m (Val m)
baseEvalF = \case
  SReturnF (Var n) -> do
    p <- askEnv
    pure $ Env.lookup p n

  -- TODO: Separate value and variable in the GRIN expressions
  SReturnF v -> value v

  SAppF fn ps -> do
    p  <- askEnv
    vs <- pure $ map (Env.lookup p) ps
    ex <- isExternal fn
--    (if ex then external else funCall evalExp) fn vs
    (if ex then external else funCallF) fn vs
    -- TODO: For a defined GRIN function

  SFetchF n -> do
    p <- askEnv
    let v = Env.lookup p n
    fetchStore v

  SUpdateF nl nn -> do
    p <- askEnv
    let vl = Env.lookup p nl
    let vn = Env.lookup p nn
    extStore vl vn
    unit

  ECaseF n alts -> do
    p <- askEnv
    v <- pure $ Env.lookup p n
    -- Select the alternative and continue the evaluation
    evalCaseF v alts

  EBindF (SStore n, _) (VarPat l) (_, rhs) -> do
    p <- askEnv
    let v = Env.lookup p n
    a  <- allocStore l
    extStore a v
    let p' = Env.insert l a p
    localEnv p' rhs

  EBindF (_, lhs) (VarPat n) (_, rhs) -> do
    v <- lhs
    p <- askEnv
    let p' = Env.insert n v p
    localEnv p' rhs

  EBindF (_, lhs) (AsPat t@(Tag{}) vs n) (_, rhs) -> do
    v   <- lhs
    p   <- askEnv
    p'  <- flip Env.inserts p <$> bindPattern v (t,vs)
    let p'' = Env.insert n v p'
    localEnv p'' rhs

  AltF _name _pat (_, body) -> do
    body

  SBlockF (_, body) -> body

  overGenerative -> error "over-generative"

-}
