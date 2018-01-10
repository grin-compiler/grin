{-# LANGUAGE DeriveFunctor, TemplateHaskell, LambdaCase #-}
module CESKMachine where

import Data.Functor.Foldable as Foldable
import Control.Monad.State
import Data.Map hiding (update)
import Data.Monoid
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Grin

data HoleF v f e
  = H
  | E (f e)
  | V v
  deriving (Eq, Show)

type Hole  = Fix (HoleF Val ExpF)
type Frame = Hole

example1, example2 :: Hole
example1 = Fix (E (SReturnF (Lit (LInt 1)))) -- ~ return 1
example2 = Fix (E (EBindF (Fix (V (Lit (LInt 1)))) (Var "pat") (Fix H))) -- ~ 1; \pat -> #

type Stack = [Frame]

data MachineState = MS
  { _stack :: Stack
  , _env   :: Map Name Val
  , _store :: Map Int  Val
  }

makeLenses ''MachineState

type Machine a = State MachineState a

instance Monoid MachineState where
  mempty = MS mempty mempty mempty
  mappend (MS sk1 e1 st1) (MS sk2 e2 st2) = MS (sk1 <> sk2) (e1 <> e2) (st1 <> st2)

pop :: Machine Frame
pop = do
  f <- use (stack . to head)
  stack %= tail
  pure f

push :: Frame -> Machine ()
push f = stack %= (f:)

newloc :: Val -> Machine Int
newloc = undefined

fetch :: Int -> Machine Val
fetch = undefined

update :: Int -> Val -> Machine ()
update = undefined

getEnv :: Name -> Machine Val
getEnv = undefined

setEnv :: Name -> Val -> Machine ()
setEnv = undefined

bindPattern :: CPat -> Val -> Machine ()
bindPattern = undefined

step :: Machine ()
step = do
  f <- pop
  case f of
    (Fix H)     -> error "impossible"
    (Fix (V v)) -> undefined
    (Fix (E e)) -> stepOnExp e

stepOnExp :: ExpF Hole -> Machine ()
stepOnExp = \case

  EBindF (Fix (E se)) pat (Fix (E e)) -> do
    push (Fix (E (EBindF (Fix H) pat (Fix (E e)))))
    push (Fix (E se))

  EBindF (Fix (V v)) pat (Fix (E e)) -> do
    bindPattern pat v
    push (Fix (E e))

  ECaseF    val alts ->
    undefined

  -- Simple Expr: Does not contain any holes.
  SAppF     name simpleVal ->
    undefined

  SReturnF  val ->
    push (Fix (V val))

  SStoreF   val -> do
    loc <- newloc val
    push (Fix (V (Loc loc)))

  SFetchIF  name pos -> do
    (Loc loc) <- getEnv name
    val <- fetch loc
    push (Fix (V (Loc loc)))

  SUpdateF  name val -> do
    (Loc loc) <- getEnv name
    update loc val
    push (Fix (V Unit))

  -- Alt
  AltF pat e -> undefined
