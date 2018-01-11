{-# LANGUAGE DeriveFunctor, TemplateHaskell, LambdaCase #-}
module CESKMachine where

import Data.Functor.Foldable as Foldable
import Control.Monad.State
import Data.Map hiding (update)
import Data.Maybe
import Data.Monoid
import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Grin

data HoleF v f g e
  = H
  | E (f e)
  | R v
  | V (g e)
  | VH (f e) -- Represent a hole that supposed to be in a place of val
  deriving (Eq, Show)

type Hole  = Fix (HoleF Val ExpF ValF)

-- There should be at most one Hole, and it should be
-- on the first level, not deeper.
type Frame = Hole

example1, example2 :: Hole
example1 = Fix (E (SReturnF (Lit (LInt 1)))) -- ~ return 1
example2 = Fix (E (EBindF (Fix (R (Lit (LInt 1)))) (Var "pat") (Fix H))) -- ~ 1; \pat -> #

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

bindPattern :: LPat -> Val -> Machine ()
bindPattern = undefined

step :: Machine ()
step = do
  f <- pop
  case f of
    (Fix H)     -> error "impossible"
    (Fix (R v)) -> undefined
    (Fix (E e)) -> stepOnExp e

-- Assumption: The stack is not empty.
stepOnValue :: Val -> Machine ()
stepOnValue v = do
  (Fix (E e)) <- pop
  case e of
    EBindF (Fix H) lpat (Fix (E e)) -> do
      bindPattern lpat v
      push (Fix (E e))

    ECaseF val as | or [ True | Fix H <- as ] -> do
      push (Fix (R v))

    SBlockF (Fix H) -> do
      push (Fix (R v))

    AltF cpat (Fix H) -> do
      push (Fix (R v))

    rest -> pure ()

-- TODO: It should be part of the abstract interpretation.
selectAlternatives :: Val -> [(CPat, a)] -> (a -> a) -> (a -> b) -> [((CPat, a), Maybe b)]
selectAlternatives v f pats = undefined

-- | push
stepOnExp :: ExpF Hole -> Machine ()
stepOnExp = \case

  EBindF (Fix (E se)) pat (Fix (E e)) -> do
    push (Fix (E (EBindF (Fix H) pat (Fix (E e)))))
    push (Fix (E se))

  ECaseF val alts -> do
    -- TODO: There should be one frame for ValF and one for ExpF
    -- TODO: Case selection should be abstract, and collection of results
    -- should be unified in some way.

    let alts1 = (\(Fix (E (AltF cpat a))) -> (cpat, a)) <$> alts
        (alts2, actions) = unzip $ selectAlternatives val alts1 (const (Fix H)) push
        alts3 = (\(cpat, a) -> Fix (E (AltF cpat a))) <$> alts2
    push (Fix (E (ECaseF val alts3)))
    sequence_ $ catMaybes actions -- pushes a frame for the selected node

  -- Simple Expr: Does not contain any holes.
  SAppF name simpleVal ->
    undefined

  SReturnF val ->
    -- TODO: Val should be part of the hole stack.
    push (Fix (R val))

  SStoreF val -> do
    loc <- newloc val
    push (Fix (R (Loc loc)))

  SFetchIF name pos -> do
    (Loc loc) <- getEnv name
    val <- fetch loc
    push (Fix (R (Loc loc)))

  SUpdateF name val -> do
    (Loc loc) <- getEnv name
    update loc val
    push (Fix (R Unit))

  -- Alt
  AltF pat (Fix (E e)) -> do
    push (Fix (E (AltF pat (Fix H))))
    push (Fix (E e))
