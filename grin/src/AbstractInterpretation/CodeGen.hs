{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.CodeGen where

import Data.Bimap as Bimap
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Grin

data Env
  = Env
  { envMemoryCounter    :: Int
  , envRegisterCounter  :: Int
  , envRegisterMap      :: Bimap Name Int
  }

emptyEnv = Env
  { envMemoryCounter    = 0
  , envRegisterCounter  = 0
  , envRegisterMap      = Bimap.empty
  }

type CG = State Env

type Result = ()

codeGen :: Exp -> Env
codeGen = flip execState emptyEnv . cata folder where
  folder :: ExpF (CG Result) -> CG Result
  folder = \case
    ProgramF defs -> sequence_ defs
    DefF name args body -> pure ()
    -- Exp
    EBindF leftExp lpat rightExp -> pure ()
    ECaseF val alts -> pure ()
    -- Simple Expr
    SAppF name args -> pure ()
    SReturnF val -> pure ()
    SStoreF val -> pure ()
    SFetchIF name maybeIndex -> pure ()
    SUpdateF name val -> pure ()
    SBlockF exp -> exp
    -- Alt
    AltF cpat exp -> pure ()

{-
  TODO
    - build values ; fully constant value; partially constant value
    - avoid temporal variables ; fuse construction with deconstruction
-}

{-
  >>= LPAT
    (Tag b c d)               - node only check, arity check, tag check, copy node items to registers for a specific tag
    (a   b c d)               - node only check, arity check, copy node items to registers for all tags
    Tag                       - specific tag only check
    Unit                      - specific simple type only check
    Literal with simple type  - specific simple type only check
    variable                  - copy to reg

  case >>= LPAT ()
  return >>= LPAT ()
  store >>= LPAT
  fetch >>= LPAT
  update >>= LPAT

  example: return (a b c) >>= \(d e f) ->

 COMPILE TIME CHECK:
  VAL / LPAT
  T a - T a   OK      (node)
      - t a   OK
      - T     FAIL
      - Unit  FAIL
      - Lit   FAIL
      - a     OK

  t a - T a   OK      (node)
      - t a   OK
      - T     FAIL
      - Unit  FAIL
      - Lit   FAIL
      - a     OK

  T   - T a   FAIL    (tag)
      - t a   FAIL
      - T     OK
      - Unit  FAIL
      - Lit   FAIL
      - a     OK

  Unit- T a   FAIL    (simple type)
      - t a   FAIL
      - T     FAIL
      - Unit  OK
      - Lit   FAIL
      - a     OK

  Lit - T a   FAIL    (simple type)
      - t a   FAIL
      - T     FAIL
      - Unit  FAIL
      - Lit   OK ; if matches
      - a     OK

  a   - T a   OK      (any)
      - t a   OK
      - T     OK
      - Unit  OK
      - Lit   OK
      - a     OK

  LOC - T a   FAIL    (heap location)
      - t a   FAIL
      - T     FAIL
      - Unit  FAIL
      - Lit   FAIL
      - a     OK

  compilation:
    store (the only valid expression): store VAL >>= var
      emits:
        one time constant register setup for var
        memory copy or one time setup
        VAL validation
    update DST VAL >>= (var | Unit)
      VAL validation
      DST validaton
      var: emits one time setup for var ; fill with Unit type
      Unit: nothing to do
    fetch SRC >>= (var | (T a) | (t a))
      SRC validation
      var: CopyMemReg
      (T a): 

-}

{-
 COMPILE TIME CHECK:
  VAL / CPAT
  T a - T a   OK      (node)
      - T     FAIL
      - Lit   FAIL

  t a - T a   OK      (node)
      - T     FAIL
      - Lit   FAIL

  T   - T a   FAIL    (tag)
      - T     OK
      - Lit   FAIL

  Unit- T a   FAIL    (simple type)
      - T     FAIL
      - Lit   FAIL

  Lit - T a   FAIL    (simple type)
      - T     FAIL
      - Lit   OK ; if matches

  a   - T a   OK      (any)
      - T     OK
      - Lit   OK

  LOC - T a   FAIL    (heap location)
      - T     FAIL
      - Lit   FAIL
-}
