{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.CopyPropagation where

import Control.Monad.State.Strict

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable

import Text.Printf
import Lens.Micro.Extra

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Util

{-
  NOTE:
    Do not propagate literal values because literals are not used for optimisations. (GRIN is not a supercompiler)
    Only propagates variables. It does not cause performance penalty, LLVM will optimise the code further.

    CopyPropagation neither does replace literal values with variables (storing the same value),
    nor does it eliminate as-patterns matching a variables against a literal value (which is the same as the value stored by the variable).

  TODO:
    Is the as-pattern elimination handled by LLVM?
    We will figure this out after implementing the LLVM codegen for as-patterns.

  NOTE:
    Anamorphisms don't let us to "skip" bindings. We circumvent this issue by replacing the binding with a block.
    This will essentially skip the bind, since SBlockF is simply projected into SBlock. These extra (possibly nested)
    blocks will be removed in the cata part of the hylo.
-}

-- (k,v) ~ the variable k has the original value v
type OriginalValues = Map Name Val
-- (k,v) ~ the variable k aliases to (is a copy of) v
type Aliases        = Map Name Name

type Env = (OriginalValues, Aliases)

copyPropagation :: Exp -> Exp
copyPropagation = flip evalState mempty . hyloM rmBlocks builder where

  builder :: Exp -> State Env (ExpF Exp)
  builder exp = do
    (origVals, aliases) <- get
    -- This substitutes all the variables on this level with their original aliases
    let exp' = substVarRefExp aliases $ exp

    case exp' of
      -- left unit law
      EBind (SReturn (Var valVar)) (VarPat patVar) rightExp
        | origVar <- getAlias valVar aliases -> do
          let aliases' = Map.insert patVar origVar aliases
              newEnv   = (origVals, aliases')
          put newEnv
          pure $ SBlockF rightExp

      -- add the lhs value as an original value
      EBind (SReturn val) bpat@(VarPat patVar) rightExp
        | isn't _Lit val
        , valWithOrigVars <- substNamesVal aliases val -> do
           let origVals' = Map.insert patVar valWithOrigVars origVals
               newEnv    = (origVals', aliases)
           put newEnv
           pure $ project $ EBind (SReturn valWithOrigVars) bpat rightExp

      -- left unit law + eliminate redundant rebinds
      EBind (SReturn (Var valVar)) (AsPat patTag patArgs patVar) rightExp
        | origVar <- getAlias valVar aliases
        , origVal <- getOrigVal origVar origVals
        , ConstTagNode valTag valArgs <- origVal
        , patTag == valTag -> do
          let aliases' = aliases <> (Map.fromList $ zip (patVar:patArgs) (origVar:valArgs))
              newEnv   = (origVals, aliases')
          put newEnv
          pure $ SBlockF rightExp

      -- add the lhs value as an original value
      -- and eliminate redudant rebinds
      EBind (SReturn val) (AsPat patTag patArgs patVar) rightExp
        | isn't _Lit val
        , valWithOrigVars <- substNamesVal aliases val
        , ConstTagNode valTag valArgs <- valWithOrigVars
        , patTag == valTag -> do
          let origVals' = Map.insert patVar valWithOrigVars origVals
              aliases'  = aliases <> (Map.fromList $ zip patArgs valArgs)
              newEnv    = (origVals', aliases')
          put newEnv
          pure $ project $ EBind (SReturn val) (VarPat patVar) rightExp

      -- simplify as-pattern matching against the same node value it binds
      EBind (SReturn retVal@(ConstTagNode retTag retArgs)) (AsPat patTag patArgs var) rightExp
        | retTag  == patTag
        , retArgs == patArgs -> do
          pure $ project $ EBind (SReturn retVal) (VarPat var) rightExp

      _ -> pure $ project exp'

  -- NOTE: This cleans up the left-over produced by the above transformation.
  -- It removes nested blocks, and blocks appearing on the right-hand side of a
  -- binding. These are always safe to remove.
  rmBlocks :: ExpF Exp -> State Env Exp
  rmBlocks = \case
    EBindF lhs bpat (SBlock rhs) -> pure $ EBind lhs bpat rhs
    SBlockF exp@SBlock{}         -> pure $ exp
    exp                          -> pure $ embed exp

getAlias :: Name -> Aliases -> Name
getAlias var aliases = Map.findWithDefault var var aliases

getOrigVal :: Name -> OriginalValues -> Val
getOrigVal var origVals = Map.findWithDefault (Var var) var origVals
