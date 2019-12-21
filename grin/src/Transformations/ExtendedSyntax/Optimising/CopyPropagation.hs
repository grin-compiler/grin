{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.CopyPropagation where

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
copyPropagation e = hylo rmBlocks builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env@(origVals, aliases), exp) = let e = substVarRefExp aliases $ exp in case e of
    -- left unit law
    EBind (SReturn (Var valVar)) (VarPat patVar) rightExp
      | origVar <- getAlias valVar aliases
      -> let aliases' = Map.insert patVar origVar aliases
             newEnv   = (origVals, aliases')
         in SBlockF (newEnv, rightExp)

    -- rename lhs variables with their original aliases
    EBind (SReturn val) bpat@(VarPat patVar) rightExp
      | isn't _Lit val
      , valWithOrigVars <- substNamesVal aliases val
      -> let origVals' = Map.insert patVar valWithOrigVars origVals
             newEnv    = (origVals', aliases)
         in (newEnv,) <$> project (EBind (SReturn valWithOrigVars) bpat rightExp)

    -- left unit law + eliminate redundant rebinds
    EBind (SReturn (Var valVar)) (AsPat patVar asPat) rightExp
      | origVar <- getAlias valVar aliases
      , origVal <- getOrigVal origVar origVals
      , ConstTagNode patTag patArgs <- asPat
      , ConstTagNode valTag valArgs <- origVal
      , patTag == valTag
      -> let aliases' = aliases <> (Map.fromList $ zip (patVar:patArgs) (origVar:valArgs))
             newEnv   = (origVals, aliases')
         in SBlockF (newEnv, rightExp)

    -- rename lhs variables with their original aliases
    -- and eliminate redudant rebinds
    EBind (SReturn val) (AsPat patVar asPat) rightExp
      | isn't _Lit val
      , valWithOrigVars <- substNamesVal aliases val
      , ConstTagNode patTag patArgs <- asPat
      , ConstTagNode valTag valArgs <- valWithOrigVars
      , patTag == valTag
      -> let origVals' = Map.insert patVar valWithOrigVars origVals
             aliases'  = aliases <> (Map.fromList $ zip patArgs valArgs)
             newEnv    = (origVals', aliases')
         in (newEnv,) <$> project (EBind (SReturn val) (VarPat patVar) rightExp)

    -- simplifying as-patterns matching against the same basic value they bind
    EBind (SReturn retVal) (AsPat var patVal) rightExp
      | isBasicValue retVal
      , retVal == patVal
      -> (env,) <$> project (EBind (SReturn retVal) (VarPat var) rightExp)

    _ -> (env,) <$> project e

  -- NOTE: This cleans up the left-over produced by the above transformation.
  -- It removes nested blocks, and blocks appearing on the left-hand side of a
  -- binding. These are always safe to remove.
  rmBlocks :: ExpF Exp -> Exp
  rmBlocks = \case
    EBindF lhs bpat (SBlock rhs) -> EBind lhs bpat rhs
    SBlockF exp@SBlock{}         -> exp
    exp                          -> embed exp

getAlias :: Name -> Aliases -> Name
getAlias var aliases = Map.findWithDefault var var aliases

getOrigVal :: Name -> OriginalValues -> Val
getOrigVal var origVals = Map.findWithDefault (Var var) var origVals
