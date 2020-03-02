{-# LANGUAGE LambdaCase, TupleSections, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.CSE where

-- HINT: common sub-expression elimination

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Functor.Foldable as Foldable

import Text.Printf

import Lens.Micro ((^.))
import Lens.Micro.Extra (isn't)

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TypeEnv
import Grin.ExtendedSyntax.EffectMap
import Transformations.ExtendedSyntax.Util


type Env = (Map SimpleExp SimpleExp)

-- TODO: track if function parameters with location type can be updated in the called function to improve CSE

{- TODO: remove skipUnit, it does nothing with the new syntax (SDVE will get rid of the unused unit-binds)
   TODO: CSE could be taught to remember pattern binds:
   (CInt k1)@n0 <- pure (CInt k0)
   n1 <- pure (CInt k0)
   n2 <- pure (CInt k1)

   could be transformed to:

   (CInt k1)@n0 <- pure (CInt k0)
   n1 <- pure n0
   n2 <- pure n0
-}
commonSubExpressionElimination :: TypeEnv -> EffectMap -> Exp -> Exp
commonSubExpressionElimination typeEnv effMap e = hylo skipUnit builder (mempty, e) where

  builder :: (Env, Exp) -> ExpF (Env, Exp)
  builder (env, subst env -> exp) = case exp of
    EBind leftExp bPat rightExp -> EBindF (env, leftExp) bPat (newEnv, rightExp) where
      newEnv = case leftExp of
        -- HINT: also save fetch (the inverse operation) for store and update
        SUpdate ptr var -> Map.insert (SFetch ptr) (SReturn (Var var)) env
        SStore var
          -- TODO: AsPat
          | VarPat ptr <- bPat -> Map.insert (SFetch ptr) (SReturn (Var var)) extEnvKeepOld
        -- HINT: location parameters might be updated in the called function, so forget their content
        SApp defName args -> foldr
          Map.delete
          (if (hasTrueSideEffect defName effMap) then env else extEnvKeepOld)
          [SFetch var | var <- args, isLocation var]
        SReturn val | isn't _Var val  -> extEnvKeepOld
        SFetch{}  -> extEnvKeepOld
        _         -> env

      extEnvKeepOld = Map.insertWith (\new old -> old) leftExp (SReturn . Var $ bPat ^. _BPatVar) env

    -- TODO: Investigate this. Will the fetched variable, and the variable to be updated with
    -- always have the same name? If not, will copy propagation solve it?
    SUpdate ptr var | Just (SReturn (Var fetchedVar)) <- Map.lookup (SFetch ptr) env
                    , fetchedVar == var
                    -> SReturnF Unit

    ECase scrut alts -> ECaseF scrut [(altEnv env scrut cpat, alt) | alt@(Alt cpat _altName _) <- alts]

    _ -> (env,) <$> project exp

  isLocation :: Name -> Bool
  isLocation name = case variableType typeEnv name of
    T_SimpleType T_Location{} -> True
    _ -> False

  altEnv :: Env -> Name -> CPat -> Env
  altEnv env scrut cpat = case cpat of
    NodePat tag args  -> env -- When we use scrutinee variable already HPT will include all the
                             -- possible values, instead of the matching one. As result it will
                             -- overapproximate the values more than needed.

                             -- NOTE: We could extend the env with [ SReturn (ConstTagNode tag args) -> SReturn val ]
                             -- HPT would _not_ overapproximate the possible type of the variable,
                             -- since it restricts the scrutinee to the alternative's domain
    LitPat lit        -> Map.insertWith (\new old -> old) (SReturn (Lit lit)) (SReturn (Var scrut)) env
    DefaultPat        -> env

