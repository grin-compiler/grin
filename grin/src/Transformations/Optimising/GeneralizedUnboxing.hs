{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Transformations.Optimising.GeneralizedUnboxing where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.List
import Data.Function
import Data.Functor.Foldable as Foldable
import Data.Functor.Infix
import qualified Data.Vector as Vector
import Grin
import TypeEnv
import Pretty
import Control.Applicative
import Lens.Micro.Platform
import Debug.Trace


generalizedUnboxing :: (TypeEnv, Exp) -> (TypeEnv, Exp)
generalizedUnboxing te@(typeEnv, exp) = (typeEnv, newExp) where
  unboxFuns = functionsToUnbox te
  -- TODO: Unify the CoAlgebras based of transformCall and transformReturn
  newExp = transformCalls unboxFuns $ (,) typeEnv $ transformReturns unboxFuns (typeEnv, exp)

-- TODO: Support tagless nodes.

tailCalls :: Exp -> Maybe [Name]
tailCalls = cata collect where
  collect :: ExpF (Maybe [Name]) -> Maybe [Name]
  collect = \case
    DefF _ _ result   -> result
    EBindF _ _ result -> result
    ECaseF _ alts
      | all isJust alts -> nub <$> mconcat alts
      | otherwise       -> Nothing
    AltF _ result -> result
    SAppF f _     -> Just [f]
    e -> Nothing

doesReturnAKnownProduct :: TypeEnv -> Name -> Bool
doesReturnAKnownProduct = isJust <$$> returnsAUniqueTag

returnsAUniqueTag :: TypeEnv -> Name -> Maybe Tag
returnsAUniqueTag te name = do
  (tag, vs) <- te ^? function . at name . _Just . _1 . _T_NodeSet . to Map.toList . to singleton . _Just
  typ       <- singleton (Vector.toList vs)
  pure tag

singleton :: [a] -> Maybe a
singleton = \case
  []  -> Nothing
  [a] -> Just a
  _   -> Nothing

-- TODO: Remove the fix combinator, explore the function
-- dependency graph and rewrite disqualify steps based on that.
functionsToUnbox :: (TypeEnv, Exp) -> [Name]
functionsToUnbox (te, Program defs) = result where
  funName (Def n _ _) = n

  tailCallsMap :: Map Name [Name]
  tailCallsMap = Map.fromList $ catMaybes $ map (\e -> (,) (funName e) <$> tailCalls e) $ defs

  result = step initial
  initial = map funName $ filter (doesReturnAKnownProduct te . funName) defs
  disqualify candidates = filter
    (\candidate -> case Map.lookup candidate tailCallsMap of
      Nothing    -> True
      Just calls -> all (`elem` candidates) calls)
    candidates
  step = fix $ \rec x0 ->
    let x1 = disqualify x0 in
    if x0 == x1
      then x0
      else rec x1

transformReturns :: [Name] -> (TypeEnv, Exp) -> Exp
transformReturns toUnbox (te, exp) = apo builder (Nothing, exp) where
  builder :: (Maybe Tag, Exp) -> ExpF (Either Exp (Maybe Tag, Exp))
  builder (mtag, exp0) = case exp0 of
    Def name params body
      | name `elem` toUnbox -> DefF name params (Right (returnsAUniqueTag te name, body))
      | otherwise           -> DefF name params (Left body)

    -- Remove the tag from the value
    EBind lhs pat (SReturn (ConstTagNode tag [val]))
      -> EBindF (Left lhs) pat (Left (SReturn val))

    -- Rewrite a node variable
    -- TODO: Unique variable name
    -- TODO: Extract the tag
    EBind lhs pat (SReturn (Var v))
      -> EBindF
          (Left lhs)
          pat
          (Left (EBind
            (SReturn (Var v))
            (ConstTagNode (fromJust mtag) [(Var $ v ++ "'")])
            (SReturn (Var $ v ++ "'"))))
          -- fromJust works, as when we enter the processing of body of the
          -- expression only happens with the provided tag.

    -- Always skip the lhs of a bind.
    EBind lhs pat rhs -> EBindF (Left lhs) pat (Right (mtag, rhs))

    rest -> Right . (,) mtag <$> project rest

transformCalls :: [Name] -> (TypeEnv, Exp) -> Exp
transformCalls toUnbox (typeEnv, exp) = ana builder exp where
  builder :: Exp -> ExpF Exp
  builder = \case
    Def name params body
      | name `elem` toUnbox -> DefF (name ++ "'") params body
      | otherwise           -> DefF name params body

    -- TODO: Unique name
    EBind lhs@(SApp name params) ctag@(ConstTagNode (Tag C tag) [Var x]) rhs
      | name `elem` toUnbox ->
          EBindF
            (SBlock (EBind
              (SApp (name ++ "'") params)
              (Var $ x ++ "'")
              (SReturn (ConstTagNode (Tag C tag) [Var $ x ++ "'"]))
              ))
            ctag
            rhs

    -- TODO: Unique name
    EBind lhs@(SApp name params) (Var x) rhs
      | name `elem` toUnbox ->
          EBindF
            (SBlock (EBind
              (SApp (name ++ "'") params)
              (Var $ x ++ "'")
              (SReturn (ConstTagNode (fromJust $ returnsAUniqueTag typeEnv name) $ [Var $ x ++ "'"]))
              ))
            (Var x)
            rhs


    -- Tailcalls do not need a transform
    EBind lhs pat (SApp name params)
      | name `elem` toUnbox ->
          EBindF lhs pat (SApp (name ++ "'") params)

    rest -> project rest
