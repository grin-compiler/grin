{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.GeneralizedUnboxing where

import Text.Printf
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.Function
import Data.Functor.Foldable as Foldable
import Data.Functor.Infix
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Grin
import TypeEnv
import Pretty
import Control.Applicative
import Lens.Micro.Platform
import Transformations.Util (anaM, apoM)
import Control.Monad.Writer
import Control.Arrow
import Debug.Trace


generalizedUnboxing :: (TypeEnv, Exp) -> (TypeEnv, Exp)
generalizedUnboxing te =
    (first (updateTypeEnv funs)) $
    transformCalls funs $
    transformReturns funs te
  where
    funs = functionsToUnbox te

-- TODO: Support tagless nodes.

tailCalls :: Exp -> Maybe [Name]
tailCalls = cata collect where
  collect :: ExpF (Maybe [Name]) -> Maybe [Name]
  collect = \case
    DefF _ _ result   -> result
    EBindF _ _ result -> result
    ECaseF _ alts -> nonEmpty $ concat $ catMaybes alts
    AltF _ result -> result
    SAppF f _     -> Just [f]
    e -> Nothing

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs

doesReturnAKnownProduct :: TypeEnv -> Name -> Bool
doesReturnAKnownProduct = isJust <$$> returnsAUniqueTag

returnsAUniqueTag :: TypeEnv -> Name -> Maybe (Tag, Type)
returnsAUniqueTag te name = do
  (tag, vs) <- te ^? function . at name . _Just . _1 . _T_NodeSet . to Map.toList . to singleton . _Just
  typ       <- singleton (Vector.toList vs)
  pure (tag, T_SimpleType typ)

singleton :: [a] -> Maybe a
singleton = \case
  []  -> Nothing
  [a] -> Just a
  _   -> Nothing

transitive :: (Ord a) => (a -> Set a) -> Set a -> Set a
transitive f res0 =
  let res1 = res0 `Set.union` (Set.unions $ map f $ Set.toList res0)
  in if res1 == res0
      then res0
      else transitive f res1

-- TODO: Remove the fix combinator, explore the function
-- dependency graph and rewrite disqualify steps based on that.
functionsToUnbox :: (TypeEnv, Exp) -> [Name]
functionsToUnbox (te, Program defs) = Set.toList result where
  funName (Def n _ _) = n

  tailCallsMap :: Map Name [Name]
  tailCallsMap = Map.fromList $ mapMaybe (\e -> (,) (funName e) <$> tailCalls e) defs

  tranisitiveTailCalls :: Map Name (Set Name)
  tranisitiveTailCalls = Map.fromList $ map (\k -> (k, transitive inTailCalls (Set.singleton k))) $ Map.keys tailCallsMap
    where
      inTailCalls :: Name -> Set Name
      inTailCalls n = maybe mempty Set.fromList $ Map.lookup n tailCallsMap

  nonCandidateTailCallMap = Map.withoutKeys tranisitiveTailCalls result0
  candidateCalledByNonCandidate = (Set.unions $ Map.elems nonCandidateTailCallMap) `Set.intersection` result0
  result = result0 `Set.difference` candidateCalledByNonCandidate

  result0 = Set.fromList $ step initial
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

updateTypeEnv :: [Name] -> TypeEnv -> TypeEnv
updateTypeEnv funs te = te & function %~ unboxFun
  where
    unboxFun = Map.fromList . map changeFun . Map.toList
    changeFun (n, ts@(ret, params)) =
      if n `elem` funs
        then (,) (n ++ "'")
          $ maybe ts ((\t -> (t, params)) . T_SimpleType) $
              ret ^? _T_NodeSet
                  . to Map.elems
                  . to singleton
                  . _Just
                  . to Vector.toList
                  . to singleton
                  . _Just
        else (n, ts)

type VarM a = Writer [(Name, Type)] a

runVarM :: TypeEnv -> VarM a -> (TypeEnv, a)
runVarM te = (\(result, newVars) -> (newTe newVars, result)) . runWriter
  where
    newTe vs = te & variable %~ (Map.union (Map.fromList vs))

transformReturns :: [Name] -> (TypeEnv, Exp) -> (TypeEnv, Exp)
transformReturns toUnbox (te, exp) = runVarM te $ apoM builder (Nothing, exp) where
  builder :: (Maybe (Tag, Type), Exp) -> VarM (ExpF (Either Exp (Maybe (Tag, Type), Exp)))
  builder (mTagType, exp0) = case exp0 of
    Def name params body
      | name `elem` toUnbox -> pure $ DefF name params (Right (returnsAUniqueTag te name, body))
      | otherwise           -> pure $ DefF name params (Left body)

    -- Always skip the lhs of a bind.
    EBind lhs pat rhs -> pure $ EBindF (Left lhs) pat (Right (mTagType, rhs))

    -- Remove the tag from the value
    SReturn (ConstTagNode tag [val]) -> pure $ SReturnF val

    -- Rewrite a node variable
    -- TODO: Unique variable name
    SReturn (Var v)
      -- fromJust works, as when we enter the processing of body of the
      -- expression only happens with the provided tag.
      -> do let Just (tag, typ) = mTagType
                v' = v ++ "'"
            tell [(v', typ)]
            pure $ (SBlockF (Left (EBind
                (SReturn (Var v))
                (ConstTagNode tag [Var v'])
                (SReturn (Var v')))))

    rest -> pure (Right . (,) mTagType <$> project rest)


transformCalls :: [Name] -> (TypeEnv, Exp) -> (TypeEnv, Exp)
transformCalls toUnbox (typeEnv, exp) = runVarM typeEnv $ anaM builderM exp where
  builderM :: Exp -> VarM (ExpF Exp)
  builderM = \case
    Def name params body
      | name `elem` toUnbox -> pure $ DefF (name ++ "'") params body
      | otherwise           -> pure $ DefF name          params body

    -- TODO: Unique name
    EBind lhs@(SApp name params) ctag@(ConstTagNode (Tag C tag) [Var x]) rhs
      | name `elem` toUnbox -> do
          let x' = x ++ "'"
              name' = name ++ "'"
              Just (_, fstType) = returnsAUniqueTag typeEnv name
          tell [(x', fstType)]
          pure $ EBindF
            (SBlock (EBind
              (SApp name' params)
              (Var x')
              (SReturn (ConstTagNode (Tag C tag) [Var x']))
              ))
            ctag
            rhs

    -- TODO: Unique name
    EBind lhs@(SApp name params) (Var x) rhs
      | name `elem` toUnbox -> do
          let x' = x ++ "'"
              name' = name ++ "'"
              Just (tag, fstType) = returnsAUniqueTag typeEnv name
          tell [(x', fstType)]
          pure $ EBindF
            (SBlock (EBind
              (SApp name' params)
              (Var x')
              (SReturn (ConstTagNode tag $ [Var x']))
              ))
            (Var x)
            rhs


    -- Tailcalls do not need a transform
    SApp name params
      | name `elem` toUnbox
      -> pure $ SAppF (name ++ "'") params

    rest -> pure $ project rest
