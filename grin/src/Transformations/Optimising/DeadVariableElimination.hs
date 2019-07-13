{-# LANGUAGE LambdaCase, RecordWildCards, TemplateHaskell #-}
module Transformations.Optimising.DeadVariableElimination where

import Data.Set (Set)
import Data.Map (Map)
import Data.Vector (Vector)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec

import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Set.Extra as Set

import qualified Data.Foldable
import Data.Functor.Foldable as Foldable

import Lens.Micro
import Lens.Micro.Extra
import Lens.Micro.Platform

import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Trans.Except

import Grin.Grin
import Grin.Pretty
import Grin.TypeEnv
import Transformations.Util
import AbstractInterpretation.LiveVariable.Result as LVA

-- TODO: clean up `analyzeCases` and the other remnants of the EffectMap

data DeletedEntities = DeletedEntities
  { _deVariables :: Set Name
  , _deLocations :: Set Int
  }
  deriving (Eq, Ord, Show)

instance Monoid DeletedEntities where
  mempty = DeletedEntities mempty mempty
instance Semigroup DeletedEntities where
  (<>) (DeletedEntities vars1 locs1) (DeletedEntities vars2 locs2) =
    DeletedEntities (vars1 <> vars2) (locs1 <> locs2)

concat <$> mapM makeLenses [''DeletedEntities]

type Trf = ExceptT String (State DeletedEntities)

runTrf :: Trf a -> Either String a
runTrf = flip evalState mempty . runExceptT

-- P and F nodes are handled by Dead Data Elimination
deadVariableElimination :: LVAResult -> TypeEnv -> Exp -> Either String Exp
deadVariableElimination lvaResult tyEnv exp =
  runTrf . (deleteDeadBindings lvaResult tyEnv >=> replaceDeletedVars tyEnv) $ exp

{- NOTE: Fetches do not have to be handled separately,
   since producer name introduction guarantees
   that all bindings with a fetch LHS will have a Var PAT
   (handled by the last case in alg).

   Side effects are only incorporated into the liveness analysis
   if `EffectTracking` was run before LVA.
   Otherwise all computations are considered pure.
-}
deleteDeadBindings :: LVAResult -> TypeEnv -> Exp -> Trf Exp
deleteDeadBindings lvaResult tyEnv p@(Program exts _) = cataM alg p where
  alg :: ExpF Exp -> Trf Exp
  alg = \case
    e@(EBindF SStore{} (Var p) rhs)
      | Just locs <- tyEnv ^? variable . at p . _Just . _T_SimpleType . _T_Location -> do
        unless (isSingleton locs) (throwE $ multipleLocs p locs)
        pointerDead <- isVarDeadM p
        rmWhen pointerDead e rhs (Set.singleton p) (Set.fromList locs)
    e@(EBindF (SApp f _) (Var v) rhs) | f `notElem` (map eName exts) -> do
      {- NOTE: A live arg could mean there is an update inside the function.
         Deleting this update usually has no effect on the code, but it is
         not proven, that it will always keep the semantics.
      -}
      noLiveArgs <- noLiveArgsM f
      varDead    <- isVarDeadM v
      bindingHasNoEffect <- varHasNoSideEffectsM v
      rmWhen (noLiveArgs && varDead && bindingHasNoEffect) e rhs (Set.singleton v) mempty
    e@(EBindF (SUpdate p v) Unit rhs) -> do
      varDead <- isVarDeadM p
      rmWhen varDead e rhs mempty mempty
    e@(EBindF _ (Var v) rhs) -> do
      varDead <- isVarDeadM v
      bindingHasNoEffect <- varHasNoSideEffectsM v
      rmWhen (varDead && bindingHasNoEffect) e rhs (Set.singleton v) mempty
    e -> pure . embed $ e

  rmWhenAllDead :: ExpF Exp -> Exp -> Val -> Trf Exp
  rmWhenAllDead orig modified val = do
    let names  = foldNamesVal Set.singleton val
    allVarsDead <- allM isVarDeadM . Set.toList $ names
    rmWhen allVarsDead orig modified names mempty

  rmWhen :: Bool -> ExpF Exp -> Exp -> Set Name -> Set Int -> Trf Exp
  rmWhen needsRemoval orig modified variables locations
    | needsRemoval = do deVariables %= (mappend variables)
                        deLocations %= (mappend locations)
                        pure modified
    | otherwise = pure . embed $ orig

  varHasNoSideEffectsM :: Name -> Trf Bool
  varHasNoSideEffectsM v = fmap (not . _hasEffect)
                         . lookupExcept (varEffNotFound v) v
                         . LVA._registerEff
                         $ lvaResult

  isVarDeadM :: Name -> Trf Bool
  isVarDeadM v = fmap (not . isLive)
               . lookupExcept (varLvNotFound v) v
               . LVA._registerLv
               $ lvaResult

  noLiveArgsM :: Name -> Trf Bool
  noLiveArgsM f = fmap (not . hasLiveArgs)
                . lookupExcept (funLvNotFound f) f
                . LVA._functionLv
                $ lvaResult

  varEffNotFound v = "DVE: Variable " ++ show (PP v) ++ " was not found in effect map"
  varLvNotFound v = "DVE: Variable " ++ show (PP v) ++ " was not found in liveness map"
  funLvNotFound f = "DVE: Function " ++ show (PP f) ++ " was not found in liveness map"

  isSingleton :: [a] -> Bool
  isSingleton [_] = True
  isSingleton _   = False

  multipleLocs :: Name ->  [Int] -> String
  multipleLocs p locs = "DVE: A pointer bound out from a store instruction "
                     ++ "should always point to a single locationn, "
                     ++ "but " ++ show (PP p) ++ " points to multiple locations: "
                     ++ show (PP locs)

-- This will not replace the occurences of a deleted pointer
-- in fetches and in updates. But it does not matter,
-- since all of these fetches/updates are also dead, so they will be removed as well.
replaceDeletedVars :: TypeEnv -> Exp -> Trf Exp
replaceDeletedVars tyEnv e = do
  deletedVars <- use deVariables
  let f = replaceVarWithUndefined deletedVars tyEnv
  cataM (mapValsExpM (mapValValM f) . embed) e

replaceVarWithUndefined :: Set Name -> TypeEnv -> Val -> Trf Val
replaceVarWithUndefined deletedVars TypeEnv{..} (Var v)
  | v `elem` deletedVars = do
    t <- lookupExcept (notFoundInTyEnv v) v _variable
    pure $ Undefined (simplifyType t)
  where notFoundInTyEnv v = "DVE: Variable " ++ show (PP v) ++ " was not found in type env"
replaceVarWithUndefined _ _ v = pure v


trfLoc :: Int -> Trf (Maybe Int)
trfLoc loc = do
  deletedLocs <- use deLocations
  let numRemovedBefore = length . Set.filter (< loc) $ deletedLocs
  if loc `elem` deletedLocs
    then pure Nothing
    else pure $ Just (loc - numRemovedBefore)

trfSimpleType :: SimpleType -> Trf SimpleType
trfSimpleType (T_Location locs) = do
  locs' <- mapM trfLoc locs
  let locs'' = catMaybes locs'
  if null locs'' then pure T_Dead
                 else pure . T_Location $ locs''

trfNodeSet :: NodeSet -> Trf NodeSet
trfNodeSet = mapM (mapM trfSimpleType)

trfType :: Type -> Trf Type
trfType (T_SimpleType st) = fmap T_SimpleType (trfSimpleType st)
trfType (T_NodeSet ns) = fmap T_NodeSet (trfNodeSet ns)
trfType t = throwE $ "DVE: Unsupported type in type env transformation: " ++ show (PP t)

trfFunT :: (Type, Vector Type) -> Trf (Type, Vector Type)
trfFunT (retT, argTs) = do
  retT' <- trfType retT
  argTs' <- mapM trfType argTs
  pure (retT', argTs')

trfTypeEnv :: TypeEnv -> Trf TypeEnv
trfTypeEnv TypeEnv{..} = do
  deletedLocs <- use deLocations
  deletedVars <- use deVariables

  let locations = Vec.ifilter (\i _ -> not (i `elem` deletedLocs)) _location
  locations' <- mapM trfNodeSet locations

  let variables = Map.withoutKeys _variable deletedVars
  variables' <- mapM trfType variables

  functions <- mapM trfFunT _function

  pure (TypeEnv locations' variables' functions)
