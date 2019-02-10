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
import Lens.Micro.Platform

import Control.Monad.Extra
import Control.Monad.State
import Control.Monad.Trans.Except

import Grin.Grin
import Grin.Pretty
import Grin.TypeEnv
import Grin.EffectMap
import Transformations.Util
import AbstractInterpretation.LiveVariable.Result as LVA


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
deadVariableElimination :: LVAResult -> EffectMap -> TypeEnv -> Exp -> Either String Exp
deadVariableElimination lvaResult effMap tyEnv exp =
  let protected = analyzeCases effMap tyEnv exp in
  runTrf . (deleteDeadBindings protected lvaResult effMap tyEnv >=> replaceDeletedVars tyEnv) $ exp

{- NOTE: Fetches do not have to be handled separately,
   since producer name introduction guarantees
   that all bindings with a fetch LHS will have a Var PAT
   (handled by the last case in alg).

   Also, protected case expressions bound to a variable
   cannot be eliminated because it might result in
   change of semantics. See `analyzeCases` for more info.
-}
deleteDeadBindings :: Set Name -> LVAResult -> EffectMap -> TypeEnv -> Exp -> Trf Exp
deleteDeadBindings protected lvaResult effMap tyEnv = cataM alg where
  alg :: ExpF Exp -> Trf Exp
  alg = \case
    e@(EBindF SStore{} (Var p) rhs)
      | Just locs <- tyEnv ^? variable . at p . _Just . _T_SimpleType . _T_Location -> do
        unless (isSingleton locs) (throwE $ multipleLocs p locs)
        pointerDead <- isVarDeadM p
        rmWhen pointerDead e rhs (Set.singleton p) (Set.fromList locs)
    e@(EBindF (SApp f _) lpat rhs) -> do
      let names = foldNamesVal Set.singleton lpat
          hasNoSideEffect = not $ hasTrueSideEffect f effMap
      funDead <- isFunDeadM f
      rmWhen (funDead && hasNoSideEffect) e rhs names mempty
    e@(EBindF (SUpdate p v) Unit rhs) -> do
      varDead <- isVarDeadM p
      rmWhen varDead e rhs mempty mempty
    e@(EBindF _ (Var v) rhs)
      | v `Set.member` protected -> pure . embed $ e
      | otherwise -> do
        varDead <- isVarDeadM v
        rmWhen varDead e rhs (Set.singleton v) mempty
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

  isVarDeadM :: Name -> Trf Bool
  isVarDeadM v = fmap (not . isLive)
                . lookupExcept (varLvNotFound v) v
                . _register
                $ lvaResult

  isFunDeadM :: Name -> Trf Bool
  isFunDeadM f = fmap isFunDead
               . lookupExcept (funLvNotFound f) f
               . LVA._function
               $ lvaResult

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

{- TODO: Pattern match failure checks should be moved to LVA.
         New instructions to the abstarct machine IR are required.
         (new Condition: NodeTypeDoesNotExist Tag)
-}
{- Collects the name of case expressions and scrutinees(*) that cannot be eliminated.
   The name of a case expression is the variable it is bound to.
   A case expression cannot be eliminated if the scrutinee
   has at least one tag/literal not covered by any of the alternatives
   (i.e. the pattern match can fail), or any of the alternatives
   contain a side-effecting function or external.

   (*) Sometimes, even if the scrutinee is dead,
   its original binding cannot be removed.
   This is becase there can be an execution path which can lead to
   a pattern match failure or a side-effecting computation.

   Also, it is assumed that all case alternatives are live (i.e.: after SCO).
-}
analyzeCases :: EffectMap -> TypeEnv -> Exp -> Set Name
analyzeCases effMap tyEnv = flip execState mempty . paraM alg where

  -- We store the names of the case expressions and scrutinees in the state.
  -- The result is a boolean represeneting the presence of side effects
  -- or filable patterns inside the expression (and its subexpressions).
  alg :: ExpF (Exp, Bool) -> State (Set Name) Bool
  alg = \case
    SAppF f _ -> pure $ hasTrueSideEffect f effMap
    AltF _ (_,s) -> pure s
    ECaseF scrut alts -> do
      let altPats = Set.fromList $ mapMaybe (^? _AltCPat) (fmap fst alts)
          altTags = Set.mapMaybe (^? _CPatNodeTag) altPats
          alts'   = and . fmap snd $ alts

          addScrutinee = case scrut of
            Var x -> modify (Set.insert x)
            _     -> pure ()

      case mTypeOfValTE tyEnv scrut of
        -- There is a case nested case expression
        -- in the alternatives which cannot be removed.
        _ | alts' -> addScrutinee >> pure True
        -- The pattern match can fail on a node tag.
        Just (T_NodeSet ns)
          | DefaultPat `Set.notMember` altPats
          , scrutTags <- Map.keysSet ns
          , any (`Set.notMember` altTags) scrutTags
          -> addScrutinee >> pure True
        -- The pattern match can fail on a literal.
        Just T_SimpleType{}
          | hasDef   <- DefaultPat    `Set.member` altPats
          , hasTrue  <- BoolPat True  `Set.member` altPats
          , hasFalse <- BoolPat False `Set.member` altPats
          , not (hasTrue && hasFalse || hasDef)
          -> addScrutinee >> pure True
        Nothing -> error $ "DVE: case analysis: scrutinee's type cannot be calculated (" ++ show (PP scrut) ++ ")"
        _ -> pure alts'

    EBindF (ECase{}, lhs) (Var v) (_,rhs) -> do
      when lhs $ modify (Set.insert v)
      pure $ lhs || rhs

    EBindF (_,lhs) _ (_,rhs) -> pure $ lhs || rhs
    SBlockF (_,s) -> pure s

    -- The other cases are not important.
    _ -> pure False

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
