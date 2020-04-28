{-# LANGUAGE LambdaCase, RecordWildCards, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.DeadDataElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.List
import Data.Maybe
import Data.Functor.Foldable as Foldable

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except

import Lens.Micro

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.TypeEnv

import AbstractInterpretation.ExtendedSyntax.CreatedBy.Util
import AbstractInterpretation.ExtendedSyntax.CreatedBy.Result
import AbstractInterpretation.ExtendedSyntax.LiveVariable.Result

import Transformations.ExtendedSyntax.Util
import Transformations.ExtendedSyntax.Names


-- TODO: make NameM local (it's only used once in ddeFromProducers)
-- (t,lv) -> t'
-- we deleted the dead fields from a node with tag t with liveness lv
-- then we introduced the new tag t' for this deleted node
type TagMapping = Map (Tag, Vector Bool) Tag
type Trf = ExceptT String (StateT TagMapping NameM)

execTrf :: Exp -> Trf a -> Either String (a, ExpChanges)
execTrf e = moveChangedResult . evalNameM e . flip evalStateT mempty . runExceptT
  where
    moveChangedResult (x, b) = either Left (\r -> Right (r, b)) x

getTag :: Tag -> Vector Bool -> Trf Tag
getTag t lv
  | and lv = pure t
getTag t@(Tag ty n) lv = do
  mt' <- gets $ Map.lookup (t,lv)
  case mt' of
    Just t' -> return t'
    Nothing -> do
      n' <- lift $ lift $ deriveNewName n
      let t' = Tag ty n'
      modify $ Map.insert (t,lv) t'
      return t'


deadDataElimination :: LVAResult -> CByResult -> TypeEnv ->  Exp -> Either String (Exp, ExpChanges)
deadDataElimination lvaResult cbyResult tyEnv e = execTrf e $
  ddeFromProducers lvaResult cbyResult tyEnv e >>= ddeFromConsumers cbyResult tyEnv


lookupNodeLivenessM :: Name -> Tag -> LVAResult -> Trf (Vector Bool)
lookupNodeLivenessM v t lvaResult = do
  lvInfo <- lookupExcept (noLiveness v) v . _registerLv $ lvaResult
  case lvInfo of
    NodeSet taggedLiveness ->
      _fields <$> lookupExcept (noLivenessTag v t) t taggedLiveness
    _ -> throwE $ notANode v
  where noLiveness    v   = noLivenessMsg ++ show (PP v)
        noLivenessTag v t = noLivenessMsg ++ show (PP v) ++ " with tag " ++ show (PP t)
        noLivenessMsg     = "No liveness information present for variable "
        notANode      v   = "Variable " ++ show (PP v) ++ " has non-node liveness information. " ++
                            "Probable cause: Either lookupNodeLivenessM was called on a non-node variable, " ++
                            "or the liveness information was never calculated for the variable " ++
                            "(e.g.: it was inside a dead case alternative)."

-- Global liveness is the accumulated liveness information about the producers
-- It represents the collective liveness of a producer group.
type GlobalLiveness = Map Name (Map Tag (Vector Bool))

{-
 This should always get an active producer graph
 Even if it does not, lookupNodeLivenessM will not be called on dead(1) variables,
 because the connectProds set will be empty for such variables.

 (1) - Here "dead variable" means a variable that was not analyzed.

 NOTE: We will ignore undefined producers, since they should always be dead.
-}
calcGlobalLiveness :: LVAResult ->
                      ProducerGraph' ->
                      Trf GlobalLiveness
calcGlobalLiveness lvaResult (withoutUndefined -> prodGraph) =
  mapWithDoubleKeyM' mergeLivenessExcept prodGraph where

    -- map using only the keys
    mapWithDoubleKeyM' f = mapWithDoubleKeyM (\k1 k2 v -> f k1 k2)

    -- For producer p and tag t, it merges the liveness information of all fields
    -- with the other producers sharing a consumer with p for tag t.
    -- Every producer must have at least one connection for its own tag
    -- with itself (reflexive closure).
    -- NOTE: What if a ctor is applied to different number of arguments?
    -- This can only happen at pattern matches, not at the time of construction.
    -- So we do not have to worry about the liveness of those "extra" parameters.
    -- They will always be at the last positions.
    mergeLivenessExcept :: Name -> Tag -> Trf (Vector Bool)
    mergeLivenessExcept prod tag = do
      let ps = Set.toList connectedProds
      when (null ps) (throwE $ noConnections prod tag)
      ls <- mapM (\v -> lookupNodeLivenessM v tag lvaResult) ps
      pure $ foldl1 (Vec.zipWith (||)) ls

      where
        connectedProds :: Set Name
        connectedProds = fromMaybe mempty
                       . Map.lookup tag
                       . fromMaybe mempty
                       . Map.lookup prod
                       $ prodGraph

        noConnections :: (Pretty a, Pretty b) => a -> b -> String
        noConnections p t = "Producer " ++ show (PP p) ++
                            " for tag " ++ show (PP t) ++
                            " is not connected with any other producers"

ddeFromConsumers :: CByResult -> TypeEnv -> (Exp, GlobalLiveness) -> Trf Exp
ddeFromConsumers cbyResult tyEnv (e, gblLiveness) = cataM alg e where

  alg :: ExpF Exp -> Trf Exp
  alg = \case
    ECaseF v alts -> do
      alts' <- forM alts $ \case
        Alt (NodePat t args) altName e -> do
          (args',lv) <- deleteDeadFieldsM v t args
          let deletedArgs = args \\ args'
          e' <- bindToUndefineds tyEnv e deletedArgs
          t' <- getTag t lv
          pure $ Alt (NodePat t' args') altName e'
        e -> pure e
      pure $ ECase v alts'

    EBindF lhs (AsPat t args v) rhs -> do
      (args',lv) <- deleteDeadFieldsM v t args
      let deletedArgs = (args \\ args')
      rhs' <- bindToUndefineds tyEnv rhs deletedArgs
      t' <- getTag t lv
      pure $ EBind lhs (AsPat t' args' v) rhs'

    e -> pure . embed $ e

  deleteDeadFieldsM :: Name -> Tag -> [a] -> Trf ([a], Vector Bool)
  deleteDeadFieldsM v t args = do
    gblLivenessVT <- lookupGlobalLivenessM v t
    let args'    = zipFilter args gblLivenessVT
        liveness = Vec.fromList $ take (length args) gblLivenessVT
    pure (args', liveness)

  -- Returns "all dead" if it cannot find the tag
  -- This way it handles impossible case alternatives
  -- NOTE: could also be solved by prior sparse case optimisation
  lookupGlobalLivenessM :: Name -> Tag -> Trf [Bool]
  lookupGlobalLivenessM v t = do
    let pMap = _producerMap . _producers $ cbyResult
    pSet <- _producerSet <$> lookupExcept (notFoundInPMap v) v pMap
    flip catchE (const $ pure $ repeat False) $ do
      ~(p:_) <- Set.toList  <$> lookupExcept (notFoundInPSet t) t pSet
      liveness <- lookupWithDoubleKeyExcept (notFoundLiveness p t) p t gblLiveness
      pure $ Vec.toList liveness

-- For each producer, it dummifies all locally unused fields.
-- If the field is dead for all other producers in the same group,
-- then it deletes that field.
-- Whenever it deletes a field, it makes a new entry into a table.
-- This table will be used to transform the consumers.
ddeFromProducers :: LVAResult -> CByResult -> TypeEnv -> Exp -> Trf (Exp, GlobalLiveness)
ddeFromProducers lvaResult cbyResult tyEnv e = (,) <$> cataM alg e <*> globalLivenessM where

  -- deleteing all globally unused fields
  -- if the variable was not analyzed (has type T_Dead), it will be skipped
  alg :: ExpF Exp -> Trf Exp
  alg = \case
    -- TODO: investigate as-pat case
    e@(EBindF (SReturn (ConstTagNode t args)) bPat@(_bPatVar -> v) rhs)
      | Just T_Dead <- tyEnv ^? variable . at v . _Just . _T_SimpleType
      -> pure . embed $ e
    -- TODO: investigate as-pat case
    EBindF (SReturn (ConstTagNode t args)) bPat@(_bPatVar -> v) rhs -> do
      globalLiveness     <- globalLivenessM
      nodeLiveness       <- lookupNodeLivenessM v t lvaResult
      globalNodeLiveness <- lookupWithDoubleKeyExcept (notFoundLiveness v t) v t globalLiveness
      let onlyDummifiable   = \locallyLive globallyLive -> not locallyLive && globallyLive
          onlyDummifiables  = Vec.zipWith onlyDummifiable nodeLiveness globalNodeLiveness
          toBeDummified     = zipFilter (zip [0..] args) (Vec.toList onlyDummifiables)
          toBeDummifiedIxs  = fst <$> toBeDummified
          toBeDummifiedArgs = snd <$> toBeDummified
      typedDummifiedArgs <- typedFreshNames toBeDummifiedArgs
      newTag             <- getTag t globalNodeLiveness -- could be the same as the old one
      let argsVec          = Vec.fromList args
          indexedNewArgs   = Vec.fromList . zip toBeDummifiedIxs . map fst $ typedDummifiedArgs
          newArgs          = Vec.toList $ Vec.update argsVec indexedNewArgs
          liveNewArgs      = zipFilter newArgs (Vec.toList globalNodeLiveness)
          returnNewNode    = SReturn (ConstTagNode newTag liveNewArgs)
      pure $ typedDummifiedArgs `areBoundThen` EBind returnNewNode bPat rhs
    e -> pure . embed $ e

  -- extracts the active producer grouping from the CByResult
  -- if not present, it calculates it (so it will always work with only the active producers)
  prodGraph :: ProducerGraph'
  prodGraph = case _groupedProducers cbyResult of
    All _ -> fromProducerGraph
           . groupActiveProducers lvaResult
           . _producers
           $ cbyResult
    Active activeProdGraph -> fromProducerGraph activeProdGraph

  globalLivenessM :: Trf GlobalLiveness
  globalLivenessM = calcGlobalLiveness lvaResult prodGraph

  -- NOTE: uses tyEnv from outer scope
  -- | Given a list of names, it looks up their types
  -- and pairs them with fresh new names.
  typedFreshNames :: [Name] -> Trf [(Name, Type)]
  typedFreshNames ns = forM ns $ \v -> do
    v' <- lift $ lift $ deriveNewName v
    ty <- lookupExcept (notFoundInTyEnv v) v (_variable tyEnv)
    let ty' = simplifyType ty
    pure (v', ty')

  -- TODO: comment
  -- | Constructs a binding sequence which first
  -- binds the typed undefineds to the given names,
  -- then returns a node with those arguments.
  areBoundThen :: [(Name, Type)] -> Exp -> Exp
  areBoundThen typedDummifiedArgs cont =
    foldl rebindToUndefined cont typedDummifiedArgs where

    -- returnNewNode :: Exp
    -- returnNewNode = SReturn (ConstTagNode tag allArgs)

    rebindToUndefined :: Exp -> (Name, Type) -> Exp
    rebindToUndefined rhs (v, ty) =
      EBind (SReturn (Undefined ty)) (VarPat v) rhs

notFoundInPMap :: Pretty a => a -> String
notFoundInPMap v = notFoundIn "Variable" (PP v) "producer map"

notFoundInPSet :: Pretty a => a -> String
notFoundInPSet t = notFoundIn "Tag" (PP t) "producer set"

notFoundLiveness :: (Pretty a, Pretty b) => a -> b -> String
notFoundLiveness p t = "Producer "  ++ show (PP p) ++
                       " with tag " ++ show (PP t) ++
                       " not found in global liveness map"

notFoundInTyEnv :: Pretty a => a -> String
notFoundInTyEnv v = notFoundIn "Variable" (PP v) "type environment"

notFoundInTySetFor :: (Pretty a, Pretty b) => a -> b -> String
notFoundInTySetFor t v = (notFoundIn "Tag" (PP t) "node type set") ++ " for variable " ++ show (PP v)
