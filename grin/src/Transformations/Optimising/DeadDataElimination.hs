{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.DeadDataElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Maybe
import Data.Functor.Foldable as Foldable

import Control.Monad
import Control.Monad.Trans.Except

import Grin.Grin
import Grin.Pretty

import AbstractInterpretation.CByUtil
import AbstractInterpretation.CByResult
import AbstractInterpretation.LVAResult

import Transformations.Util

{-
 TODO: replace modify with modify'
       is it more optimal?
-}

deadDataElimination :: LVAResult -> CByResult -> Exp -> Trf Exp
deadDataElimination lvaResult cbyResult =
  ddeFromProducers lvaResult cbyResult >=> ddeFromConsumers cbyResult

markToRemove :: a -> Bool -> Maybe a
markToRemove x True  = Just x
markToRemove _ False = Nothing

lookupNodeLivenessM :: Name -> Tag -> LVAResult -> Except String (Vector Bool)
lookupNodeLivenessM v t lvaResult = do
  NodeSet taggedLiveness <- lookupExcept (noLiveness v) v . _register $ lvaResult
  _node <$> lookupExcept (noLivenessTag v t) t taggedLiveness
  where noLiveness    v   = noLivenessMsg ++ show (PP v)
        noLivenessTag v t = noLivenessMsg ++ show (PP v) ++ " with tag " ++ show (PP t)
        noLivenessMsg     = "No liveness information present for variable"

type GlobalLiveness = Map Name (Map Tag (Vector Bool))

calcGlobalLiveness :: LVAResult -> CByResult -> ProducerGraph' -> Except String GlobalLiveness
calcGlobalLiveness lvaResult cbyResult prodGraph =
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
    mergeLivenessExcept :: Name -> Tag -> Except String (Vector Bool)
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

ddeFromConsumers :: CByResult -> (Exp, GlobalLiveness) -> Trf Exp
ddeFromConsumers cbyResult (e, gblLiveness) = cataM alg e where

  alg :: ExpF Exp -> Trf Exp
  alg = \case
    ECaseF (Var v) alts -> do
      alts' <- forM alts $ \case
        Alt (NodePat t args) e -> do
          args' <- deleteDeadFieldsM v t args
          pure $ Alt (NodePat t args') e
        e -> throwE $ "Unsupported case alternative in dead data elimination: "
                      ++ show (pretty e)
      pure $ ECase (Var v) alts'

    EBindF lhs@(SReturn (Var v)) (ConstTagNode t args) rhs -> do
      args' <- deleteDeadFieldsM v t args
      pure $ EBind lhs (ConstTagNode t args') rhs
    e -> pure . embed $ e

  deleteDeadFieldsM :: Name -> Tag -> [a] -> Except String [a]
  deleteDeadFieldsM v t args = do
    gblLivenessVT <- Vec.toList <$> lookupGlobalLivenessM v t
    pure $ catMaybes $ zipWith markToRemove args gblLivenessVT

  lookupGlobalLivenessM :: Name -> Tag -> Except String (Vector Bool)
  lookupGlobalLivenessM v t = do
    let pMap = _producerMap . _producers $ cbyResult
    pSet <- _producerSet <$> lookupExcept (notFoundInPMap v) v pMap
    (p:_) <- Set.toList  <$> lookupExcept (notFoundInPMap t) t pSet
    lookupWithDoubleKeyExcept (notFoundLiveness p t) p t gblLiveness

-- For each producer, it dummifies all locally unused fields.
-- If the field is dead for all other producers in the same group,
-- then it deletes the field.
-- Whenever it deletes a field, it makes a new entry into a table.
-- This table will be used to transform the consumers.
ddeFromProducers :: LVAResult -> CByResult -> Exp -> Trf (Exp, GlobalLiveness)
ddeFromProducers lvaResult cbyResult e = (,) <$> cataM alg e <*> globalLivenessM where

  -- dummifying all locally unused fields
  -- deleteing all globally unused fields
  alg :: ExpF Exp -> Trf Exp
  alg = \case
    EBindF (SReturn (ConstTagNode t args)) (Var v) rhs -> do
      globalLiveness     <- globalLivenessM
      nodeLiveness       <- lookupNodeLivenessM v t lvaResult
      globalNodeLiveness <- lookupWithDoubleKeyExcept (notFoundLiveness v t) v t globalLiveness
      let args'  = zipWith dummify args (Vec.toList nodeLiveness)
          args'' = catMaybes $ zipWith markToRemove args' (Vec.toList globalNodeLiveness)
      pure $ EBind (SReturn (ConstTagNode t args'')) (Var v) rhs
    e -> pure . embed $ e

  prodGraph :: ProducerGraph'
  prodGraph = fromProducerGraph . groupActiveProducers lvaResult cbyResult $ e

  globalLivenessM :: Except String GlobalLiveness
  globalLivenessM = calcGlobalLiveness lvaResult cbyResult prodGraph

  switch :: a -> a -> Bool -> a
  switch _ x True  = x
  switch d _ False = d

  dummify :: Val -> Bool -> Val
  dummify = switch (Lit LUndefined)



notFoundInPMap :: Pretty a => a -> String
notFoundInPMap v = notFoundIn "Variable" (PP v) "producer map"

notFoundInPSet :: Pretty a => a -> String
notFoundInPSet t = notFoundIn "Tag" (PP t) "producer set"

notFoundLiveness :: (Pretty a, Pretty b) => a -> b -> String
notFoundLiveness p t = "Producer "  ++ show (PP p) ++
                       " with tag " ++ show (PP t) ++
                       " not found in global liveness map"
