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

import Grin.Grin
import Grin.Pretty

import AbstractInterpretation.CByUtil
import AbstractInterpretation.CByResult
import AbstractInterpretation.LVAResult

import Transformations.Util

{-
 TODO: replace modify with modify'
       is it more optimal?

 NOTE: There are some `unsafe` function defined and used in this module.
       These functions are regular Map.lookup functions, but they will
       fail on Nothing. These are used only in specific cases where
       thanks to certain invariants, the keys will always be present
       in the given input.

       These calls could be replaced with `fromMaybe (error msg)`s.
       Where `msg` would contain a specific error message for each call site.
       (too C++esque)

       We could put the whole computation into an Exception monad.
-}


deadDataElimination :: LVAResult -> CByResult -> Exp -> Exp
deadDataElimination lvaResult cbyResult =
  ddeFromConsumers cbyResult . ddeFromProducers lvaResult cbyResult

markToRemove :: a -> Bool -> Maybe a
markToRemove x True  = Just x
markToRemove _ False = Nothing

unsafeLookupNodeLiveness :: Name -> Tag -> LVAResult -> Vector Bool
unsafeLookupNodeLiveness v t lvaResult
  | NodeSet taggedLiveness <- unsafeLookup v . _register $ lvaResult
  = _node . unsafeLookup t $ taggedLiveness

type GlobalLiveness = Map Name (Map Tag (Vector Bool))

calcGlobalLiveness :: LVAResult -> CByResult -> ProducerGraph' -> GlobalLiveness
calcGlobalLiveness lvaResult cbyResult prodGraph = mapWithDoubleKey' mergeLiveness prodGraph where

    -- map using only the keys
    mapWithDoubleKey' f = mapWithDoubleKey (\k1 k2 v -> f k1 k2)

    -- For producer p and tag t, it merges the liveness information of all fields
    -- with the other producers sharing a consumer with p for tag t.
    -- NOTE: What if a ctor is applied to different number of arguments?
    -- This can only happen at pattern matches, not at the time of construction.
    -- So we do not have to worry about the liveness of those "extra" parameters.
    -- They will always be at the last positions.
    mergeLiveness :: Name -> Tag -> Vector Bool
    mergeLiveness prod tag = foldl1 (Vec.zipWith (||))
                           . map (\v -> unsafeLookupNodeLiveness v tag lvaResult)
                           . Set.toList
                           $ connectedProds
      where
        connectedProds :: Set Name
        connectedProds = fromMaybe mempty
                       . Map.lookup tag
                       . fromMaybe mempty
                       . Map.lookup prod
                       $ prodGraph

ddeFromConsumers :: CByResult -> (Exp, GlobalLiveness) -> Exp
ddeFromConsumers cbyResult (e, gblLiveness) = cata alg e where

  alg :: ExpF Exp -> Exp
  alg = \case
    ECaseF (Var v) alts -> ECase (Var v) . flip map alts $ \case
      Alt (NodePat t args) e -> let args' = deleteDeadFields v t args
                                in Alt (NodePat t args') e
      e -> error $ "Unsupported case alternative in dead data elimination: " ++ show (pretty e)
    EBindF lhs pat rhs
      | SReturn (Var v)  <- lhs
      , ConstTagNode t args <- pat
      , args' <- deleteDeadFields v t args
      -> EBind lhs (ConstTagNode t args') rhs
    e -> embed e

  deleteDeadFields :: Name -> Tag -> [a] -> [a]
  deleteDeadFields v t args = catMaybes $ zipWith markToRemove args gblLiveness
    where gblLiveness = Vec.toList $ lookupGlobalLiveness v t

  lookupGlobalLiveness :: Name -> Tag -> Vector Bool
  lookupGlobalLiveness v t = unsafeLookup t . unsafeLookup p $ gblLiveness where
    (p:_) = Set.toList
          . unsafeLookup t
          . _producerSet
          . unsafeLookup v
          . _producerMap
          . _producers
          $ cbyResult

-- For each producer, it dummifies all locally unused fields.
-- If the field is dead for all other producers in the same group,
-- then it deletes the field.
-- Whenever it deletes a field, it makes a new entry into a table.
-- This table will be used to transform the consumers.
ddeFromProducers :: LVAResult -> CByResult -> Exp -> (Exp, GlobalLiveness)
ddeFromProducers lvaResult cbyResult e = (cata alg e, globalLiveness) where

  alg :: ExpF Exp -> Exp
  alg = \case
    EBindF (SReturn (ConstTagNode t args)) (Var v) rhs
      | nodeLiveness <- unsafeLookupNodeLiveness v t lvaResult
      , globalNodeLiveness <- unsafeLookupWithDoubleKey v t globalLiveness
      -- dummifying all locally unused fields
      , args' <- zipWith dummify args (Vec.toList nodeLiveness)
      -- deleteing all globally unused fields
      , args'' <- catMaybes $ zipWith markToRemove args' (Vec.toList globalNodeLiveness)
      -> EBind (SReturn (ConstTagNode t args'')) (Var v) rhs
    e -> embed e

  prodGraph :: ProducerGraph'
  prodGraph = fromProducerGraph . groupActiveProducers lvaResult cbyResult $ e

  globalLiveness :: GlobalLiveness
  globalLiveness = calcGlobalLiveness lvaResult cbyResult prodGraph

  switch :: a -> a -> Bool -> a
  switch _ x True  = x
  switch d _ False = d

  dummify :: Val -> Bool -> Val
  dummify = switch (Var "undefined")
