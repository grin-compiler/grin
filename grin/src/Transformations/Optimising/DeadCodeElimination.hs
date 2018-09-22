{-# LANGUAGE LambdaCase #-}

module DeadCodeElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.Functor.Foldable as Foldable

import Control.Monad.State

import Grin.Grin

import AbstractInterpretation.CByResult
import Transformations.Util


-- TODO: replace modify with modify'
-- is it more optimal?

deadCodeElimination :: (CByResult, Exp) -> Exp
deadCodeElimination (cbyResult, e) = cata alg e where
  alg :: ExpF Exp -> Exp
  alg = \case
    EBindF lhs (Var v) (SReturn (ConstTagNode t args)) -> undefined
    e -> embed e

-- A graph representing the connections between producers.
-- p1 <-t-> p2 means: producers p1 and p2 share a consumer for tag t
type ProducerGraph = Map Name (Map Tag (Set Name))

-- Collects the consumers from the syntax tree.
-- A consumer can be a case expression scrutinee,
-- or the left-hand side of a bind, if it is of form: <tag> <fields> <- pure v.
-- Only the producers of these consumers will be interesting (the others are unused).
-- NOTE: VarNode?
collectConsumers :: Exp -> Set Name
collectConsumers = flip execState mempty . paraM alg where
  alg :: ExpF (Exp,()) -> State (Set Name) ()
  alg = \case
    ECaseF (Var v) _ -> modify $ Set.insert v
    EBindF lhs pat rhs
      | SReturn (Var v)  <- fst lhs
      , ConstTagNode _ _ <- pat
      -> modify $ Set.insert v
    x -> pure ()

-- Constructs the connection graph between producers.
-- If a consumer has multiple producers with the same tag,
-- then one producer will be selected, and the others will be connected to it.
-- Then the transitive closure is calculated.
groupProducers :: CByResult -> Exp -> ProducerGraph
groupProducers cbyResult e = transitiveClosure . flip execState mempty $ do
  let
    consumers :: Set Name
    consumers = collectConsumers e

    -- All the active producers found in the program grouped by tags.
    -- A producer is active if it has at least one consumer.
    taggedGroups :: [(Tag, Set Name)]
    taggedGroups = concatMap (Map.toList . _producerSet)
                 . Map.elems
                 . flip Map.restrictKeys consumers
                 . _producerMap
                 . _producers
                 $ cbyResult

  forM taggedGroups $ \(t,ps) -> do
    let (p:_)  = Set.toList ps
        entry  = Map.singleton t ps
        update = Map.unionWith Set.union
    modify $ Map.insertWith update p entry

transitiveClosure :: ProducerGraph -> ProducerGraph
transitiveClosure m
  | next <- tcStep m
  , next /= m
  = transitiveClosure next
  | otherwise = m
  where

  -- for each (p, (t, [p1 .. pn])),
  -- it inserts (p1, (t, [p])) .. (pn, (t, [p]))
  tcStep :: ProducerGraph -> ProducerGraph
  tcStep m = flip execState m $ do
    let pList = Map.toList
              . Map.map Map.toList
              . Map.map (Map.map Set.toList)
              $ m
    forM pList $ \(p, taggedGroups) ->
      forM taggedGroups $ \(t, ps) ->
        forM ps $ \p' -> do
          let entry  = Map.singleton t (Set.singleton p)
              update = Map.unionWith Set.union
          modify $ Map.insertWith update p' entry
