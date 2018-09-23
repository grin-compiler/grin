{-# LANGUAGE LambdaCase #-}
module AbstractInterpretation.CByUtil where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import Data.Maybe
import Data.Functor.Foldable as Foldable

import Control.Monad.State

import Grin.Grin

import AbstractInterpretation.LVAResult
import AbstractInterpretation.CByResult

import Transformations.Util

-- A graph representing the connections between producers.
-- p1 <-t-> p2 means: producers p1 and p2 share a consumer for tag t
-- In a ProducerMap, we map variables to producers,
-- in a ProducerGraph we map producers to other producers.
newtype ProducerGraph = ProducerGraph { _producerGraph :: ProducerMap }
-- An untyped representation of the ProducerGraph (easier to handle).
type ProducerGraph' = Map Name (Map Tag (Set Name))

toProducerGraph :: ProducerGraph' -> ProducerGraph
toProducerGraph = ProducerGraph . ProducerMap . Map.map ProducerSet

fromProducerGraph :: ProducerGraph -> ProducerGraph'
fromProducerGraph = Map.map _producerSet . _producerMap . _producerGraph

-- Collects the consumers from the syntax tree.
-- A consumer can be a case expression scrutinee,
-- or the left-hand side of a bind, if it is of form: <tag> <fields> <- pure v.
-- Only the producers of these consumers will be interesting (the others are unused).
-- NOTE: VarTagNode?
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

collectActiveProducers :: LVAResult -> Exp -> Set Name
collectActiveProducers lvaResult = selectActiveProducers lvaResult . collectProducers

-- Collects the producers from the syntax tree.
-- NOTE: VarTagNode
collectProducers :: Exp -> Set Name
collectProducers = flip execState mempty . cataM alg where

  alg :: ExpF Exp -> State (Set Name) Exp
  alg = \case
    e@(EBindF (SReturn (ConstTagNode t args)) (Var v) rhs) -> do
      modify $ Set.insert v
      pure . embed $ e
    e -> pure . embed $ e


-- Selects the active producers from a producer set.
-- A producers is active if at least one of its tags has a live field.
selectActiveProducers :: LVAResult -> Set Name -> Set Name
selectActiveProducers lvaResult prods = Map.keysSet
                                      . Map.filter hasActiveTag
                                      . Map.map nodeLiveness
                                      . producerLiveness
                                      $ lvaResult
  where

  producerLiveness :: LVAResult -> Map Name Liveness
  producerLiveness = flip Map.restrictKeys prods . _register

  nodeLiveness :: Liveness -> Map Tag (Vector Bool)
  nodeLiveness (NodeSet m) = Map.map _node m
  nodeLiveness _ = error "Producers cannot have non-node liveness information"

  hasActiveTag :: Map Tag (Vector Bool) -> Bool
  hasActiveTag = any (Vec.elem True) . Map.elems

-- Constructs the basic connection graph between all producers.
-- If a consumer has multiple producers with the same tag,
-- then one producer will be selected, and the others will be connected to it.
mkBasicProdGraph :: CByResult -> ProducerGraph'
mkBasicProdGraph cbyResult = flip execState mempty $ do
  let
    -- All the active producers found in the program grouped by tags.
    taggedGroups :: [(Tag, Set Name)]
    taggedGroups = concatMap (Map.toList . _producerSet)
                 . Map.elems
                 . _producerMap
                 . _producers
                 $ cbyResult

  forM taggedGroups $ \(t,ps) -> do
    let (p:_)  = Set.toList ps
        entry  = Map.singleton t ps
        update = Map.unionWith Set.union
    modify $ Map.insertWith update p entry

-- Constructs the connection graph between all producers.
-- First, it constructs the basic connection graph,
-- then it calculcates the basic graph's transitive closure.
groupAllProducers :: CByResult -> ProducerGraph
groupAllProducers = toProducerGraph
                  . transitiveClosure
                  . undirectedReflexiveClosure
                  . mkBasicProdGraph

-- Constructs the connection graph between the active producers.
-- First, it constructs the basic connection graph,
-- then it calculcates the basic graph's transitive closure.
groupActiveProducers :: LVAResult -> CByResult -> Exp -> ProducerGraph
groupActiveProducers lvaResult cbyResult e = toProducerGraph
                                           . transitiveClosure
                                           . undirectedReflexiveClosure
                                           . flip Map.restrictKeys activeProds
                                           . mkBasicProdGraph
                                           $ cbyResult
  where
    activeProds :: Set Name
    activeProds = collectActiveProducers lvaResult e


-- Creates an undirected graph from a directed one by connecting vertices
-- in both directions. Also connects each vertex with itself.
undirectedReflexiveClosure :: ProducerGraph' -> ProducerGraph'
undirectedReflexiveClosure m = flip execState m $ do
  let pList = Map.toList
            . Map.map Map.toList
            . Map.map (Map.map Set.toList)
            $ m
  -- for each (p, (t, [p1 .. pn])),
  -- it add the entries: (p1, (t, [p])) .. (pn, (t, [p]))
  -- also insert p into (p, (t, [p1 .. pn])),
  forM pList $ \(p, taggedGroups) ->
    forM taggedGroups $ \(t, ps) ->
      forM ps $ \p' -> do
        let entry  = Map.singleton t (Set.singleton p)
            itself = Map.singleton t (Set.singleton p)
            update = Map.unionWith Set.union
        modify $ Map.insertWith update p' entry   -- undirecting
        modify $ Map.insertWith update p  itself  -- reflexivity

-- Transitive clocure for undirected graphs.
transitiveClosure :: ProducerGraph' -> ProducerGraph'
transitiveClosure m
  | next <- tcStep m
  , next /= m
  = transitiveClosure next
  | otherwise = m
  where

  lookup' :: (Ord k, Monoid v) => k -> Map k v -> v
  lookup' k = fromMaybe mempty . Map.lookup k

  -- if p1 --t-> p2 and p2 --t-> p3 then p1 --t-> p3
  tcStep :: ProducerGraph' -> ProducerGraph'
  tcStep m = flip execState m $ do
    let pList = Map.toList
              . Map.map Map.toList
              . Map.map (Map.map Set.toList)
              $ m
    forM pList $ \(p, taggedGroups) ->
      forM taggedGroups $ \(t, ps) ->
        forM ps $ \p' -> do
          let entry  = (lookup' t . lookup' p' $ m) :: Set Name
              update = Map.adjust (Set.union entry) t
          modify $ Map.adjust update p
