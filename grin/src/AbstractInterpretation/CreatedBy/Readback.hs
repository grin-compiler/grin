{-# LANGUAGE RecordWildCards #-}

module AbstractInterpretation.CreatedBy.Readback where

import Data.Set    (Set)
import Data.Map    (Map)
import Data.Vector (Vector)
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.Vector as V

import Data.Maybe

import Lens.Micro.Platform

import Grin.Grin (Name, Tag)
import AbstractInterpretation.IR (Reg(..))
import AbstractInterpretation.Reduce (ComputerState)
import AbstractInterpretation.CreatedBy.CodeGen as CBy hiding (Producer)
import AbstractInterpretation.CreatedBy.Util
import AbstractInterpretation.CreatedBy.Result
import AbstractInterpretation.HeapPointsTo.Result
import AbstractInterpretation.LiveVariable.Result (LVAResult)


-- HPTResult with producer info
type HPTResultP = HPTResult
type Producer   = Int

-- node with its possible producers in its first field
type NodeP    = Node
-- typeSet with producer info for its nodeSet
type TypeSetP = TypeSet

regToProd :: Reg -> Producer
regToProd (Reg i) = fromIntegral i

toProdMap :: Map Reg Name -> Map Producer Name
toProdMap = M.mapKeys regToProd

-- Adds the undefined producer to a producer mapping
withUndefined :: Map Producer Name -> Map Producer Name
withUndefined = M.insert udProdId udProdName
  where udProdId   = fromIntegral undefinedProducer
        udProdName = undefinedProducerName

-- the heap locations will be interpreted as producers
-- also, the undefined value will hold the undefined producer's id
toProducer :: SimpleType -> Producer
toProducer (T_Location  n) = n
toProducer (Local UndefinedProducer) = fromIntegral . fromHPTLocal $ UndefinedProducer
toProducer t = error $ "Incorrect information for producer. Expected T_Location Int or the undefined producer, got: " ++ show t

-- removes the producers info from nodes
dropProducer :: NodeP -> Node
dropProducer = V.tail

-- removes the producer info from the nodes in a typeSet
simplifyTypeSet :: TypeSetP -> TypeSet
simplifyTypeSet = over (nodeSet.nodeTagMap) (M.map dropProducer)

unsafeUncons :: Vector a -> (a, Vector a)
unsafeUncons = (,) <$> V.head <*> V.tail

getProducer :: NodeP -> Set Producer
getProducer = fst . extractProducer

-- we assume that the producer will always be present in the register mapping
getNamedProducer :: Map Producer Name -> NodeP -> Set Name
getNamedProducer regs = S.map (`lookupE` regs) . fst . extractProducer
  where lookupE k m = fromMaybe (error $ hasNoName k) $ M.lookup k m
        hasNoName p = "Producer with id " ++ show p ++ " has no name. " ++ fix
        fix = "Possible fix: run producer name introduction before the created-by analysis"

extractProducer :: NodeP -> (Set Producer, Node)
extractProducer nodeP = (S.map toProducer ps, node)
  where (ps,node) = unsafeUncons nodeP

toCByResult :: CByMapping -> ComputerState -> CByResult
toCByResult CByMapping{..} comp = CByResult hptResult producers groupedProducers
  where prodMap = withUndefined . toProdMap $ _producerMap
        hptProdResult@HPTResult{..} = toHPTResult _hptMapping comp

        mem  = V.map (over nodeTagMap (M.map dropProducer)) _memory
        regs = M.map simplifyTypeSet _register
        funs = M.map (over _1 simplifyTypeSet)
             . M.map (over _2 (V.map simplifyTypeSet))
             $ _function
        hptResult = HPTResult mem regs funs

        producers = ProducerMap $ M.map (ProducerSet . getNamedProducer') _register

        groupedProducers = All $ groupAllProducers producers

        getNamedProducer' :: TypeSet -> Map Tag (Set Name)
        getNamedProducer' = M.map (getNamedProducer prodMap)
                          . _nodeTagMap
                          . _nodeSet

toCByResultWithLiveness :: LVAResult -> CByMapping -> ComputerState -> CByResult
toCByResultWithLiveness lvaResult cbyMapping comp
  | CByResult hptResult producers _ <- toCByResult cbyMapping comp
  , groupedProducers <- Active $ groupActiveProducers lvaResult producers
  = CByResult hptResult producers groupedProducers
