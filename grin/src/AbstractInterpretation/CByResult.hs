{-# LANGUAGE RecordWildCards #-}

module AbstractInterpretation.CByResult where

import Data.Tuple (swap)

import Data.Set    (Set)
import Data.Map    (Map)
import Data.Vector (Vector)
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.Vector as V

import Lens.Micro.Platform

import Grin.Grin (Name, Tag)
import AbstractInterpretation.HPTResult
import AbstractInterpretation.IR (Reg(..))
import AbstractInterpretation.Reduce (Computer)
import AbstractInterpretation.CreatedBy (CByProgram(..), HPTWProducerInfo(..))

-- HPTResult with producer info
type HPTResultP = HPTResult
type Producer   = Int

-- possible producers grouped by tags
newtype ProducerSet = ProducerSet { _producerSet :: Map Tag (Set Name)}

data CByResult
  = CByResult
  { _hptResult :: HPTResult
  , _producers :: Map Name ProducerSet
  }

-- node with its possible producers in its first field
type NodeP    = Vector (Set SimpleType)
-- typeSet with producer info for its nodeSet
type TypeSetP = TypeSet

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x,y) = (x, f y)

regToProd :: Reg -> Producer
regToProd (Reg i) = fromIntegral i

toProdMap :: Map Reg Name -> Map Producer Name
toProdMap = M.mapKeys regToProd

toProducer :: SimpleType -> Producer
toProducer (T_Location n) = n
toProducer t = error $ "Incorrect information for producer. Expected T_Location Int, got: " ++ show t

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
getNamedProducer regs = S.map (regs M.!) . fst . extractProducer

extractProducer :: NodeP -> (Set Producer, Node)
extractProducer nodeP = (S.map toProducer ps, node)
  where (ps,node) = unsafeUncons nodeP

toCByResult :: CByProgram -> Computer -> CByResult
toCByResult cbyProg comp = CByResult hptResult producers
  where prodMap = toProdMap . _producerMap $ cbyProg
        hptProg = _hptProg . _hptProgWProd $ cbyProg
        hptProdResult@HPTResult{..} = toHPTResult hptProg comp

        mem  = V.map (over nodeTagMap (M.map dropProducer)) _memory
        regs = M.map simplifyTypeSet _register
        funs = M.map (over _2 (V.map simplifyTypeSet)) _function
        hptResult = HPTResult mem regs funs

        producers = M.map (ProducerSet . getNamedProducer') _register

        getNamedProducer' :: TypeSet -> Map Tag (Set Name)
        getNamedProducer' = M.map (getNamedProducer prodMap)
                          . _nodeTagMap
                          . _nodeSet
