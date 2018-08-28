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

-- the register mapping is always injective
-- (two different variables will always be stored in different registers)
reverseRegMap :: Map Name Reg -> Map Int Name
reverseRegMap = M.fromList
              . map (swap . mapSnd regToProd)
              . M.toList

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

toCByResult :: Map Name Reg -> HPTResult -> CByResult
toCByResult regMap HPTResult{..} = CByResult hptResult producers
  where mem  = V.map (over nodeTagMap (M.map dropProducer)) _memory
        regs = M.map simplifyTypeSet _register
        funs = M.map (over _2 (V.map simplifyTypeSet)) _function
        hptResult = HPTResult mem regs funs

        revRegMap = reverseRegMap regMap
        producers = M.map (ProducerSet . getNamedProducer') _register

        getNamedProducer' :: TypeSet -> Map Tag (Set Name)
        getNamedProducer' = M.map (getNamedProducer revRegMap)
                          . _nodeTagMap
                          . _nodeSet
