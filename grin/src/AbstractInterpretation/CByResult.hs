{-# LANGUAGE RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving #-}

module AbstractInterpretation.CByResult where

import Data.Tuple (swap)

import Data.Set    (Set)
import Data.Map    (Map)
import Data.Vector (Vector)
import qualified Data.Set    as S
import qualified Data.Map    as M
import qualified Data.Vector as V

import Lens.Micro.Platform
import Lens.Micro.Internal

import Grin.Grin (Name, Tag)
import AbstractInterpretation.HPTResult
import AbstractInterpretation.IR (Reg(..))
import AbstractInterpretation.Reduce (Computer)
import AbstractInterpretation.CreatedBy as CBy (CByProgram(..), HPTWProducerInfo(..))

-- HPTResult with producer info
type HPTResultP = HPTResult
type Producer   = Int

-- possible producers grouped by tags
newtype ProducerMap = ProducerMap { _producerMap :: Map Name ProducerSet }
  deriving (Eq, Show)
newtype ProducerSet = ProducerSet { _producerSet :: Map Tag (Set Name)   }
  deriving (Eq, Show)

instance Monoid ProducerSet where
  mempty = ProducerSet M.empty
  mappend (ProducerSet x) (ProducerSet y) = ProducerSet $ M.unionWith mappend x y

instance Monoid ProducerMap where
  mempty = ProducerMap M.empty
  mappend (ProducerMap x) (ProducerMap y) = ProducerMap $ M.unionWith mappend x y

data CByResult
  = CByResult
  { _hptResult :: HPTResult
  , _producers :: ProducerMap
  }

concat <$> mapM makeLenses [''ProducerMap, ''ProducerSet, ''CByResult]

-- node with its possible producers in its first field
type NodeP    = Node
-- typeSet with producer info for its nodeSet
type TypeSetP = TypeSet

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x,y) = (x, f y)

regToProd :: Reg -> Producer
regToProd (Reg i) = fromIntegral i

toProdMap :: Map Reg Name -> Map Producer Name
toProdMap = M.mapKeys regToProd

-- the producers will be interpreted as heap locations
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
  where prodMap = toProdMap . CBy._producerMap $ cbyProg
        hptProg = _hptProg . _hptProgWProd $ cbyProg
        hptProdResult@HPTResult{..} = toHPTResult hptProg comp

        mem  = V.map (over nodeTagMap (M.map dropProducer)) _memory
        regs = M.map simplifyTypeSet _register
        funs = M.map (over _2 (V.map simplifyTypeSet)) _function
        hptResult = HPTResult mem regs funs

        producers = ProducerMap $ M.map (ProducerSet . getNamedProducer') _register

        getNamedProducer' :: TypeSet -> Map Tag (Set Name)
        getNamedProducer' = M.map (getNamedProducer prodMap)
                          . _nodeTagMap
                          . _nodeSet
