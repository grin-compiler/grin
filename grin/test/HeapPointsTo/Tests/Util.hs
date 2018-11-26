module HeapPointsTo.Tests.Util where 

import System.FilePath

import Data.Monoid

import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Grin.Grin
import AbstractInterpretation.HPTResult


hptExamples :: FilePath
hptExamples = "HeapPointsTo" </> "examples"

unspecLocT :: SimpleType
unspecLocT = T_UnspecifiedLocation

unspecLoc :: TypeSet
unspecLoc = tySetFromTypes [T_UnspecifiedLocation]

locT :: Int -> SimpleType
locT = T_Location

loc :: Int -> TypeSet
loc = tySetFromTypes . pure . locT

mkNode :: [[SimpleType]] -> Node
mkNode = V.fromList . map S.fromList

mkNodeSet :: [(Tag, [[SimpleType]])] -> NodeSet 
mkNodeSet = NodeSet . M.fromList . map (\(t,v) -> (t,mkNode v)) 

mkTySet :: [(Tag, [[SimpleType]])] -> TypeSet 
mkTySet = tySetFromNodeSet . mkNodeSet

tySetFromNodeSet :: NodeSet -> TypeSet 
tySetFromNodeSet = TypeSet mempty

tySetFromTypes :: [SimpleType] -> TypeSet 
tySetFromTypes = flip TypeSet mempty . S.fromList

mkFun :: (TypeSet, [TypeSet]) -> (TypeSet, Vector TypeSet)
mkFun (t,ts) = (t, V.fromList ts)

mkSimpleMain :: SimpleType -> (TypeSet, Vector TypeSet)
mkSimpleMain t = (tySetFromTypes [t], mempty) 