module HeapPointsTo.Tests.Undefined where

import System.FilePath

import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Test.Util
import Test.Hspec
import Test.Assertions

import Grin.Grin
import AbstractInterpretation.HPTResult

import HeapPointsTo.Tests.Util

undefinedSrc :: FilePath
undefinedSrc = hptExamples </> "undefined.grin"

undefinedSpec :: HPTResult -> Spec
undefinedSpec found = it "undefined" $ found `sameAs` undefinedExpected

undefinedExpected :: HPTResult
undefinedExpected = HPTResult
  { _memory   = undefinedExpectedHeap
  , _register = undefinedExpectedRegisters
  , _function = undefinedExpectedFunctions
  }

nodeSetN0 :: NodeSet
nodeSetN0 = mkNodeSet [(cCons, [[T_Int64], [locT 0, locT 1]])]

locTP0 :: SimpleType
locTP0 = locT 0

undefinedExpectedHeap :: Vector NodeSet
undefinedExpectedHeap = V.fromList
  [ mkNodeSet [(cNil, [])]
  , mkNodeSet [(cCons, [[T_Int64], [locTP0]])]
  , nodeSetN0
  ]

undefinedExpectedRegisters :: Map Name TypeSet
undefinedExpectedRegisters = M.fromList
  [ ("p0", loc 0)
  , ("p1", loc 1)
  , ("p2", loc 2)
  , ("x0", tySetFromTypes [T_Int64])
  , ("n0", tySetFromNodeSet nodeSetN0)
  , ("n1", typeN1)
  , ("n2", typeN2)
  ]
  where typeN1 = mkTySet [ (cCons, [[T_Int64], [locT 2]])
                         , (cNil, [])
                         ]
        typeN2 = mkTySet [ (cCons, [[T_Int64], [locTP0]])
                         ]

undefinedExpectedFunctions :: Map Name (TypeSet, Vector TypeSet)
undefinedExpectedFunctions = M.singleton "grinMain" (mkSimpleMain T_Int64)