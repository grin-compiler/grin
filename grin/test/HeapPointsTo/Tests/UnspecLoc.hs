module HeapPointsTo.Tests.UnspecLoc where 

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

unspecLocSrc :: FilePath
unspecLocSrc = hptExamples </> "unspec_loc.grin"

unspecLocSpec :: HPTResult -> Spec 
unspecLocSpec found = it "unspec_loc" $ found `sameAs` unspecLocExpected

unspecLocExpected :: HPTResult
unspecLocExpected = HPTResult
  { _memory   = unspecLocExpectedHeap
  , _register = unspecLocExpectedRegisters
  , _function = unspecLocExpectedFunctions
  }

nodeSetN0, nodeSetN1 :: NodeSet
nodeSetN0 = mkNodeSet [(cInt,  [[T_Int64]])]
nodeSetN1 = mkNodeSet [(cNode, [[unspecLocT]])]


unspecLocExpectedHeap :: Vector NodeSet
unspecLocExpectedHeap = V.fromList 
  [ nodeSetN0
  ]

unspecLocExpectedRegisters :: Map Name TypeSet 
unspecLocExpectedRegisters = M.fromList
  [ ("p0", tySetFromTypes [locT 0, unspecLocT])
  , ("p1", unspecLoc)
  , ("n0", tySetFromNodeSet nodeSetN0)
  , ("n1", tySetFromNodeSet nodeSetN1)
  , ("x0", tySetFromTypes [])
  ]

unspecLocExpectedFunctions :: Map Name (TypeSet, Vector TypeSet)
unspecLocExpectedFunctions = M.singleton "grinMain" (mkSimpleMain T_Unit)