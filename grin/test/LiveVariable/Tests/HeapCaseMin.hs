module LiveVariable.Tests.HeapCaseMin where

import Data.Map    (Map)
import Data.Vector (Vector)

import qualified Data.Map    as M
import qualified Data.Vector as V

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import AbstractInterpretation.LiveVariable hiding (live)
import AbstractInterpretation.LVAResult

import LiveVariable.Tests.Util


heapCaseMinSrc :: FilePath
heapCaseMinSrc = lvaExamples </> "heap_case_min.grin"

heapCaseMinSpec :: LVAResult -> Spec
heapCaseMinSpec found = it "heap_case_min" $ found `sameAs` heapCaseMinExpected

heapCaseMinExpected :: LVAResult
heapCaseMinExpected = LVAResult
  { _memory   = heapCaseMinExpectedHeap
  , _register = heapCaseMinExpectedRegisters
  , _function = heapCaseMinExpectedFunctions
  }

heapCaseMinExpectedHeap :: Vector Liveness
heapCaseMinExpectedHeap = V.fromList [ livenessLoc1 ]

heapCaseMinExpectedRegisters :: Map Name Liveness
heapCaseMinExpectedRegisters = M.fromList
  [ ("p0", liveVal)
  , ("c1", liveVal)
  , ("a1", liveVal)
  ]

heapCaseMinExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapCaseMinExpectedFunctions = mkFunctionLivenessMap []

livenessLoc1 :: Liveness
livenessLoc1 = nodeSet [ (cBool, [live]) ]
