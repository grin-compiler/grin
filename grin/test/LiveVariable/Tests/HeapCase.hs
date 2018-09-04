module LiveVariable.Tests.HeapCase where

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


heapCaseSrc :: FilePath
heapCaseSrc = lvaExamples </> "heap_case.grin"

heapCaseSpec :: LVAResult -> Spec
heapCaseSpec found = it "heap_case" $ found `sameAs` heapCaseExpected

heapCaseExpected :: LVAResult
heapCaseExpected = LVAResult
  { _memory   = heapCaseExpectedHeap
  , _register = heapCaseExpectedRegisters
  , _function = heapCaseExpectedFunctions
  }

heapCaseExpectedHeap :: Vector Liveness
heapCaseExpectedHeap = V.fromList
  [ nodeSet [ (cWord, [dead]) ]
  , nodeSet [ (cBool, [live]) ]
   ]

heapCaseExpectedRegisters :: Map Name Liveness
heapCaseExpectedRegisters = M.fromList
  [ ("n0", livenessN0)
  , ("p0", liveVal)
  , ("c0", deadVal)
  , ("a0", liveVal)
  , ("c1", liveVal)
  ]
  where livenessN0 = nodeSet [ (cBool, [live]) ]

heapCaseExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapCaseExpectedFunctions = mkFunctionLivenessMap []

livenessLoc1 :: Liveness
livenessLoc1 = nodeSet [ (cBool, [live]) ]
