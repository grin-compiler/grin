module LiveVariable.Tests.HeapSimple where

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


heapSimpleSrc :: FilePath
heapSimpleSrc = lvaExamples </> "heap_simple.grin"

heapSimpleSpec :: LVAResult -> Spec
heapSimpleSpec found = it "heap_simple" $ found `sameAs` heapSimpleExpected

heapSimpleExpected :: LVAResult
heapSimpleExpected = LVAResult
  { _memory   = heapSimpleExpectedHeap
  , _register = heapSimpleExpectedRegisters
  , _function = heapSimpleExpectedFunctions
  }

heapSimpleExpectedHeap :: Vector Liveness
heapSimpleExpectedHeap = V.fromList
  [ nodeSet $ [ (cTwo, [live, dead]) ] ]

heapSimpleExpectedRegisters :: Map Name Liveness
heapSimpleExpectedRegisters = M.fromList
  [ ("n", livenessN)
  , ("p", liveVal)
  , ("x", livenessX)
  , ("a", liveVal)
  , ("b", deadVal)
  ]
  where livenessN = nodeSet $ [ (cTwo, [live, dead]) ]
        livenessX = livenessN

heapSimpleExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapSimpleExpectedFunctions = mkFunctionLivenessMap []
