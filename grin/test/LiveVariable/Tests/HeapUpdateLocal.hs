module LiveVariable.Tests.HeapUpdateLocal where

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


heapUpdateLocalSrc :: FilePath
heapUpdateLocalSrc = lvaExamples </> "heap_update_local.grin"

heapUpdateLocalSpec :: LVAResult -> Spec
heapUpdateLocalSpec found = it "heap_update_local" $ found `sameAs` heapUpdateLocalExpected

heapUpdateLocalExpected :: LVAResult
heapUpdateLocalExpected = LVAResult
  { _memory   = heapUpdateLocalExpectedHeap
  , _register = heapUpdateLocalExpectedRegisters
  , _function = heapUpdateLocalExpectedFunctions
  }

heapUpdateLocalExpectedHeap :: Vector Liveness
heapUpdateLocalExpectedHeap = V.fromList
  [ nodeSet [ (cBool, [live])
            , (cWord, [dead])
            ]
  ]

heapUpdateLocalExpectedRegisters :: Map Name Liveness
heapUpdateLocalExpectedRegisters = M.fromList
  [ ("n0", livenessN0)
  , ("n1", livenessN1)
  , ("n2", livenessN2)
  , ("p0", liveVal)
  , ("c0", liveVal)
  , ("c1", deadVal)
  , ("c2", deadVal)
  ]
  where livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cWord, [dead]) ]
        livenessN2 = nodeSet [ (cBool, [live])
                             , (cWord, [dead])
                             ]

heapUpdateLocalExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapUpdateLocalExpectedFunctions = mkFunctionLivenessMap []
