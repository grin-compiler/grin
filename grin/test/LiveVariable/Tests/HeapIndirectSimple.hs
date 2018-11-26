module LiveVariable.Tests.HeapIndirectSimple where

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


heapIndirectSimpleSrc :: FilePath
heapIndirectSimpleSrc = lvaExamples </> "heap_indirect_simple.grin"

heapIndirectSimpleSpec :: LVAResult -> Spec
heapIndirectSimpleSpec found = it "heap_indirect_simple" $ found `sameAs` heapIndirectSimpleExpected

heapIndirectSimpleExpected :: LVAResult
heapIndirectSimpleExpected = LVAResult
  { _memory   = heapIndirectSimpleExpectedHeap
  , _register = heapIndirectSimpleExpectedRegisters
  , _function = heapIndirectSimpleExpectedFunctions
  }

heapIndirectSimpleExpectedHeap :: Vector Liveness
heapIndirectSimpleExpectedHeap = V.fromList
  [ nodeSet [ (cNil, []) ]
  ]

heapIndirectSimpleExpectedRegisters :: Map Name Liveness
heapIndirectSimpleExpectedRegisters = M.fromList
  [ ("a0", deadVal)
  , ("n0", livenessN0)
  , ("p0", liveVal)
  , ("n1", livenessN1)
  , ("r", livenessN0)
  , ("x", deadVal)
  , ("xs", liveVal)
  , ("xs'", livenessN0)
  ]
  where livenessN0 = nodeSet [ (cNil, [])
                             ]
        livenessN1 = nodeSet [ (cCons, [dead,live])
                             ]

heapIndirectSimpleExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapIndirectSimpleExpectedFunctions = M.fromList
  [ ("grinMain", fun (livenessMainRet,[]))
  ]

livenessMainRet :: Liveness
livenessMainRet = nodeSet [ (cNil, [])
                          ]
