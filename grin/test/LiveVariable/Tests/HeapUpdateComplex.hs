module LiveVariable.Tests.HeapUpdateComplex where

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


heapUpdateComplexSrc :: FilePath
heapUpdateComplexSrc = lvaExamples </> "heap_update_complex.grin"

heapUpdateComplexSpec :: LVAResult -> Spec
heapUpdateComplexSpec found = it "heap_update_complex" $ found `sameAs` heapUpdateComplexExpected

heapUpdateComplexExpected :: LVAResult
heapUpdateComplexExpected = LVAResult
  { _memory   = heapUpdateComplexExpectedHeap
  , _register = heapUpdateComplexExpectedRegisters
  , _function = heapUpdateComplexExpectedFunctions
  }

heapUpdateComplexExpectedHeap :: Vector Liveness
heapUpdateComplexExpectedHeap = V.fromList
  [ nodeSet [ (cBool, [live])
            , (cNode, [live])
            ]
  , nodeSet [ (cWord, [dead])
            , (cNode, [live])
            ]
  ]

heapUpdateComplexExpectedRegisters :: Map Name Liveness
heapUpdateComplexExpectedRegisters = M.fromList
  [ ("n0", livenessN0)
  , ("n1", livenessN1)
  , ("n2", livenessN2)
  , ("n3", livenessN3)
  , ("p0", liveVal)
  , ("c0", liveVal)
  , ("c1", deadVal)
  , ("c2", liveVal)
  , ("c3", deadVal)
  ]
  where livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cWord, [dead]) ]
        livenessN2 = nodeSet [ (cNode, [live]) ]
        livenessN3 = nodeSet [ (cBool, [live])
                             , (cWord, [dead])
                             , (cNode, [live])
                             ]

heapUpdateComplexExpectedFunctions :: Map Name (Liveness, Vector Liveness)
heapUpdateComplexExpectedFunctions = mkFunctionLivenessMap []
