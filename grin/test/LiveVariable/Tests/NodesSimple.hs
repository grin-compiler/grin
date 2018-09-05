module LiveVariable.Tests.NodesSimple where

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


nodesSimpleSrc :: FilePath
nodesSimpleSrc = lvaExamples </> "nodes_simple.grin"

nodesSimpleSpec :: LVAResult -> Spec
nodesSimpleSpec found = it "nodes_simple" $ found `sameAs` nodesSimpleExpected

nodesSimpleExpected :: LVAResult
nodesSimpleExpected = LVAResult
  { _memory   = nodesSimpleExpectedHeap
  , _register = nodesSimpleExpectedRegisters
  , _function = nodesSimpleExpectedFunctions
  }

nodesSimpleExpectedHeap :: Vector Liveness
nodesSimpleExpectedHeap = V.fromList []

nodesSimpleExpectedRegisters :: Map Name Liveness
nodesSimpleExpectedRegisters = M.fromList
  [ ("a0", liveVal)
  , ("a1", liveVal)
  , ("a2", deadVal)
  , ("n0", livenessN0)
  , ("c0", liveVal)
  , ("c1", deadVal)
  , ("x",  liveVal)
  , ("y",  liveVal)
  , ("z",  deadVal)
  ]
  where livenessN = nodeSet [ (cFoo, [live]) ]

nodesSimpleExpectedFunctions :: Map Name (Liveness, Vector Liveness)
nodesSimpleExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (livenessN0, [liveVal, liveVal, deadVal])) ]

livenessN0 :: Liveness
livenessN0 = nodeSet [ (cInt,  [live])
                     , (cBool, [dead])]
