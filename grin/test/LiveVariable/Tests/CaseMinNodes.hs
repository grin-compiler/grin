module LiveVariable.Tests.CaseMinNodes where

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


caseMinNodesSrc :: FilePath
caseMinNodesSrc = lvaExamples </> "case_min_nodes.grin"

caseMinNodesSpec :: LVAResult -> Spec
caseMinNodesSpec found = it "case_min_nodes" $ found `sameAs` caseMinNodesExpected

caseMinNodesExpected :: LVAResult
caseMinNodesExpected = LVAResult
  { _memory   = caseMinNodesExpectedHeap
  , _register = caseMinNodesExpectedRegisters
  , _function = caseMinNodesExpectedFunctions
  }

caseMinNodesExpectedHeap :: Vector Liveness
caseMinNodesExpectedHeap = V.fromList []

caseMinNodesExpectedRegisters :: Map Name Liveness
caseMinNodesExpectedRegisters = M.fromList
  [ ("n0", livenessN0)
  , ("n1", livenessN1)
  , ("c0", liveVal)
  , ("c1", deadVal)
  , ("b0", liveVal)
  ]
  where livenessN0 = nodeSet [ (cBool, [live]) ]
        livenessN1 = nodeSet [ (cNode, [live]) ]

caseMinNodesExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseMinNodesExpectedFunctions = mkFunctionLivenessMap []
