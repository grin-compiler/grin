module LiveVariable.Tests.CaseBackwardPropagationNodes where

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


caseBackwardPropagationNodesSrc :: FilePath
caseBackwardPropagationNodesSrc = lvaExamples </> "case_backward_propagation_nodes.grin"

caseBackwardPropagationNodesSpec :: LVAResult -> Spec
caseBackwardPropagationNodesSpec found = it "case_backward_propagation_nodes" $ found `sameAs` caseBackwardPropagationNodesExpected

caseBackwardPropagationNodesExpected :: LVAResult
caseBackwardPropagationNodesExpected = LVAResult
  { _memory   = caseBackwardPropagationNodesExpectedHeap
  , _register = caseBackwardPropagationNodesExpectedRegisters
  , _function = caseBackwardPropagationNodesExpectedFunctions
  }

caseBackwardPropagationNodesExpectedHeap :: Vector Liveness
caseBackwardPropagationNodesExpectedHeap = V.fromList []

caseBackwardPropagationNodesExpectedRegisters :: Map Name Liveness
caseBackwardPropagationNodesExpectedRegisters = M.fromList
  [ ("n0", livenessFRet)
  , ("n1", livenessFRet)
  , ("n2", livenessFRet)
  , ("n3", livenessFRet)
  , ("n4", livenessFRet)
  , ("c0", deadVal)
  , ("c1", liveVal)
  , ("b0", liveVal)
  , ("b1", liveVal)
  , ("x",  liveVal)
  ]

caseBackwardPropagationNodesExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseBackwardPropagationNodesExpectedFunctions = M.fromList
  [ ("f",        fun (livenessFRet, [liveVal]))
  , ("grinMain", fun (livenessFRet, []))
  ]

livenessFRet :: Liveness
livenessFRet = nodeSet [ (cInt,  [live])
                       , (cBool, [live])
                       , (cWord, [live])
                       ]
