module LiveVariable.Tests.CaseRestrictedNodes where

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


caseRestrictedNodesSrc :: FilePath
caseRestrictedNodesSrc = lvaExamples </> "case_restricted_nodes.grin"

caseRestrictedNodesSpec :: LVAResult -> Spec
caseRestrictedNodesSpec found = it "case_restricted_nodes" $ found `sameAs` caseRestrictedNodesExpected

caseRestrictedNodesExpected :: LVAResult
caseRestrictedNodesExpected = LVAResult
  { _memory   = caseRestrictedNodesExpectedHeap
  , _register = caseRestrictedNodesExpectedRegisters
  , _function = caseRestrictedNodesExpectedFunctions
  }

caseRestrictedNodesExpectedHeap :: Vector Liveness
caseRestrictedNodesExpectedHeap = V.fromList []

caseRestrictedNodesExpectedRegisters :: Map Name Liveness
caseRestrictedNodesExpectedRegisters = M.fromList
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

caseRestrictedNodesExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseRestrictedNodesExpectedFunctions = M.fromList
  [ ("f",        fun (livenessFRet, [liveVal]))
  , ("grinMain", fun (livenessFRet, []))
  ]

livenessFRet :: Liveness
livenessFRet = nodeSet [ (cInt,  [live])
                       , (cBool, [live])
                       , (cWord, [live])
                       ]
