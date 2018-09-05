module LiveVariable.Tests.CaseNested where

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


caseNestedSrc :: FilePath
caseNestedSrc = lvaExamples </> "case_nested.grin"

caseNestedSpec :: LVAResult -> Spec
caseNestedSpec found = it "case_nested" $ found `sameAs` caseNestedExpected

caseNestedExpected :: LVAResult
caseNestedExpected = LVAResult
  { _memory   = caseNestedExpectedHeap
  , _register = caseNestedExpectedRegisters
  , _function = caseNestedExpectedFunctions
  }

caseNestedExpectedHeap :: Vector Liveness
caseNestedExpectedHeap = V.fromList []

caseNestedExpectedRegisters :: Map Name Liveness
caseNestedExpectedRegisters = M.fromList
  [ ("n0", livenessFRet)
  , ("c0", deadVal)
  , ("c1", liveVal)
  , ("c2", deadVal)
  , ("c3", deadVal)
  , ("c4", liveVal)
  , ("c5", deadVal)
  , ("c6", deadVal)
  , ("c7", deadVal)
  , ("b0", deadVal)
  , ("b1", deadVal)
  , ("x",  liveVal)
  ]

caseNestedExpectedFunctions :: Map Name (Liveness, Vector Liveness)
caseNestedExpectedFunctions = M.fromList
  [ ("f", fun (livenessFRet, [liveVal]))
  , ("grinMain", fun (livenessMainRet,[]))
  ]

livenessFRet :: Liveness
livenessFRet = nodeSet [ (cBool, [live])
                       , (cWord, [live])
                       ]

livenessMainRet :: Liveness
livenessMainRet = nodeSet [ (cBool, [live])
                          , (cWord, [live])
                          ]
