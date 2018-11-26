module LiveVariable.Tests.Undefined where

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


undefinedSrc :: FilePath
undefinedSrc = lvaExamples </> "undefined.grin"

undefinedSpec :: LVAResult -> Spec
undefinedSpec found = it "undefined" $ found `sameAs` undefinedExpected

undefinedExpected :: LVAResult
undefinedExpected = LVAResult
  { _memory   = undefinedExpectedHeap
  , _register = undefinedExpectedRegisters
  , _function = undefinedExpectedFunctions
  }

undefinedExpectedHeap :: Vector Liveness
undefinedExpectedHeap = V.fromList 
  [ deadNodeSet [ (cNil, 0) ]
  , cConsBothDead
  ]

undefinedExpectedRegisters :: Map Name Liveness
undefinedExpectedRegisters = M.fromList
  [ ("p0", deadVal)
  , ("p1", deadVal)
  , ("x0", deadVal)
  , ("x1", liveVal)
  , ("n0", livenessN0)
  , ("n1", livenessN1)
  , ("n2", livenessN2)
  , ("c0", liveVal)
  , ("c1", deadVal)
  ]

undefinedExpectedFunctions :: Map Name (Liveness, Vector Liveness)
undefinedExpectedFunctions = mkFunctionLivenessMap []

livenessN0, livenessN1, livenessN2 :: Liveness
livenessN0 = cConsBothDead
livenessN1 = cConsBothDead
livenessN2 = nodeSet [ (cCons, [live, dead]) ]

cConsBothDead :: Liveness 
cConsBothDead = deadNodeSet [ (cCons, 2) ]