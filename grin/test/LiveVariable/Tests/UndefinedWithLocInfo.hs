module LiveVariable.Tests.UndefinedWithLocInfo where

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


undefinedWithLocInfoSrc :: FilePath
undefinedWithLocInfoSrc = lvaExamples </> "undefined_with_loc_info.grin"

undefinedWithLocInfoSpec :: LVAResult -> Spec
undefinedWithLocInfoSpec found = it "undefined_with_loc_info" $ found `sameAs` undefinedWithLocInfoExpected

undefinedWithLocInfoExpected :: LVAResult
undefinedWithLocInfoExpected = LVAResult
  { _memory   = undefinedWithLocInfoExpectedHeap
  , _register = undefinedWithLocInfoExpectedRegisters
  , _function = undefinedWithLocInfoExpectedFunctions
  }

undefinedWithLocInfoExpectedHeap :: Vector Liveness
undefinedWithLocInfoExpectedHeap = V.fromList
  [ cNilLiveness
  ]

undefinedWithLocInfoExpectedRegisters :: Map Name Liveness
undefinedWithLocInfoExpectedRegisters = M.fromList
  [ ("p0", deadVal)
  , ("x0", cNilLiveness)
  , ("n0", livenessN0)
  , ("c0", liveVal)
  , ("c1", deadVal)
  ]

undefinedWithLocInfoExpectedFunctions :: Map Name (Liveness, Vector Liveness)
undefinedWithLocInfoExpectedFunctions = M.fromList
  [ ("grinMain", fun (cNilLiveness, [])) ]

livenessN0 :: Liveness
livenessN0 = nodeSet [ (cCons, [live, dead]) ]

cNilLiveness :: Liveness
cNilLiveness = nodeSet [ (cNil, []) ]