module LiveVariable.Tests.DeadTags where

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


deadTagsSrc :: FilePath
deadTagsSrc = lvaExamples </> "dead_tags.grin"

deadTagsSpec :: LVAResult -> Spec
deadTagsSpec found = it "dead_tags" $ found `sameAs` deadTagsExpected

deadTagsExpected :: LVAResult
deadTagsExpected = LVAResult
  { _memory   = deadTagsExpectedHeap
  , _register = deadTagsExpectedRegisters
  , _function = deadTagsExpectedFunctions
  }

deadTagsExpectedHeap :: Vector Liveness
deadTagsExpectedHeap = V.fromList
  [ livenessN2
  , livenessN3
  ]

deadTagsExpectedRegisters :: Map Name Liveness
deadTagsExpectedRegisters = M.fromList
  [ ("p0", deadVal)
  , ("p1", deadVal)
  , ("n0", livenessN0)
  , ("n1", livenessN1)
  , ("n2", livenessN2)
  , ("n3", livenessN3)
  , ("n4", livenessN4)
  , ("c0", deadVal)
  ]

deadTagsExpectedFunctions :: Map Name (Liveness, Vector Liveness)
deadTagsExpectedFunctions = mkFunctionLivenessMap []

livenessN0, livenessN1, livenessN2 :: Liveness
livenessN0 = deadNodeSet [ (cNil, 0) ]
livenessN1 = deadNodeSet [ (cCons, 2) ]
livenessN2 = deadNodeSet [ (cNil, 0),  (cInt, 1) ]
livenessN3 = deadNodeSet [ (cCons, 2), (cInt, 1) ]
livenessN4 = deadNodeSet [ (cNil, 0), (cCons, 2),  (cInt, 1) ]
