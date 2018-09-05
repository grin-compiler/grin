module LiveVariable.Tests.NodesTricky where

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


nodesTrickySrc :: FilePath
nodesTrickySrc = lvaExamples </> "nodes_tricky.grin"

nodesTrickySpec :: LVAResult -> Spec
nodesTrickySpec found = it "nodes_tricky" $ found `sameAs` nodesTrickyExpected

nodesTrickyExpected :: LVAResult
nodesTrickyExpected = LVAResult
  { _memory   = nodesTrickyExpectedHeap
  , _register = nodesTrickyExpectedRegisters
  , _function = nodesTrickyExpectedFunctions
  }

nodesTrickyExpectedHeap :: Vector Liveness
nodesTrickyExpectedHeap = V.fromList []

nodesTrickyExpectedRegisters :: Map Name Liveness
nodesTrickyExpectedRegisters = M.fromList
  [ ("n0", livenessN0)
  , ("c0", deadVal)
  , ("c1", deadVal)
  , ("c2", liveVal)
  , ("c3", deadVal)
  , ("x",  liveVal)
  ]

nodesTrickyExpectedFunctions :: Map Name (Liveness, Vector Liveness)
nodesTrickyExpectedFunctions = mkFunctionLivenessMap
  [ ("f", fun (livenessN0, [liveVal])) ]

livenessN0 :: Liveness
livenessN0 = nodeSet [ (cOne, [dead])
                     , (cTwo, [live, dead])
                     ]
