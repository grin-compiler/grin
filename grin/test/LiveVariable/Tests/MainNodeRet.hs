module LiveVariable.Tests.MainNodeRet where

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


mainNodeRetSrc :: FilePath
mainNodeRetSrc = lvaExamples </> "main_node_ret.grin"

mainNodeRetSpec :: LVAResult -> Spec
mainNodeRetSpec found = it "main_node_ret" $ found `sameAs` mainNodeRetExpected

mainNodeRetExpected :: LVAResult
mainNodeRetExpected = LVAResult
  { _memory   = mainNodeRetExpectedHeap
  , _register = mainNodeRetExpectedRegisters
  , _function = mainNodeRetExpectedFunctions
  }

mainNodeRetExpectedHeap :: Vector Liveness
mainNodeRetExpectedHeap = V.fromList []

mainNodeRetExpectedRegisters :: Map Name Liveness
mainNodeRetExpectedRegisters = M.fromList
  [ ("a", liveVal)
  , ("b", liveVal)
  , ("n", livenessN)
  ]

mainNodeRetExpectedFunctions :: Map Name (Liveness, Vector Liveness)
mainNodeRetExpectedFunctions = M.fromList
  [ ("grinMain", fun (livenessN, [])) ]

livenessN :: Liveness
livenessN = nodeSet [ (cTwo, [live, live]) ]
