module DeadCodeElimination.Tests.DeadParam.Simple where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadParam.Util

(simpleBefore, simpleAfter, simpleSpec) = mkDPETestCase "simple"
