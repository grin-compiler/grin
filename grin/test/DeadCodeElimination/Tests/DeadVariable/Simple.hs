module DeadCodeElimination.Tests.DeadVariable.Simple where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(simpleBefore, simpleAfter, simpleSpec) = mkDVETestCase "simple"
