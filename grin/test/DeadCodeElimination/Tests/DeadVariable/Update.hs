module DeadCodeElimination.Tests.DeadVariable.Update where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(updateBefore, updateAfter, updateSpec) = mkDVETestCase "update"
