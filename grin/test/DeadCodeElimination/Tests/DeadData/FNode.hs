module DeadCodeElimination.Tests.DeadData.FNode where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.Util

(fNodeBefore, fNodeAfter, fNodeSpec) = mkDDETestCase "fnode"
