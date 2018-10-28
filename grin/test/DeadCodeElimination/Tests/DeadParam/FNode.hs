module DeadCodeElimination.Tests.DeadParam.FNode where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadParam.Util

(fNodeBefore, fNodeAfter, fNodeSpec) = mkDPETestCase "fnode"
