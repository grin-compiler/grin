module DeadCodeElimination.Tests.DeadParam.PNode where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadParam.Util

(pNodeBefore, pNodeAfter, pNodeSpec) = mkDPETestCase "pnode"
