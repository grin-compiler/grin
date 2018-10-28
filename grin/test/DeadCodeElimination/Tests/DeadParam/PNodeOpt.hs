module DeadCodeElimination.Tests.DeadParam.PNodeOpt where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadParam.Util

(pNodeOptBefore, pNodeOptAfter, pNodeOptSpec) = mkDPETestCase "pnode_opt"
