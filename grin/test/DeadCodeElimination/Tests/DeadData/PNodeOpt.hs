module DeadCodeElimination.Tests.DeadData.PNodeOpt where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadData.Util

(pNodeOptBefore, pNodeOptAfter, pNodeOptSpec) = mkDDETestCase "pnode_opt"
