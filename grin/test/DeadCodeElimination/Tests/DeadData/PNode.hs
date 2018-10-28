module DeadCodeElimination.Tests.DeadData.PNode where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadData.Util

(pNodeBefore, pNodeAfter, pNodeSpec) = mkDDETestCase "pnode"
