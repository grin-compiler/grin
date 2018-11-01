module DeadCodeElimination.Tests.DeadVariable.Heap where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(heapBefore, heapAfter, heapSpec) = mkDVETestCase "heap"
