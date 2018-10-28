module DeadCodeElimination.Tests.DeadParam.MutuallyRecursive where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadParam.Util

(mutuallyRecursiveBefore, mutuallyRecursiveAfter, mutuallyRecursiveSpec) = mkDPETestCase "mutually_recursive"
