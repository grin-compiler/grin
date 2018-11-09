module DeadCodeElimination.Tests.DeadFunction.MutuallyRecursive where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadFunction.Util

(mutuallyRecursiveBefore, mutuallyRecursiveAfter, mutuallyRecursiveSpec) = mkDFETestCase "mutually_recursive"
