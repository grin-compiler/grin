module DeadCodeElimination.Tests.DeadData.Length where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadData.Util

(lengthBefore, lengthAfter, lengthSpec) = mkDDETestCase "length"
