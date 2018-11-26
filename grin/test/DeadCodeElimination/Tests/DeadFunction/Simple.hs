module DeadCodeElimination.Tests.DeadFunction.Simple where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadFunction.Util

(simpleBefore, simpleAfter, simpleSpec) = mkDFETestCase "simple"
