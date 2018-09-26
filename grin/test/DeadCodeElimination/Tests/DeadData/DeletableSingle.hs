module DeadCodeElimination.Tests.DeadData.DeletableSingle where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.Util

(deletableSingleSrc, deletableSingleSpec) = mkDDETestCase "deletable_single"
