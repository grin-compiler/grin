module DeadCodeElimination.Tests.DeadData.DeletableMulti where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadData.Util

(deletableMultiBefore, deletableMultiAfter, deletableMultiSpec) = mkDDETestCase "deletable_multi"
