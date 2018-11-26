module DeadCodeElimination.Tests.DeadVariable.ReplaceStore where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(replaceStoreBefore, replaceStoreAfter, replaceStoreSpec) = mkDVETestCase "replace_store"
