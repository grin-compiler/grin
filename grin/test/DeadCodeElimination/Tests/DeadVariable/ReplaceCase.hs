module DeadCodeElimination.Tests.DeadVariable.ReplaceCase where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(replaceCaseBefore, replaceCaseAfter, replaceCaseSpec) = mkDVETestCase "replace_case"
