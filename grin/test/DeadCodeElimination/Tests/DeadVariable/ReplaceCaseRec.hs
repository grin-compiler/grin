module DeadCodeElimination.Tests.DeadVariable.ReplaceCaseRec where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(replaceCaseRecBefore, replaceCaseRecAfter, replaceCaseRecSpec) = mkDVETestCase "replace_case_rec"
