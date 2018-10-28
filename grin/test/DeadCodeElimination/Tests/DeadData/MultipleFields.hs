module DeadCodeElimination.Tests.DeadData.MultipleFields where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadData.Util

(multipleFieldsBefore, multipleFieldsAfter, multipleFieldsSpec) = mkDDETestCase "multiple_fields"
