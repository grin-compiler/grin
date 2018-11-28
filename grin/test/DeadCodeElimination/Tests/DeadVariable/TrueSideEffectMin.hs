module DeadCodeElimination.Tests.DeadVariable.TrueSideEffectMin where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadVariable.Util

(trueSideEffectMinBefore, trueSideEffectMinAfter, trueSideEffectMinSpec) = mkDVETestCase "true_side_effect_min"
