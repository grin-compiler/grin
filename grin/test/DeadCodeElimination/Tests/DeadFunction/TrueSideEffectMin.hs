module DeadCodeElimination.Tests.DeadFunction.TrueSideEffectMin where

import System.FilePath

import Grin.Grin

import Test.Hspec
import Test.Assertions

import DeadCodeElimination.Tests.DeadFunction.Util

(trueSideEffectMinBefore, trueSideEffectMinAfter, trueSideEffectMinSpec) = mkDFETestCase "true_side_effect_min"
