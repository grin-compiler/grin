module Transformations.Optimising.DeadFunctionEliminationSpec where

import System.FilePath

import Data.Either (fromRight)

import Test.IO
import Test.Hspec
import Test.Util
import Test.Hspec.PipelineExample

import Grin.Grin
import Grin.TypeCheck

import AbstractInterpretation.LVAResultTypes
import Transformations.Optimising.DeadFunctionElimination
import Transformations.EffectMap

import DeadCodeElimination.Tests.Util

import AbstractInterpretation.LiveVariable as LiveVariable (codeGen)
import AbstractInterpretation.LVAResult (LVAResult(..), toLVAResult)
import AbstractInterpretation.Reduce (evalDataFlowInfo, _airComp)
import AbstractInterpretation.CreatedBy as CreatedBy (codeGen)
import AbstractInterpretation.CByResult (toCByResult)
import Pipeline.Definitions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Dead Function Elimination" $ do

    let deadFunctionEliminationPipeline =
          [ CBy Compile
          , CBy RunPure
          , LVA Compile
          , LVA RunPure
          , Eff CalcEffectMap
          , HPT Compile
          , HPT RunPure
          , T DeadFunctionElimination
          ]

    it "app_side_effect_1" $ pipeline
      "dead_code/app_side_effect_1.grin"
      "dead_fun/after/app_side_effect_1.grin"
      deadFunctionEliminationPipeline

    it "mutually_recursive" $ pipeline
      "dead_fun/before/mutually_recursive.grin"
      "dead_fun/after/mutually_recursive.grin"
      deadFunctionEliminationPipeline

    it "replace_node" $ pipeline
      "dead_fun/before/replace_node.grin"
      "dead_fun/after/replace_node.grin"
      deadFunctionEliminationPipeline

    it "replace_simple_type" $ pipeline
      "dead_fun/before/replace_simple_type.grin"
      "dead_fun/after/replace_simple_type.grin"
      deadFunctionEliminationPipeline

    it "simple" $ pipeline
      "dead_fun/before/simple.grin"
      "dead_fun/after/simple.grin"
      deadFunctionEliminationPipeline

    it "true_side_effect_min" $ pipeline
      "dead_fun/before/true_side_effect_min.grin"
      "dead_fun/after/true_side_effect_min.grin"
      deadFunctionEliminationPipeline
