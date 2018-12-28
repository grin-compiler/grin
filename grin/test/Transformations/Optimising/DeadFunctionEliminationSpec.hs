module Transformations.Optimising.DeadFunctionEliminationSpec where

import Test.Hspec
import Test.Hspec.PipelineExample
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
          , T DoNotRunAnalysis DeadFunctionElimination
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
