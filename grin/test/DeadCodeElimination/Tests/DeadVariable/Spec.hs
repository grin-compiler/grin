module DeadCodeElimination.Tests.DeadVariable.Spec where

import Test.Hspec
import Test.Hspec.PipelineExample
import Pipeline.Definitions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Dead Variable Elimination" $ do

    let deadVariableEliminationPipeline =
          [ HPT Compile
          , HPT RunPure
          , LVA Compile
          , LVA RunPure
          , Eff CalcEffectMap
          , T DeadVariableElimination
          ]

    it "simple" $ pipeline
      "dead_variable/before/simple.grin"
      "dead_variable/after/simple.grin"
      deadVariableEliminationPipeline

    it "heap" $ pipeline
      "dead_variable/before/heap.grin"
      "dead_variable/after/heap.grin"
      deadVariableEliminationPipeline

    it "update" $ pipeline
      "dead_variable/before/update.grin"
      "dead_variable/after/update.grin"
      deadVariableEliminationPipeline

    it "app_simple" $ pipeline
      "dead_variable/before/app_simple.grin"
      "dead_variable/after/app_simple.grin"
      deadVariableEliminationPipeline

    it "app_side_effect_1" $ pipeline
      "dead_code/app_side_effect_1.grin"
      "dead_variable/after/app_side_effect_1.grin"
      deadVariableEliminationPipeline

    it "app_side_effect_2" $ pipeline
      "dead_variable/before/app_side_effect_2.grin"
      "dead_variable/after/app_side_effect_2.grin"
      deadVariableEliminationPipeline

    it "pattern_match" $ pipeline
      "dead_variable/before/pattern_match.grin"
      "dead_variable/after/pattern_match.grin"
      deadVariableEliminationPipeline

    it "replace_app" $ pipeline
      "dead_variable/before/replace_app.grin"
      "dead_variable/after/replace_app.grin"
      deadVariableEliminationPipeline

    it "replace_case" $ pipeline
      "dead_variable/before/replace_case.grin"
      "dead_variable/after/replace_case.grin"
      deadVariableEliminationPipeline

    it "replace_case_rec" $ pipeline
      "dead_variable/before/replace_case_rec.grin"
      "dead_variable/after/replace_case_rec.grin"
      deadVariableEliminationPipeline

    it "replace_pure" $ pipeline
      "dead_variable/before/replace_pure.grin"
      "dead_variable/after/replace_pure.grin"
      deadVariableEliminationPipeline

    it "replace_store" $ pipeline
      "dead_variable/before/replace_store.grin"
      "dead_variable/after/replace_store.grin"
      deadVariableEliminationPipeline

    it "replace_update" $ pipeline
      "dead_variable/before/replace_update.grin"
      "dead_variable/after/replace_update.grin"
      deadVariableEliminationPipeline

    it "replace_unspec_loc" $ pipeline
      "dead_variable/before/replace_unspec_loc.grin"
      "dead_variable/after/replace_unspec_loc.grin"
      deadVariableEliminationPipeline

    it "true_side_effect_min" $ pipeline
      "dead_variable/before/true_side_effect_min.grin"
      "dead_variable/after/true_side_effect_min.grin"
      deadVariableEliminationPipeline
