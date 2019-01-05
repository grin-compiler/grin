{-# LANGUAGE QuasiQuotes #-}
module Transformations.Optimising.DeadVariableEliminationSpec where

import Test.Hspec
import Test.Hspec.PipelineExample
import Pipeline.Pipeline hiding (pipeline)
import qualified Pipeline.Pipeline as P
import Grin.TH
import Test.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Dead Variable Elimination" $ do

    let deadVariableEliminationPipeline =
          [ T DeadVariableElimination
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

    describe "bugs" $ do
      xit "Not explicitly covered alternatives trigger undefined replacements" $ do
        let before = [prog|
              grinMain =
                v0 <- _prim_int_add 1 1
                v1 <- case v0 of
                  2 ->
                    v2 <- _prim_int_lt 1 3
                    v3 <- case v2 of
                      #False -> pure v0
                      #True -> pure 1
                    case v3 of
                      0 -> pure (CGT)
                      1 -> pure (CLT)
                  1 -> pure (CEQ)
                -- If #default is changed to explicit alternatives the undefineds are not introduced.
                -- Undefineds are introduced for missing alternatives too.
                case v1 of
                  (CEQ) -> _prim_int_print 1
                  #default -> _prim_int_print 2
            |]
        let after = [prog|
              grinMain =
                v0 <- _prim_int_add 1 1
                v1 <- case v0 of
                  2 ->
                    v2 <- _prim_int_lt 1 3
                    v3 <- case v2 of
                      #False -> pure v0
                      #True -> pure 1
                    case v3 of
                      0 -> pure (CGT)
                      1 -> pure (CLT)
                  1 -> pure (CEQ)
                case v1 of
                  (CEQ) -> _prim_int_print 1
                  #default -> _prim_int_print 2
            |]
        result <- P.pipeline defaultOpts Nothing before
          [ T ProducerNameIntroduction
          , T DeadVariableElimination
          , T BindNormalisation
          ]
        result `sameAs` after
