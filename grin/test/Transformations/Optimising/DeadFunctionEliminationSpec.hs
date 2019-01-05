{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.Optimising.DeadFunctionEliminationSpec where

import Test.Hspec
import Test.Hspec.PipelineExample
import Pipeline.Pipeline hiding (pipeline)
import Grin.TH


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Dead Function Elimination" $ do

    let deadFunctionEliminationPipeline =
          [ T DeadFunctionElimination
          ]

    it "app_side_effect_1" $ do
      let before = [prog|
            grinMain =
              p0 <- store (CInt 0)
              y0 <- f p0
              y1 <- fetch p0
              pure y1

            f p =
              update p (CInt 1)
              pure 0
          |]

      let after = [prog|
            grinMain =
              p0 <- store (CInt 0)
              y0 <- f p0
              y1 <- fetch p0
              pure y1

            f p =
              update p (CInt 1)
              pure 0
          |]
      pipelineSrc before after deadFunctionEliminationPipeline

    it "mutually_recursive" $ do
      let before = [prog|
            grinMain = pure 0
            f x = g x
            g y = f y
          |]

      let after = [prog|
            grinMain = pure 0
          |]
      pipelineSrc before after deadFunctionEliminationPipeline

    it "replace_node" $ do
      let before = [prog|
            grinMain =
              n0 <- f 0
              pure 0

            f x =
              p <- store (CInt 5)
              pure (CNode p)
          |]

      let after = [prog|
            grinMain =
              n0 <- pure (#undefined :: {CNode[#ptr]})
              pure 0
          |]
      pipelineSrc before after deadFunctionEliminationPipeline

    it "replace_simple_type" $ do
      let before = [prog|
            grinMain =
              y0 <- f 0
              pure 0

            f x = pure x
          |]

      let after = [prog|
            grinMain =
              y0 <- pure (#undefined :: T_Int64)
              pure 0
          |]
      pipelineSrc before after deadFunctionEliminationPipeline

    it "simple" $ do
      let before = [prog|
            grinMain = pure 0

            f x = pure x
          |]

      let after = [prog|
            grinMain = pure 0
          |]
      pipelineSrc before after deadFunctionEliminationPipeline

    it "true_side_effect_min" $ do
      let before = [prog|
            grinMain =
              result_main <- Main.main1 $
              pure ()

            Main.main1 =
              _prim_int_print $ 1
          |]

      let after = [prog|
            grinMain =
              result_main <- Main.main1 $
              pure ()

            Main.main1 =
              _prim_int_print $ 1
          |]
      pipelineSrc before after deadFunctionEliminationPipeline
