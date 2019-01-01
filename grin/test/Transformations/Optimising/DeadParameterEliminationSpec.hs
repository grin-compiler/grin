module Transformations.Optimising.DeadParameterEliminationSpec where

import Test.Hspec
import Test.Hspec.PipelineExample
import Pipeline.Pipeline hiding (pipeline)


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Dead Parameter Elimination" $ do

    let deadParameterEliminationPipeline =
          [ T DeadParameterElimination
          ]

    it "Fnode" $ pipeline
      "dead_code/fnode.grin"
      "dead_param/after/fnode.grin"
      deadParameterEliminationPipeline

    it "Pnode" $ pipeline
      "dead_code/pnode.grin"
      "dead_param/after/pnode.grin"
      deadParameterEliminationPipeline

    it "Pnode opt" $ pipeline
      "dead_code/pnode_opt.grin"
      "dead_param/after/pnode_opt.grin"
      deadParameterEliminationPipeline

    it "Simple" $ pipeline
      "dead_param/before/simple.grin"
      "dead_param/after/simple.grin"
      deadParameterEliminationPipeline

    it "Mutually recursive" $ pipeline
      "dead_param/before/mutually_recursive.grin"
      "dead_param/after/mutually_recursive.grin"
      deadParameterEliminationPipeline
