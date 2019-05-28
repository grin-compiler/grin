module PipelineSpec where

import Data.Functor.Infix ((<$$>))
import Data.List ((\\), nub)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Pipeline.Pipeline
import Test.Test
import Pipeline.Eval
import Grin.Pretty
import Debug.Trace


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "Exploratory testing on random program and random pipeline" $ do
    pending
    -- NOTE: commented out due type error
    {-
    property $
      forAll (PP <$> genProg) $ \(PP original) ->
  --    forAllShrink genPipeline shrinkPipeline $ \ppln ->
      forAll genPipeline $ \ppln ->
      monadicIO $ do
        (pipelineInfo, transformed) <- run $ pipeline defaultOpts original ppln
        pre $ any ((==ExpChanged) . snd) pipelineInfo
        traceShowM pipelineInfo
        pre $ transformed /= original
        originalValue    <- run $ pure $ evalProgram PureReducer original
        transformedValue <- run $ pure $ evalProgram PureReducer transformed
        run (transformedValue `shouldBe` originalValue)
    -}
genPipeline :: Gen [PipelineStep]
genPipeline = do
  ([SimplePrintGrin id, HPT Compile, HPT RunPure]++) <$> (T <$$> transformations)
--  ([HPT CompileHPT, HPT RunHPTPure]++) <$> (T <$$> transformations)

shrinkPipeline :: [PipelineStep] -> [[PipelineStep]]
shrinkPipeline (printast:chpt:hpt:rest) = ([printast, chpt, hpt]++) <$> shrinkList (const []) rest

transformations :: Gen [Transformation]
transformations = do
  ts <- shuffle [toEnum 0 .. ]
  fmap nub $ listOf1 $ elements (ts \\ knownIssues)

knownIssues :: [Transformation]
knownIssues =
  [ Vectorisation        -- Needs maintained HTP results
  , RegisterIntroduction -- Memory leak
  ]
