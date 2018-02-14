module PipelineSpec where

import Data.Functor.Infix ((<$$>))
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Pipeline
import Test
import Eval
import Pretty


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "Exploratory testing on random program and random pipeline" $ property $
    forAll (PP <$> genProg) $ \(PP original) ->
    forAllShrink genPipeline shrinkPipeline $ \ppln ->
    monadicIO $ do
      transformed      <- run $ pipeline defaultOpts original ppln
      originalValue    <- run $ pure $ evalProgram PureReducer original
      transformedValue <- run $ pure $ evalProgram PureReducer transformed
      run (transformedValue `shouldBe` originalValue)

genPipeline :: Gen [Pipeline]
genPipeline = do
  ((HPT RunHPTPure):) <$> (T <$$> transformations)

shrinkPipeline :: [Pipeline] -> [[Pipeline]]
shrinkPipeline (hpt:rest) = (hpt:) <$> shrinkList (const []) rest

transformations :: Gen [Transformation]
transformations = do
  ts <- shuffle [toEnum 0 .. ]
  listOf1 $ elements ts
