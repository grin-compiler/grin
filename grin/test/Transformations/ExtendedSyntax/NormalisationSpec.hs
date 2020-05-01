module Transformations.ExtendedSyntax.NormalisationSpec where

import Test.Hspec
import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.Assertions


test :: IO ()
test = hspec spec

spec :: Spec
spec = describe "Normalisation" $ do
  it "works for an example" $ do
    let before = [prog|
      pureLast1 x = pure x

      pureLast2 x =
        a <- fun1 x
        store a

      pureLast3 x =
        case x of
          (CCons a b) -> pure a

      pureLast4 x =
        y <- pure x
        case y of
          (CCons a b) -> pure a

      pureLast5 x =
        y <- pure (CInt x)
        pure y
      |]

    printGrin $ normalise before
    42 `shouldBe` 42

