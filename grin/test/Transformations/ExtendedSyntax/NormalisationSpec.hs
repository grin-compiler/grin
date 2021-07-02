{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.ExtendedSyntax.NormalisationSpec where

import Test.Hspec
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Pretty
import Test.ExtendedSyntax.Assertions
import Transformations.ExtendedSyntax.Normalisation


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
              (CCons a b) @ a1 -> pure a

          pureLast4 x =
            y <- pure x
            case y of
              (CCons a b) @ a2 -> pure a

          pureLast5 x =
            y <- pure (CInt x)
            pure y
        |]

    let after = [prog|
          pureLast1 x =
            rapl.0 <- pure x
            pure rapl.0

          pureLast2 x =
            a <- fun1 $ x
            rapl.1 <- store a
            pure rapl.1

          pureLast3 x =
            rapl.2 <- case x of
              (CCons a b) @ a1 ->
                pure a
            pure rapl.2

          pureLast4 x =
            y <- pure x
            rapl.3 <- case y of
              (CCons a b) @ a2 ->
                pure a
            pure rapl.3

          pureLast5 x =
            y <- pure (CInt x)
            pure y
        |]
    (normalise before) `sameAs` after
