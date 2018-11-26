{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.MangleNamesSpec where

import Transformations.MangleNames

import Test.Test
import Grin.TH
import Test.Hspec
import Test.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "simple" $ do
    let before = [prog|
      f x y =
        z <- pure x
        w <- pure y
        u <- pure (CNode x y z w)
        pure u
      |]
    let after = [prog|
      name.0 name.1 name.2 =
        name.3 <- pure name.1
        name.4 <- pure name.2
        name.5 <- pure (CNode name.1 name.2 name.3 name.4)
        pure name.5
      |]
    (mangleNames before) `sameAs` after

  it "case" $ do
    let before = [prog|
      f x =
        n <- pure 5
        case x of
          (COne a) -> pure a
          (CTwo b) -> pure b
          #default -> pure n
      |]
    let after = [prog|
      name.0 name.1 =
        name.2 <- pure 5
        case name.1 of
          (COne name.3) -> pure name.3
          (CTwo name.4) -> pure name.4
          #default -> pure name.2
      |]
    (mangleNames before) `sameAs` after

  it "mutually_recursive" $ do
    let before = [prog|
      f x = g x
      g y = f y
      |]
    let after = [prog|
      name.0 name.1 = name.2 name.1
      name.2 name.3 = name.0 name.3
      |]
    (mangleNames before) `sameAs` after
