{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.ExtendedSyntax.MangleNamesSpec where

import Transformations.ExtendedSyntax.MangleNames

import Test.Hspec

import Test.ExtendedSyntax.Old.Test
import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.Assertions


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

  it "as-pattern" $ do
    let before = [prog|
      grinMain =
        n0 <- pure 5
        (CInt k) @ v <- pure (CInt n0)
        pure k
      |]
    let after = [prog|
      name.0 =
        name.1 <- pure 5
        (CInt name.2) @ name.3 <- pure (CInt name.1)
        pure name.2
      |]
    (mangleNames before) `sameAs` after

  it "case" $ do
    let before = [prog|
      f x =
        n <- pure 5
        case x of
          (COne a)@_1 -> pure a
          (CTwo b)@_2 -> pure b
          #default@_3 -> pure n
      |]
    let after = [prog|
      name.0 name.1 =
        name.2 <- pure 5
        case name.1 of
          (COne name.3)@name.4 -> pure name.3
          (CTwo name.5)@name.6 -> pure name.5
          #default@name.7 -> pure name.2
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
