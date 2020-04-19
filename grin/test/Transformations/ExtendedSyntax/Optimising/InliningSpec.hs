{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.InliningSpec where

import Transformations.ExtendedSyntax.Optimising.Inlining

import qualified Data.Set as Set

import Test.Hspec
import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.TypeCheck
import Transformations.ExtendedSyntax.Names (ExpChanges(..))


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "base case" $ do
    let before = [prog|
        grinMain =
          k <- pure 0
          x <- funA k
          y <- funA k
          pure x

        funA i = pure i
      |]
    let after = [prog|
        grinMain =
          k <- pure 0
          x <- do
            i.0 <- pure k
            pure i.0
          y <- do
            i.1 <- pure k
            pure i.1
          pure x

        funA i = pure i
      |]
    let inlineSet = Set.fromList ["funA"]
    inlining inlineSet (inferTypeEnv before) before `sameAs` (after, NewNames)

  it "no-inline grinMain" $ do
    let before = [prog|
        grinMain =
          k <- pure 0
          x <- pure k
          pure x
      |]
    let after = [prog|
        grinMain =
          k <- pure 0
          x <- pure k
          pure x
      |]
    lateInlining (inferTypeEnv before) before `sameAs` (after, NoChange)
