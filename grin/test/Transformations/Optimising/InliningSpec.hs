{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.InliningSpec where

import Transformations.Optimising.Inlining

import qualified Data.Set as Set
import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions
import TypeCheck


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "base case" $ do
    let before = [prog|
        grinMain =
          x <- funA 22
          y <- funA 55
          pure x

        funA i = pure i
      |]
    let after = [prog|
        grinMain =
          x <- do
            i.0 <- pure 22
            pure i.0
          y <- do
            i.1 <- pure 55
            pure i.1
          pure x

        funA i = pure i
      |]
    let inlineSet = Set.fromList ["funA"]
    snd (inlining inlineSet (inferTypeEnv before, before)) `sameAs` after
