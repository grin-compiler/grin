{-# LANGUAGE OverloadedStrings, LambdaCase, QuasiQuotes #-}
module Transformations.Simplifying.SplitFetchSpec where

import Control.Monad
import Test.Hspec
import Test hiding (asVal)
import Test.QuickCheck.Property

import Grin
import GrinTH
import Test
import Assertions
import Transformations.Simplifying.SplitFetch


spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "Example from Figure 4.13" $ do
      let before = [expr|
              l1 <- store (CNone)
              (t a1 a2 a3) <- fetch p
              l2 <- store (CNone)
              pure 5
            |]
      let after = [expr|
              l1 <- store (CNone)
              t  <- fetch p[0]
              a1 <- fetch p[1]
              a2 <- fetch p[2]
              a3 <- fetch p[3]
              l2 <- store (CNone)
              pure 5
            |]
      pending
      splitFetch (ctx before) `sameAs` (ctx after)

    it "Example from Figure 4.14" $ do
      let before = [expr|
              l1 <- store (CNone)
              (CInt x) <- fetch p
              l2 <- store (CNone)
              pure 5
            |]
      let after = [expr|
              l1 <- store (CNone)
              x  <- fetch p[1]
              l2 <- store (CNone)
              pure 5
            |]
      pending
      splitFetch (ctx before) `sameAs` (ctx after)

    it "Example from Figure 4.15" $ do
      -- TODO Include hpt-result with t \elem { CPair }
      let before = [expr|
              l1 <- store (CNone)
              (t a1 a2) <- fetch p
              l2 <- store (CNone)
              pure 5
            |]
      let after = [expr|
              l1 <- store (CNone)
              t  <- pure CPair
              a1 <- fetch p[1]
              a2 <- fetch p[2]
              l2 <- store (CNone)
              pure 5
            |]
      pending
      splitFetch (ctx before) `sameAs` (ctx after)

  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "transformation has effect" $ do
        pending
    -- NOTE: commented out due type error
    {-
        property $ forAll gen $ \before ->
          let after = splitFetch before
          in changed before after True
    -}

runTests :: IO ()
runTests = hspec spec
