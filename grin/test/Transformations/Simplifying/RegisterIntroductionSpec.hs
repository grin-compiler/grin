{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.Simplifying.RegisterIntroductionSpec where

import Control.Monad
import Test.Hspec
import Transformations.Simplifying.RegisterIntroduction
import Test.QuickCheck.Property

import Grin.Grin
import Grin.TH
import Test.Assertions
import Test.Test hiding (asVal)


spec :: Spec
spec = do
  describe "internals" tests

  testExprContextE $ \ctx -> do
    it "Example from Figure 4.17" $ do
      let before = [expr|
              l1 <- store (CNone)
              p  <- store (CCons a b)
              u' <- foo 1 3
              q  <- store (CInt u')
              x  <- pure (CCons q p)
              l2 <- store (CNone)
              pure 2
            |]
      let after = [expr|
              l1 <- store (CNone)
              t1 <- pure CCons
              p  <- store (t1 a b)
              x' <- pure 1
              y' <- pure 3
              u' <- foo x' y'
              t2 <- pure CInt
              q  <- store (t2 u')
              t3 <- pure CCons
              x  <- pure (t3 q p)
              l2 <- store (CNone)
              pure 2
            |]
      pending
      pure ()
      --registerIntroduction 0 (ctx before) `sameAs` (ctx after)

  forM_ programGenerators $ \(name, gen) -> do
    describe name $ do
      it "transformation has effect" $ do
        pending
    -- NOTE: commented out due type error
    {-
        property $ forAll gen $ \before ->
          let after = registerIntroduction 0 before
          in changed before after True
    -}

runTests :: IO ()
runTests = hspec spec
