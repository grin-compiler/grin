{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.CopyPropagationSpec where

import Transformations.Optimising.CopyPropagation

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions


runTests :: IO ()
runTests = hspec spec


spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "simple value" $ do
      let before = [expr|
          a1 <- pure 1
          a2 <- pure a1
          a3 <- pure a2
          case a2 of
            #default -> pure a3
        |]
      let after = [expr|
          a1 <- pure 1
          a2 <- pure a1
          a3 <- pure a1
          case a1 of
            #default -> pure a1
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "node value - node pattern" $ do
      let before = [expr|
          a1 <- pure 1
          (CNode a2 b1) <- pure (CNode a1 0)
          b2 <- pure b1
          (CNode a3 0) <- pure (CNode a2 0)
          pure (CNode a3 b2)
        |]
      let after = [expr|
          a1 <- pure 1
          (CNode a2 b1) <- pure (CNode a1 0)
          b2 <- pure b1
          (CNode a3 0) <- pure (CNode a1 0)
          pure (CNode a1 b1)
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "node value - var pattern" $ do
      let before = [expr|
          a1 <- pure 1
          b1 <- pure 0
          n1 <- pure (CNode a1 b1)
          a2 <- pure a1
          n2 <- pure (CNode a2 b1)
          case n2 of
            #default -> pure n2
        |]
      let after = [expr|
          a1 <- pure 1
          b1 <- pure 0
          n1 <- pure (CNode a1 b1)
          a2 <- pure a1
          n2 <- pure (CNode a1 b1)
          case n2 of
            #default -> pure n2
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "right unit law" $ do
      let before = [expr|
          a1 <- pure 1
          b1 <- pure 0
          n1 <- pure (CNode a1 b1)
          (CNode 1 0) <- pure (CNode a1 b1)
          0 <- pure 0
          (CNode 1 0) <- pure (CNode 1 0)
          pure n1
        |]
      let after = [expr|
          a1 <- pure 1
          b1 <- pure 0
          n1 <- pure (CNode a1 b1)
          (CNode 1 0) <- pure (CNode a1 b1)
          pure n1
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)
