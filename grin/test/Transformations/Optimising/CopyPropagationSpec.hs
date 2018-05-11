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
          n1 <- pure (CNode a1 0)
          n2 <- pure n1
          (CNode a2 b1) <- pure n2
          b2 <- pure b1
          (CNode a3 0) <- pure (CNode a2 0)
          pure (CNode a3 b2)
        |]
      let after = [expr|
          a1 <- pure 1
          n1 <- pure (CNode a1 0)
          n2 <- pure n1
          a2 <- pure a1
          b1 <- pure 0
          b2 <- pure b1
          a3 <- pure a1
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
          pure n1
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "literal pattern" $ do
      let before = [expr|
          a1 <- pure 1
          a2 <- pure a1
          0 <- pure a2
          1 <- pure a2
          pure a2
        |]
      let after = [expr|
          a1 <- pure 1
          a2 <- pure a1
          0 <- pure 1
          pure a1
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "bugfix - node pattern - var (infinite loop)" $ do
      let before = [expr|
          p1 <- store (CNode 1 1)
          v1 <- fetch p1
          (CNode p2 p3) <- pure v1
          pure ()
        |]
      let after = [expr|
          p1 <- store (CNode 1 1)
          v1 <- fetch p1
          (CNode p2 p3) <- pure v1
          pure ()
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "node pattern mismatch" $ do
      let before = [expr|
          n1 <- pure (CPair 1 1)
          (CNode v1 v2) <- pure n1
          (CNode 1 1) <- pure n1
          (CPair v3 v4) <- pure n1
          (CPair 1 1) <- pure n1
          pure ()
        |]
      let after = [expr|
          n1 <- pure (CPair 1 1)
          (CNode v1 v2) <- pure n1
          (CNode 1 1) <- pure n1
          v3 <- pure 1
          v4 <- pure 1
          pure ()
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)
