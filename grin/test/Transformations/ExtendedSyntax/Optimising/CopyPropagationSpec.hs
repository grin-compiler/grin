{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.CopyPropagationSpec where

import Transformations.ExtendedSyntax.Optimising.CopyPropagation

import Test.Hspec

import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.New.Test hiding (newVar)
import Test.ExtendedSyntax.Assertions


runTests :: IO ()
runTests = hspec spec


spec :: Spec
spec = do
  testExprContextE $ \ctx -> do

    it "left unit law" $ do
      let before = [expr|
          a1 <- pure 1
          a2 <- pure a1
          a3 <- pure a2
          pure a3
        |]
      let after = [expr|
          a1 <- pure 1
          pure a1
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "simple value" $ do
      let before = [expr|
          a1 <- pure 1
          a2 <- pure a1
          a3 <- pure a2
          case a2 of
            #default @ alt1 -> pure a3
        |]
      let after = [expr|
          a1 <- pure 1
          case a1 of
            #default @ alt1 -> pure a1
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "does not replace literal values" $ do
      let before = [expr|
          a1 <- pure 1
          a2 <- pure 1
          pure a2
        |]
      let after = [expr|
          a1 <- pure 1
          a2 <- pure 1
          pure a2
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "node value - node pattern" $ do
      let before = [expr|
          a1 <- pure 1
          b1 <- pure 0
          n1 <- pure (CNode a1 b1)
          n2 <- pure n1
          (CNode a2 b2) @ _1 <- pure n2
          b3 <- pure b2
          (CNode a3 b4) @ _2 <- pure (CNode a2 b3)
          pure (CNode a3 b4)
        |]
      let after = [expr|
          a1 <- pure 1
          b1 <- pure 0
          n1 <- pure (CNode a1 b1)
          _2 <- pure (CNode a1 b1)
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
            #default @ alt1 -> pure n2
        |]
      let after = [expr|
          a1 <- pure 1
          b1 <- pure 0
          n1 <- pure (CNode a1 b1)
          n2 <- pure (CNode a1 b1)
          case n2 of
            #default @ alt1 -> pure n2
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "node value - substitution" $ do
      let before = [expr|
          c1 <- pure 0
          n1 <- pure (CInt c1)
          n2 <- pure n1
          v1 <- pure n2
          (CInt a1) @ v2 <- pure v1
          foo a1 v1
        |]
      let after = [expr|
          c1 <- pure 0
          n1 <- pure (CInt c1)
          foo c1 n1
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "node pattern mismatch" $ do
      let before = [expr|
          c1 <- pure 1
          n1 <- pure (CPair c1 c1)
          (CNode v1 v2) @ _1 <- pure n1
          (CPair v3 v4) @ _2 <- pure n1
          pure ()
        |]
      let after = [expr|
          c1 <- pure 1
          n1 <- pure (CPair c1 c1)
          (CNode v1 v2) @ _1 <- pure n1
          pure ()
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)

    it "bugfix - node pattern - var (infinite loop)" $ do
      let before = [expr|
          c1 <- pure 1
          n1 <- pure (CNode c1 c1)
          p1 <- store n1
          v1 <- fetch p1
          (CNode p2 p3) @ _1 <- pure v1
          pure ()
        |]
      let after = [expr|
          c1 <- pure 1
          n1 <- pure (CNode c1 c1)
          p1 <- store n1
          v1 <- fetch p1
          (CNode p2 p3) @ _1 <- pure v1
          pure ()
        |]
      copyPropagation (ctx before) `sameAs` (ctx after)
