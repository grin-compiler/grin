{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.ConstantPropagationSpec where

import Transformations.ExtendedSyntax.Optimising.ConstantPropagation

import Test.Hspec

import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.Assertions


runTests :: IO ()
runTests = hspec spec


spec :: Spec
spec = do
  it "ignores binds" $ do
    let before = [expr|
        i1 <- pure 1
        i2 <- pure i1
        n1 <- pure (CNode i2)
        n2 <- pure n1
        (CNode i3) @ n3 <- pure n1
        pure 2
      |]
    let after = [expr|
        i1 <- pure 1
        i2 <- pure i1
        n1 <- pure (CNode i2)
        n2 <- pure n1
        (CNode i3) @ n3 <- pure n1
        pure 2
      |]
    constantPropagation before `sameAs` after

  it "is not interprocedural" $ do
    let before = [prog|
        grinMain =
          x <- f
          case x of
            (COne) @ alt1 -> pure 0
            (CTwo) @ alt2 -> pure 1

        f = pure (COne)
      |]
    let after = [prog|
        grinMain =
          x <- f
          case x of
            (COne) @ alt1 -> pure 0
            (CTwo) @ alt2 -> pure 1

        f = pure (COne)
      |]
    constantPropagation before `sameAs` after

  it "does not propagate info outwards of case expressions" $ do
    let before = [prog|
        grinMain =
          x <- pure 0
          y <- case x of
            0 @ alt1 -> pure (COne)
          case y of
            (COne) @ alt2 -> pure 0
            (CTwo) @ alt3 -> pure 1
      |]
    let after = [prog|
        grinMain =
          x <- pure 0
          y <- case x of
            0 @ alt1 -> pure (COne)
          case y of
            (COne) @ alt2 -> pure 0
            (CTwo) @ alt3 -> pure 1
      |]
    constantPropagation before `sameAs` after

  it "base case" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        case n1 of
          (CNil)     @ alt1 -> pure 1
          (CNode a1) @ alt2 -> pure 2
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        do
          (CNode a1) @ alt2 <- pure (CNode i1)
          pure 2
      |]
    constantPropagation before `sameAs` after

  it "ignores illformed case - multi matching" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        _1 <- case n1 of
          (CNil)     @ alt1 -> pure 1
          (CNode a1) @ alt2 -> pure 2
          (CNode b1) @ alt3 -> pure 3
        case n1 of
          (CNil)   @ alt4 -> pure 4
          #default @ alt5 -> pure 5
          #default @ alt6 -> pure 6
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        _1 <- case n1 of
          (CNil)     @ alt1 -> pure 1
          (CNode a1) @ alt2 -> pure 2
          (CNode b1) @ alt3 -> pure 3
        case n1 of
          (CNil)   @ alt4 -> pure 4
          #default @ alt5 -> pure 5
          #default @ alt6 -> pure 6
      |]
    constantPropagation before `sameAs` after

  it "default pattern" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        case n1 of
          (CNil)   @ alt1 -> pure 2
          #default @ alt2 -> pure 3
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        do
          alt2 <- pure n1
          pure 3
      |]
    constantPropagation before `sameAs` after

  it "unknown scrutinee - simple" $ do
    let before = [expr|
        case n1 of
          (CNil)   @ alt1 -> pure 2
          #default @ alt2 -> pure 3
      |]
    let after = [expr|
        case n1 of
          (CNil)   @ alt1 -> pure 2
          #default @ alt2 -> pure 3
      |]
    constantPropagation before `sameAs` after

  it "unknown scrutinee becomes known in alternatives - specific pattern" $ do
    let before = [expr|
        case n1 of
          (CNil) @ alt11 ->
            case n1 of
              (CNil)     @ alt21 -> pure 1
              (CNode a1) @ alt22 -> pure 2
          (CNode a2) @ alt12 ->
            case n1 of
              (CNil)     @ alt23 -> pure 3
              (CNode a3) @ alt24 -> pure 4
      |]
    let after = [expr|
        case n1 of
          (CNil) @ alt11 ->
            do
              (CNil) @ alt21 <- pure (CNil)
              pure 1
          (CNode a2) @ alt12 ->
            do
              (CNode a3) @ alt24 <- pure (CNode a2)
              pure 4
      |]
    constantPropagation before `sameAs` after

  it "unknown scrutinee becomes known in alternatives - default pattern" $ do
    let before = [expr|
        case n1 of
          #default @ alt11 ->
            case n1 of
              #default   @ alt21 -> pure 1
              (CNode a1) @ alt22 -> pure 2
          (CNode a2) @ alt12 ->
            case n1 of
              #default   @ alt23 -> pure 3
              (CNode a3) @ alt24 -> pure 4
      |]
    let after = [expr|
        case n1 of
          #default @ alt11 ->
            do
              alt21 <- pure n1
              pure 1
          (CNode a2) @ alt12 ->
            do
              (CNode a3) @ alt24 <- pure (CNode a2)
              pure 4
      |]
    constantPropagation before `sameAs` after

  it "literal - specific pattern" $ do
    let before = [expr|
        i1 <- pure 1
        case i1 of
          (CNil)     @ alt1 -> pure 1
          (CNode a1) @ alt2 -> pure 2
          1          @ alt3 -> pure 3
          2          @ alt4 -> pure 4
          #default   @ alt5 -> pure 5
      |]
    let after = [expr|
        i1 <- pure 1
        case i1 of
          (CNil)     @ alt1 -> pure 1
          (CNode a1) @ alt2 -> pure 2
          1          @ alt3 -> pure 3
          2          @ alt4 -> pure 4
          #default   @ alt5 -> pure 5
      |]
    constantPropagation before `sameAs` after

  it "literal - default pattern" $ do
    let before = [expr|
        i1 <- pure 3
        case i1 of
          (CNil)     @ alt1 -> pure 1
          (CNode a1) @ alt2 -> pure 2
          1          @ alt3 -> pure 3
          2          @ alt4 -> pure 4
          #default   @ alt5 -> pure 5
      |]
    let after = [expr|
        i1 <- pure 3
        case i1 of
          (CNil)     @ alt1 -> pure 1
          (CNode a1) @ alt2 -> pure 2
          1          @ alt3 -> pure 3
          2          @ alt4 -> pure 4
          #default   @ alt5 -> pure 5
      |]
    constantPropagation before `sameAs` after
