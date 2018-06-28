{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.ConstantPropagationSpec where

import Transformations.Optimising.ConstantPropagation

import Test.Hspec
import Grin.TH
import Test.Test hiding (newVar)
import Test.Assertions


runTests :: IO ()
runTests = hspec spec


spec :: Spec
spec = do
  it "ignore binds" $ do
    let before = [expr|
        i1 <- pure 1
        i2 <- pure i1
        n1 <- pure (CNode i2)
        (CNode 0) <- pure n1
        (CNode 1) <- pure n1
        pure 2
      |]
    let after = [expr|
        i1 <- pure 1
        i2 <- pure i1
        n1 <- pure (CNode i2)
        (CNode 0) <- pure n1
        (CNode 1) <- pure n1
        pure 2
      |]
    constantPropagation before `sameAs` after

  it "base case" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        case n1 of
          (CNil)      -> pure 1
          (CNode a1)  -> pure 2
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        do
          (CNode a1) <- pure (CNode i1)
          pure 2
      |]
    constantPropagation before `sameAs` after

  it "ignore illformed case - multi matching" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        case n1 of
          (CNil)      -> pure 1
          (CNode a1)  -> pure 2
          (CNode b1)  -> pure 3
        case n1 of
          (CNil)    -> pure 4
          #default  -> pure 5
          #default  -> pure 6
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        case n1 of
          (CNil)      -> pure 1
          (CNode a1)  -> pure 2
          (CNode b1)  -> pure 3
        case n1 of
          (CNil)    -> pure 4
          #default  -> pure 5
          #default  -> pure 6
      |]
    constantPropagation before `sameAs` after

  it "default pattern" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        case n1 of
          (CNil)    -> pure 2
          #default  -> pure 3
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        do
          pure 3
      |]
    constantPropagation before `sameAs` after

  it "unknown scrutinee - simple" $ do
    let before = [expr|
        case n1 of
          (CNil)    -> pure 2
          #default  -> pure 3
      |]
    let after = [expr|
        case n1 of
          (CNil)    -> pure 2
          #default  -> pure 3
      |]
    constantPropagation before `sameAs` after

  it "unknown scrutinee becomes known in alternatives - specific pattern" $ do
    let before = [expr|
        case n1 of
          (CNil) ->
            case n1 of
              (CNil)      -> pure 1
              (CNode a1)  -> pure 2
          (CNode a2) ->
            case n1 of
              (CNil)      -> pure 3
              (CNode a3)  -> pure 4
      |]
    let after = [expr|
        case n1 of
          (CNil) ->
            do
              (CNil) <- pure (CNil)
              pure 1
          (CNode a2) ->
            do
              (CNode a3) <- pure (CNode a2)
              pure 4
      |]
    constantPropagation before `sameAs` after

  it "unknown scrutinee becomes known in alternatives - default pattern" $ do
    let before = [expr|
        case n1 of
          #default ->
            case n1 of
              #default    -> pure 1
              (CNode a1)  -> pure 2
          (CNode a2) ->
            case n1 of
              #default    -> pure 3
              (CNode a3)  -> pure 4
      |]
    let after = [expr|
        case n1 of
          #default ->
            do
              pure 1
          (CNode a2) ->
            do
              (CNode a3) <- pure (CNode a2)
              pure 4
      |]
    constantPropagation before `sameAs` after

  it "literal - specific pattern" $ do
    let before = [expr|
        i1 <- pure 1
        case i1 of
          (CNil)      -> pure 1
          (CNode a1)  -> pure 2
          1           -> pure 3
          2           -> pure 4
          #default    -> pure 5
      |]
    let after = [expr|
        i1 <- pure 1
        case i1 of
          (CNil)      -> pure 1
          (CNode a1)  -> pure 2
          1           -> pure 3
          2           -> pure 4
          #default    -> pure 5
      |]
    constantPropagation before `sameAs` after

  it "literal - default pattern" $ do
    let before = [expr|
        i1 <- pure 3
        case i1 of
          (CNil)      -> pure 1
          (CNode a1)  -> pure 2
          1           -> pure 3
          2           -> pure 4
          #default    -> pure 5
      |]
    let after = [expr|
        i1 <- pure 3
        case i1 of
          (CNil)      -> pure 1
          (CNode a1)  -> pure 2
          1           -> pure 3
          2           -> pure 4
          #default    -> pure 5
      |]
    constantPropagation before `sameAs` after
