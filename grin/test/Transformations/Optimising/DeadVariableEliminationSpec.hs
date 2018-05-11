{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.DeadVariableEliminationSpec where

import Transformations.Optimising.DeadVariableElimination

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions


runTests :: IO ()
runTests = hspec spec


spec :: Spec
spec = do
  it "simple" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        p1 <- store n1
        p2 <- store (CNode p1)
        pure 0
      |]
    let after = [expr|
        pure 0
      |]
    deadVariableElimination before `sameAs` after

  it "pure case" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        p1 <- store n1
        p2 <- store (CNode p1)
        _prim_int_print i1
        i2 <- case n1 of
          1         -> pure 2
          2         -> pure 3
          #default  -> pure 4
        pure 0
      |]
    let after = [expr|
        i1 <- pure 1
        _prim_int_print i1
        pure 0
      |]
    deadVariableElimination before `sameAs` after

  it "effectful case" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        p1 <- store n1
        p2 <- store (CNode p1)
        _prim_int_print i1
        case n1 of
          1         -> pure ()
          2         -> _prim_int_print 3
          #default  -> pure ()
        pure 0
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        _prim_int_print i1
        case n1 of
          1         -> pure ()
          2         -> _prim_int_print 3
          #default  -> pure ()
        pure 0
      |]
    deadVariableElimination before `sameAs` after

  it "nested effectful case" $ do
    let before = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        p1 <- store n1
        n2 <- pure (CNode p1)
        p2 <- store n2
        _prim_int_print i1
        case n1 of
          1 ->
            i2 <- case n2 of
              0         -> pure 1
              #default  -> pure 2
            pure ()
          2 -> _prim_int_print 3
          #default -> pure ()
        pure 0
      |]
    let after = [expr|
        i1 <- pure 1
        n1 <- pure (CNode i1)
        _prim_int_print i1
        case n1 of
          1         -> pure ()
          2         -> _prim_int_print 3
          #default  -> pure ()
        pure 0
      |]
    deadVariableElimination before `sameAs` after

  it "node pattern" $ do
    let before = [expr|
        i1 <- pure 1
        (CNode i2) <- pure (CNode i1)
        (CNode i3) <- pure (CNode i1)
        n1 <- pure (CNode i2)
        (CNode i4) <- pure n1
        pure i1
      |]
    let after = [expr|
        i1 <- pure 1
        pure i1
      |]
    deadVariableElimination before `sameAs` after

  it "pattern match" $ do
    let before = [expr|
        (CNode i3) <- pure n1
        (CNil) <- pure (CNil)
        (CNil) <- pure (CUnit)
        n2 <- pure (CNode i3)
        (CNode i4) <- pure (CNode i3)
        (CNode i5) <- pure n2
        (CBox i6) <- pure n2
        pure 0
      |]
    let after = [expr|
        pure 0
      |]
    deadVariableElimination before `sameAs` after
