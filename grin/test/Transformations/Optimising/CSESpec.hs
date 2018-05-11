{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.CSESpec where

import Transformations.Optimising.CSE

import Test.Hspec
import GrinTH
import Test hiding (newVar)
import Assertions
import TypeEnv


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  testExprContext $ \ctx -> do
    it "Figure 4.34" $ do
      let teBefore = create $
            (newVar "x'" int64_t)

      let before = [expr|
          y' <- intAdd x' 1
          t' <- pure CInt
          p <- store (t' y')
          fun 1 2
          z' <- intAdd x' 1
          r' <- pure CInt
          q <- store (r' z')
          fun 2 3
          (CInt a') <- fetch p
          (CInt b') <- fetch q
          fun 3 4
        |]
      let after = [expr|
          y' <- intAdd x' 1
          t' <- pure CInt
          p <- store (t' y')
          fun 1 2
          z' <- pure y'
          r' <- pure t'
          q <- store (r' z')
          fun 2 3
          (CInt a') <- pure (t' y')
          (CInt b') <- pure (r' z')
          fun 3 4
        |]
      pending
      commonSubExpressionElimination (ctx (teBefore, before)) `sameAs` (ctx (teBefore, after))

    let te = emptyTypeEnv
    it "store - fetch" $ do
      let before = [expr|
          p1 <- store (CInt 0)
          (CInt a1) <- fetch p1
          pure ()
        |]
      let after = [expr|
          p1 <- store (CInt 0)
          (CInt a1) <- pure (CInt 0)
          pure ()
        |]
      commonSubExpressionElimination (ctx (te, before)) `sameAs` (ctx (te, after))

    it "store - fetch - update" $ do
      let before = [expr|
          p1 <- store (CInt 0)
          v1 <- fetch p1
          update p1 v1

          update p1 (CInt 1)
          fetch p1
        |]
      let after = [expr|
          p1 <- store (CInt 0)
          v1 <- pure (CInt 0)

          update p1 (CInt 1)
          pure (CInt 1)
        |]
      commonSubExpressionElimination (ctx (te, before)) `sameAs` (ctx (te, after))

    it "store - update" $ do
      let before = [expr|
          p1 <- store (CInt 0)
          update p1 (CInt 0)

          v1 <- pure (CInt 0)
          p2 <- store v1
          update p2 v1
        |]
      let after = [expr|
          p1 <- store (CInt 0)

          v1 <- pure (CInt 0)
          p2 <- store v1
          pure ()
        |]
      commonSubExpressionElimination (ctx (te, before)) `sameAs` (ctx (te, after))

    let te = emptyTypeEnv
    it "fetch - update" $ do
      let before = [expr|
          v <- fetch p
          update p v
          pure 1
        |]
      let after = [expr|
          v <- fetch p
          pure 1
        |]
      commonSubExpressionElimination (ctx (te, before)) `sameAs` (ctx (te, after))

    it "constant" $ do
      let before = [expr|
          v1 <- pure (CInt 0)
          v2 <- pure (CInt 0)
          i1 <- pure 1
          i2 <- pure 1
          pure v2
        |]
      let after = [expr|
          v1 <- pure (CInt 0)
          v2 <- pure v1
          i1 <- pure 1
          i2 <- pure i1
          pure v2
        |]
      commonSubExpressionElimination (ctx (te, before)) `sameAs` (ctx (te, after))

    it "application" $ do
      let te = create $ mconcat
            [ newFun "_prim_int_add" int64_t [int64_t, int64_t]
            , newVar "a1" int64_t
            , newVar "v1" int64_t
            , newVar "v2" int64_t
            , newVar "v3" int64_t
            , newVar "v4" int64_t
            ]

      let before = [expr|
          a1 <- pure 1
          v2 <- _prim_int_add a1 a1
          v3 <- _prim_int_add a1 a1
          v4 <- _prim_int_add v2 v3
          pure v4
        |]
      let after = [expr|
          a1 <- pure 1
          v2 <- _prim_int_add a1 a1
          v3 <- pure v2
          v4 <- _prim_int_add v2 v3
          pure v4
        |]
      commonSubExpressionElimination (ctx (te, before)) `sameAs` (ctx (te, after))

    it "case alternative tracking" $ do
      let before = [expr|
          case n1 of
            (CNode a1 b2) ->
              n2 <- pure (CNode a1 b2)
              pure n2
            (CBox a2) ->
              n3 <- pure (CBox a2)
              pure n3
            #default ->
              n4 <- pure n1
              pure n4
        |]
      let after = [expr|
          case n1 of
            (CNode a1 b2) ->
              n2 <- pure n1
              pure n2
            (CBox a2) ->
              n3 <- pure n1
              pure n3
            #default ->
              n4 <- pure n1
              pure n4
        |]
      commonSubExpressionElimination (ctx (te, before)) `sameAs` (ctx (te, after))
