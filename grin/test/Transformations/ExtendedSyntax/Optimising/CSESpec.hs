{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.CSESpec where

import Transformations.ExtendedSyntax.Optimising.CSE

import Test.Hspec hiding (before)

import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.New.Test hiding (testExprContext)
import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.TypeEnv
import Grin.ExtendedSyntax.TypeCheck
import Transformations.ExtendedSyntax.EffectMap

import Control.Monad (forM_)


testExprContext mkSpec = (\(label, ctx) -> describe (concat ["(", label, ")"]) $ mkSpec ctx) emptyCtx

runTests :: IO ()
runTests = hspec spec

cseOptNoEff exp = commonSubExpressionElimination (inferTypeEnv exp) mempty exp

spec :: Spec
spec = do
    -- TODO: check whether this program can be reduced further by iteratively applying CSE and copy propagation
  describe "Figure 4.34 (with new syntax)" $ do
    it "CSE" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 1
            x0 <- intAdd c0 c0
            n0 <- pure (CInt x0)
            p0 <- store n0

            c1 <- pure 1
            c2 <- pure 2
            _1 <- fun c1 c2

            c3 <- pure 1
            x1 <- intAdd c0 c3
            n1 <- pure (CInt x1)
            p1 <- store n1

            c4 <- pure 2
            c5 <- pure 3
            _2 <- fun c4 c5

            (CInt a')@v0 <- fetch p0
            (CInt b')@v1 <- fetch p1

            c6 <- pure 3
            c7 <- pure 4
            fun c6 c7
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 1
            x0 <- intAdd c0 c0
            n0 <- pure (CInt x0)
            p0 <- store n0

            c1 <- pure c0
            c2 <- pure 2
            _1 <- fun c1 c2

            c3 <- pure c0
            x1 <- intAdd c0 c3
            n1 <- pure (CInt x1)
            p1 <- store n1

            c4 <- pure c2
            c5 <- pure 3
            _2 <- fun c4 c5

            (CInt a')@v0 <- pure n0
            (CInt b')@v1 <- pure n1

            c6 <- pure c5
            c7 <- pure 4
            fun c6 c7
        |]

      cseOptNoEff before `sameAs` after

    it "CSE -> copy propagation -> CSE" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 1
            x0 <- intAdd c0 c0
            n0 <- pure (CInt x0)
            p0 <- store n0

            c2 <- pure 2
            _1 <- fun c0 c2

            x1 <- intAdd c0 c0
            n1 <- pure (CInt x1)
            p1 <- store n1

            c5 <- pure 3
            _2 <- fun c2 c5

            (CInt a')@v0 <- pure n0
            (CInt b')@v1 <- pure n1

            c7 <- pure 4
            fun c5 c7
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 1
            x0 <- intAdd c0 c0
            n0 <- pure (CInt x0)
            p0 <- store n0

            c2 <- pure 2
            _1 <- fun c0 c2

            x1 <- pure x0
            n1 <- pure (CInt x1)
            p1 <- store n1

            c5 <- pure 3
            _2 <- fun c2 c5

            (CInt a')@v0 <- pure n0
            (CInt b')@v1 <- pure n1

            c7 <- pure 4
            fun c5 c7
        |]

      cseOptNoEff before `sameAs` after

    it "CSE -> copy propagation -> CSE -> copy propagation -> CSE" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 1
            x0 <- intAdd c0 c0
            n0 <- pure (CInt x0)
            p0 <- store n0

            c2 <- pure 2
            _1 <- fun c0 c2

            n1 <- pure (CInt x0)
            p1 <- store n1

            c5 <- pure 3
            _2 <- fun c2 c5

            (CInt a')@v0 <- pure n0
            (CInt b')@v1 <- pure n1

            c7 <- pure 4
            fun c5 c7
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 1
            x0 <- intAdd c0 c0
            n0 <- pure (CInt x0)
            p0 <- store n0

            c2 <- pure 2
            _1 <- fun c0 c2

            n1 <- pure n0
            p1 <- store n1

            c5 <- pure 3
            _2 <- fun c2 c5

            (CInt a')@v0 <- pure n0
            (CInt b')@v1 <- pure n1

            c7 <- pure 4
            fun c5 c7
        |]

      cseOptNoEff before `sameAs` after

  -- TODO: check whether this program can be reduced further by iteratively applying CSE, simple DVE and copy propagation
  describe "store - fetch - update" $ do
    it "CSE" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0
            v0 <- fetch p1

            _1 <- update p1 v0
            _2 <- update p1 v0

            c1 <- pure 0
            n1 <- pure (CInt c1)
            _3 <- update p1 n1
            _4 <- update p1 n1

            c2 <- pure 2
            n2 <- pure (CInt c2)
            _5 <- update p1 n2
            _6 <- update p1 n2
            _7 <- update p1 n2
            _8 <- update p1 n2

            c3 <- pure 1
            n3 <- pure (CInt c3)
            _9 <- update p1 n3
            fetch p1
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0
            v0 <- pure n0

            _1 <- update p1 v0
            _2 <- pure ()

            c1 <- pure c0
            n1 <- pure (CInt c1)
            _3 <- update p1 n1
            _4 <- pure ()

            c2 <- pure 2
            n2 <- pure (CInt c2)
            _5 <- update p1 n2
            _6 <- pure ()
            _7 <- pure ()
            _8 <- pure ()

            c3 <- pure 1
            n3 <- pure (CInt c3)
            _9 <- update p1 n3
            pure n3
        |]
      cseOptNoEff before `sameAs` after

    it "CSE -> DVE -> CP -> CSE" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0

            _1 <- update p1 n0

            n1 <- pure (CInt c0)
            _3 <- update p1 n1

            c2 <- pure 2
            n2 <- pure (CInt c2)
            _5 <- update p1 n2

            c3 <- pure 1
            n3 <- pure (CInt c3)
            _9 <- update p1 n3
            pure n3
        |]

      let after = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0

            _1 <- pure ()

            n1 <- pure n0
            _3 <- update p1 n1

            c2 <- pure 2
            n2 <- pure (CInt c2)
            _5 <- update p1 n2

            c3 <- pure 1
            n3 <- pure (CInt c3)
            _9 <- update p1 n3
            pure n3
        |]
      cseOptNoEff before `sameAs` after

    it "CSE -> DVE -> CP -> CSE -> DVE -> CP" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0

            _3 <- update p1 n0

            c2 <- pure 2
            n2 <- pure (CInt c2)
            _5 <- update p1 n2

            c3 <- pure 1
            n3 <- pure (CInt c3)
            _9 <- update p1 n3
            pure n3
        |]

      let after = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0

            _3 <- pure ()

            c2 <- pure 2
            n2 <- pure (CInt c2)
            _5 <- update p1 n2

            c3 <- pure 1
            n3 <- pure (CInt c3)
            _9 <- update p1 n3
            pure n3
        |]
      cseOptNoEff before `sameAs` after

  describe "other heap operation sequences" $ do
    it "store - fetch" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0
            (CInt a1)@v0 <- fetch p1
            pure ()
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            p1 <- store n0
            (CInt a1)@v0 <- pure n0
            pure ()
        |]
      cseOptNoEff before `sameAs` after

    it "store - update" $ do
      let before = [prog|
          grinMain =
            n0 <- pure (COne)
            p1 <- store n0
            _1 <- update p1 n0

            p2 <- store n1
            update p2 n1
        |]
      let after = [prog|
          grinMain =
            n0 <- pure (COne)
            p1 <- store n0
            _1 <- pure ()

            p2 <- store n1
            pure ()
        |]
      cseOptNoEff before `sameAs` after

    it "fetch - update" $ do
      let before = [prog|
          grinMain =
            v <- fetch p
            _1 <- update p v
            pure 1
        |]
      let after = [prog|
          grinMain =
            v <- fetch p
            _1 <- pure ()
            pure 1
        |]
      cseOptNoEff before `sameAs` after

    it "store - fetch - update - fetch" $ do
      let before = [prog|
          grinMain =
            n1 <- pure (COne)
            p1 <- store n1
            (COne)@v1 <- fetch p1

            n2 <- pure (CTwo)
            _1 <- update p1 n2
            v2 <- fetch p1
            pure ()
        |]
      let after = [prog|
          grinMain =
            n1 <- pure (COne)
            p1 <- store n1
            (COne)@v1 <- pure n1

            n2 <- pure (CTwo)
            _1 <- update p1 n2
            v2 <- pure n2
            pure ()
        |]
      cseOptNoEff before `sameAs` after

  describe "simple expressions" $ do
    it "basic values" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 0
            pure 0
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 0
            pure c0
        |]
      cseOptNoEff before `sameAs` after

    it "nodes" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            n1 <- pure (CInt c0)
            pure ()
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 0
            n0 <- pure (CInt c0)
            n1 <- pure n0
            pure ()
        |]
      cseOptNoEff before `sameAs` after

    it "primops" $ do
      let before = [prog|
          grinMain =
            c0 <- pure 1
            k0 <- _prim_int_add c0 c0
            k1 <- _prim_int_add c0 c0
            k2 <- _prim_int_add k0 k1
            pure k2
        |]
      let after = [prog|
          grinMain =
            c0 <- pure 1
            k0 <- _prim_int_add c0 c0
            k1 <- pure k0
            k2 <- _prim_int_add k0 k1
            pure k2
        |]
      cseOptNoEff before `sameAs` after

    -- QUESTION: Should we keep this "dead code rewriting"?
    -- It might be correct, but it is quite unintuitive.
    it "case alternative tracking" $ do
      let before = [prog|
          grinMain =
            n1 <- pure (CFoo)
            case n1 of
              (CNode a1 b2) @ alt0 ->
                n2 <- pure (CNode a1 b2)
                pure n2
              (CBox a2) @ alt1 ->
                n3 <- pure (CBox a2)
                pure (CFoo)
              #default @ alt2 ->
                n4 <- pure n1
                pure n4
        |]
      let after = [prog|
          grinMain =
            n1 <- pure (CFoo)
            case n1 of
              (CNode a1 b2) @ alt0 ->
                n2 <- pure (CNode a1 b2)
                pure n2
              (CBox a2) @ alt1 ->
                n3 <- pure (CBox a2)
                -- NOTE: this rewrite is valid, since this alternative is dead anyways
                pure n1
              #default @ alt2 ->
                n4 <- pure n1
                pure n4
        |]
      cseOptNoEff before `sameAs` after

    -- NOTE: The body of the function doesn't matter as long as it is pure.
    -- If there are calls to update, or fetch, they can only be through eval,
    -- which always returns the same result for the same input pointer (thunk).
    it "function call" $ do
      let before = [prog|
            grinMain =
              n0 <- pure (COne)
              p0 <- store n0
              n1 <- foo p0
              n2 <- foo p0
              n3 <- fetch p0
              pure ()

            foo x =
              pure (CTwo)
        |]
      let after = [prog|
            grinMain =
              n0 <- pure (COne)
              p0 <- store n0
              n1 <- foo p0
              n2 <- pure n1
              n3 <- fetch p0
              pure ()

            foo x =
              pure (CTwo)
        |]
      let tyEnv   = inferTypeEnv before
          effMap  = effectMap (tyEnv, before)
      commonSubExpressionElimination tyEnv effMap before `sameAs` after

  describe "effectful function calls" $ do
    it "do not memoize effectful functions" $ do
      let before = [prog|
            primop effectful
              _prim_int_print :: T_Int64  -> T_Unit
            funEff b =
              _1 <- _prim_int_print b
              pure (CData)
            grinMain =
              c <- pure 0
              d <- funEff c
              e <- funEff c
              pure ()
        |]
      let after = [prog|
            primop effectful
              _prim_int_print :: T_Int64  -> T_Unit
            funEff b =
              _1 <- _prim_int_print b
              pure (CData)
            grinMain =
              c <- pure 0
              d <- funEff c
              e <- funEff c
              pure ()
        |]
      let tyEnv   = inferTypeEnv before
          effMap  = effectMap (tyEnv, before)
      commonSubExpressionElimination tyEnv effMap before `sameAs` after

    it "do not remove effectful function from the rhs position of a bind" $ do
      let before = [prog|
          primop effectful
            _prim_int_print :: T_Int64 -> T_Unit
          grinMain =
            c <- pure 0
            _1 <- _prim_int_print c
            _2 <- _prim_int_print c
            pure ()
        |]
      let after = [prog|
          primop effectful
            _prim_int_print :: T_Int64 -> T_Unit
          grinMain =
            c <- pure 0
            _1 <- _prim_int_print c
            _2 <- _prim_int_print c
            pure ()
        |]
      let tyEnv   = inferTypeEnv before
          effMap  = effectMap (tyEnv, before)
      commonSubExpressionElimination tyEnv effMap before `sameAs` after

