{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.DeadVariableEliminationSpec where

import Transformations.ExtendedSyntax.Optimising.DeadVariableElimination
import Transformations.ExtendedSyntax.EffectMap

import Test.Hspec

import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.PrimOpsPrelude
import Grin.ExtendedSyntax.TypeCheck


runTests :: IO ()
runTests = hspec spec


spec :: Spec
spec = do
  describe "Bugs" $ do
    it "keep blocks" $ do
      let before = withPrimPrelude [prog|
        grinMain =
          fun_main.0 <- pure (P1Main.main.closure.0)
          p.1.0 <- pure fun_main.0
          "unboxed.C\"GHC.Prim.Unit#\".0" <- do
            result_Main.main1.0.0.0 <- pure (P1Main.main1.closure.0)
            apply.unboxed2 $ result_Main.main1.0.0.0
          k0 <- pure 0
          _prim_int_print $ k0

        apply.unboxed2 p.1.X =
          do
            (P1Main.main1.closure.0) @ v0 <- pure p.1.X
            k1 <- pure 12
            _1 <- _prim_int_print $ k1
            n0 <- pure (F"GHC.Tuple.()")
            store n0
        |]
      let after = withPrimPrelude [prog|
        grinMain =
          "unboxed.C\"GHC.Prim.Unit#\".0" <- do
            result_Main.main1.0.0.0 <- pure (P1Main.main1.closure.0)
            apply.unboxed2 $ result_Main.main1.0.0.0
          k0 <- pure 0
          _prim_int_print $ k0

        apply.unboxed2 p.1.X =
          do
            k1 <- pure 12
            _1 <- _prim_int_print $ k1
            n0 <- pure (F"GHC.Tuple.()")
            store n0
        |]
      let tyEnv   = inferTypeEnv before
          effMap  = effectMap (tyEnv, before)
          dveExp  = deadVariableElimination effMap before
      dveExp `sameAs` after

    it "do not remove effectful case" $ do
      let before = withPrimPrelude [prog|
          sideeff s1 =
            s2 <- _prim_int_add s1 s1
            _prim_int_print s2

          grinMain =
            k1 <- pure 0
            n0 <- pure (CInt k1)
            x <- case n0 of
              (CInt x1) @ alt1 ->
                _1 <- sideeff x1
                pure 1 -- pure (CInt 1)
              (CFloat y1) @ alt2 ->
                y2 <- _prim_int_add k1 k1
                pure 2 -- pure (CInt y2)
            pure ()
        |]
      let after = withPrimPrelude [prog|
          sideeff s1 =
            s2 <- _prim_int_add s1 s1
            _prim_int_print s2

          grinMain =
            k1 <- pure 0
            n0 <- pure (CInt k1)
            x <- case n0 of
              (CInt x1) @ alt1 ->
                _1 <- sideeff x1
                pure 1
              (CFloat y1) @ alt2 ->
                pure 2
            pure ()
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after

    it "do not remove effectful case 2" $ do
      let before = withPrimPrelude [prog|
          grinMain =
            k0 <- pure #"str"
            y <- pure (CInt k0)
            x <- case y of
              (CInt x1) @ alt1 ->
                _1 <- _prim_string_print x1
                pure 1 -- pure (CInt 1)
            pure ()
        |]
      let after = withPrimPrelude [prog|
          grinMain =
            k0 <- pure #"str"
            y <- pure (CInt k0)
            x <- case y of
              (CInt x1) @ alt1 ->
                _1 <- _prim_string_print x1
                pure 1
            pure ()
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after


  describe "Simple dead variable elimination works for" $ do
    it "simple" $ do
      let before = [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            p1 <- store n1
            n2 <- pure (CNode p1)
            p2 <- store n2
            pure 0
        |]
      let after = [prog|
          grinMain =
            pure 0
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after

    it "pure case" $ do
      let before = withPrimPrelude [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            p1 <- store n1
            n2 <- pure (CNode p1)
            p2 <- store n2
            _1 <- _prim_int_print i1
            i2 <- case n1 of
              1        @ alt1  -> pure 2
              2        @ alt2  -> pure 3
              #default @ alt3  -> pure 4
            pure 0
        |]
      let after = withPrimPrelude [prog|
          grinMain =
            i1 <- pure 1
            _1 <- _prim_int_print i1
            pure 0
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after

    it "effectful case" $ do
      let before = withPrimPrelude [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            p1 <- store n1
            n2 <- pure (CNode p1)
            p2 <- store n2
            _1 <- _prim_int_print i1
            _2 <- case n1 of
              1        @ alt1 -> pure ()
              2        @ alt2 -> _prim_int_print i1
              #default @ alt3 -> pure ()
            pure 0
        |]
      let after = withPrimPrelude [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            _1 <- _prim_int_print i1
            _2 <- case n1 of
              1        @ alt1 -> pure ()
              2        @ alt2 -> _prim_int_print i1
              #default @ alt3 -> pure ()
            pure 0
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after

    it "nested effectful case" $ do
      let before = withPrimPrelude [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            p1 <- store n1
            n2 <- pure (CNode p1)
            p2 <- store n2
            _1 <- _prim_int_print i1
            _2 <- case n1 of
              1 @ alt1 ->
                i2 <- case n2 of
                  0        @ alt11 -> pure 1
                  #default @ alt12 -> pure 2
                pure ()
              2        @ alt2 -> _prim_int_print i1
              #default @ alt3 -> pure ()
            pure 0
        |]
      let after = withPrimPrelude [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            _1 <- _prim_int_print i1
            _2 <- case n1 of
              1        @ alt1 -> pure ()
              2        @ alt2 -> _prim_int_print i1
              #default @ alt3 -> pure ()
            pure 0
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after

    it "node pattern" $ do
      let before = [prog|
          grinMain =
            i1 <- pure 1
            (CNode i2) @ v1 <- pure (CNode i1)
            (CNode i3) @ v2 <- pure (CNode i1)
            n1 <- pure (CNode i2)
            (CNode i4) @ v3 <- pure n1
            pure i1
        |]
      let after = [prog|
          grinMain =
            i1 <- pure 1
            pure i1
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after

    it "pattern match" $ do
      let before = [prog|
          grinMain =
            i1 <- pure 0
            n1 <- pure (CNode i1)
            (CNode i3) @ v1 <- pure n1
            (CNil)     @ v2 <- pure (CNil)
            (CUnit)    @ v3 <- pure (CUnit)
            n2 <- pure (CNode i3)
            (CNode i4) @ v4 <- pure (CNode i3)
            (CNode i5) @ v5 <- pure n2
            (CNode i6) @ v6 <- pure n2
            pure 0
        |]
      let after = [prog|
          grinMain =
            pure 0
        |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after

  -- QUESTION: Does this belong here, or to DeadVariableEliminationSpec?
  describe "Interprocedural DVE regression tests" $ do
    it "not explicitly covered alternatives trigger undefined replacements" $ do
      let before = withPrimPrelude [prog|
            grinMain =
              one <- pure 1
              two <- pure 2
              v0 <- _prim_int_add one one
              v1 <- case v0 of
                2 @ alt1 ->
                  v2 <- _prim_int_lt one two
                  v3 <- case v2 of
                    #False @ alt11 -> pure v0
                    #True  @ alt12 -> pure 1
                  case v3 of
                    0 @ alt13 -> pure (CGT)
                    1 @ alt14 -> pure (CLT)
                1 @ alt2 -> pure (CEQ)
              -- If #default is changed to explicit alternatives the undefineds are not introduced.
              -- Undefineds are introduced for missing alternatives too.
              case v1 of
                (CEQ)    @ alt3 -> _prim_int_print one
                #default @ alt4 -> _prim_int_print two
          |]
      let after = withPrimPrelude [prog|
            grinMain =
              one <- pure 1
              two <- pure 2
              v0 <- _prim_int_add one one
              v1 <- case v0 of
                2 @ alt1 ->
                  v2 <- _prim_int_lt one two
                  v3 <- case v2 of
                    #False @ alt11 -> pure v0
                    #True  @ alt12 -> pure 1
                  case v3 of
                    0 @ alt13 -> pure (CGT)
                    1 @ alt14 -> pure (CLT)
                1 @ alt2 -> pure (CEQ)
              case v1 of
                (CEQ)    @ alt3 -> _prim_int_print one
                #default @ alt4 -> _prim_int_print two
          |]
      let tyEnv = inferTypeEnv before
          effMap = effectMap (tyEnv, before)
          dveExp = deadVariableElimination effMap before
      dveExp `sameAs` after
