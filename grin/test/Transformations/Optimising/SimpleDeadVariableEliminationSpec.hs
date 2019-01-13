{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.SimpleDeadVariableEliminationSpec where

import Transformations.Optimising.SimpleDeadVariableElimination
import Transformations.EffectMap
import Grin.TypeCheck

import Test.Hspec
import Grin.TH
import Grin.PrimOpsPrelude
import Test.Test hiding (newVar)
import Test.Assertions


runTests :: IO ()
runTests = hspec spec


spec :: Spec
spec = do
  describe "bugs" $ do
    it "keep blocks" $ do
      let before = [prog|
        grinMain =
          fun_main.0 <- pure (P1Main.main.closure.0)
          p.1.0 <- pure fun_main.0
          "unboxed.C\"GHC.Prim.Unit#\".0" <- do
            result_Main.main1.0.0.0 <- pure (P1Main.main1.closure.0)
            apply.unboxed2 $ result_Main.main1.0.0.0
          _prim_int_print $ 0

        apply.unboxed2 p.1.X =
          do
            (P1Main.main1.closure.0) <- pure p.1.X
            _prim_int_print $ 12
            store (F"GHC.Tuple.()")
        |]
      let after = [prog|
        grinMain =
          "unboxed.C\"GHC.Prim.Unit#\".0" <- do
            result_Main.main1.0.0.0 <- pure (P1Main.main1.closure.0)
            apply.unboxed2 $ result_Main.main1.0.0.0
          _prim_int_print $ 0

        apply.unboxed2 p.1.X =
          do
            _prim_int_print $ 12
            store (F"GHC.Tuple.()")
        |]
      tyEnv <- inferTypeEnv before
      let effMap  = effectMap (tyEnv, before)
          dveExp  = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after

    it "do not remove effectful case" $ do
      let before = withPrimPrelude [prog|
          sideeff s1 =
            s2 <- _prim_int_add s1 1
            _prim_int_print s2

          grinMain =
            y <- pure (CInt 0)
            x <- case y of
              (CInt x1) -> sideeff x1
                           pure 1 -- pure (CInt 1)
              (CFloat y1) -> y2 <- _prim_int_add 1 2
                             pure 2 -- pure (CInt y2)
            pure ()
        |]
      let after = withPrimPrelude [prog|
          sideeff s1 =
            s2 <- _prim_int_add s1 1
            _prim_int_print s2

          grinMain =
            y <- pure (CInt 0)
            x <- case y of
              (CInt x1) -> sideeff x1
                           pure 1
              (CFloat y1) -> pure 2
            pure ()
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after

    it "do not remove effectful case 2" $ do
      let before = withPrimPrelude [prog|
          grinMain =
            y <- pure (CInt #"str")
            x <- case y of
              (CInt x1) -> _prim_string_print x1
                           pure 1 -- pure (CInt 1)
            pure ()
        |]
      let after = withPrimPrelude [prog|
          grinMain =
            y <- pure (CInt #"str")
            x <- case y of
              (CInt x1) -> _prim_string_print x1
                           pure 1
            pure ()
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after


  describe "Simple dead variable elimination works for" $ do
    it "simple" $ do
      let before = [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            p1 <- store n1
            p2 <- store (CNode p1)
            pure 0
        |]
      let after = [prog|
          grinMain =
            pure 0
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after

    it "pure case" $ do
      let before = [prog|
          grinMain =
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
      let after = [prog|
          grinMain =
            i1 <- pure 1
            _prim_int_print i1
            pure 0
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after

    it "effectful case" $ do
      let before = [prog|
          grinMain =
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
      let after = [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            _prim_int_print i1
            case n1 of
              1         -> pure ()
              2         -> _prim_int_print 3
              #default  -> pure ()
            pure 0
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after

    it "nested effectful case" $ do
      let before = [prog|
          grinMain =
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
      let after = [prog|
          grinMain =
            i1 <- pure 1
            n1 <- pure (CNode i1)
            _prim_int_print i1
            case n1 of
              1         -> pure ()
              2         -> _prim_int_print 3
              #default  -> pure ()
            pure 0
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after

    it "node pattern" $ do
      let before = [prog|
          grinMain =
            i1 <- pure 1
            (CNode i2) <- pure (CNode i1)
            (CNode i3) <- pure (CNode i1)
            n1 <- pure (CNode i2)
            (CNode i4) <- pure n1
            pure i1
        |]
      let after = [prog|
          grinMain =
            i1 <- pure 1
            pure i1
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after

    it "pattern match" $ do
      let before = [prog|
          grinMain =
            n1 <- pure (CNode 0)
            (CNode i3) <- pure n1
            (CNil) <- pure (CNil)
            (CUnit) <- pure (CUnit)
            n2 <- pure (CNode i3)
            (CNode i4) <- pure (CNode i3)
            (CNode i5) <- pure n2
            (CNode i6) <- pure n2
            pure 0
        |]
      let after = [prog|
          grinMain =
            pure 0
        |]
      tyEnv <- inferTypeEnv before
      let effMap = effectMap (tyEnv, before)
          dveExp = simpleDeadVariableElimination tyEnv effMap before
      dveExp `sameAs` after
