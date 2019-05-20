{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.CSESpec where

import Transformations.Optimising.CSE

import Test.Hspec
import Test.Hspec.PipelineExample
import Pipeline.Pipeline hiding (pipeline)
import Grin.TH
import Test.Test hiding (newVar)
import Test.Assertions
import Grin.Syntax (Exp)
import Grin.TypeEnv
import Grin.TypeCheck
import Transformations.EffectMap


runTests :: IO ()
runTests = hspec spec

pipelineSrcWithCtx :: ((TypeEnv, Exp) -> (TypeEnv, Exp)) ->
                      TypeEnv -> Exp -> Exp ->
                      [PipelineStep] -> Pipeline
pipelineSrcWithCtx ctx tyEnv before after =
  pipelineSrc (snd $ ctx (tyEnv, before)) (snd $ ctx (tyEnv, after))

spec :: Spec
spec = do
  let csePipeline = [ T CommonSubExpressionElimination ]

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
      pipelineSrcWithCtx ctx teBefore before after csePipeline

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
      pipelineSrcWithCtx ctx te before after csePipeline

    it "store - fetch - update" $ do
      let before = [expr|
          p1 <- store (CInt 0)
          v1 <- fetch p1
          update p1 v1
          update p1 v1
          update p1 (CInt 0)
          update p1 (CInt 0)
          update p1 (CInt 2)
          update p1 (CInt 2)
          update p1 (CInt 2)
          update p1 (CInt 2)
          update p1 (CInt 1)
          fetch p1
        |]
      let after = [expr|
          p1 <- store (CInt 0)
          v1 <- pure (CInt 0)

          -- TODO: improve CSE to remove these redundant updates
          update p1 v1
          update p1 (CInt 0)
          update p1 (CInt 2)

          update p1 (CInt 1)
          pure (CInt 1)
        |]
      pipelineSrcWithCtx ctx te before after csePipeline

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
      pipelineSrcWithCtx ctx te before after csePipeline

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
      pipelineSrcWithCtx ctx te before after csePipeline

    it "constant" $ do
      let before = [expr|
          v1 <- pure (CInt 0)
          v2 <- pure (CInt 0)
          i1 <- pure 1
          i2 <- pure 1
          (CInt i3) <- pure (CInt i1)
          (CInt i4) <- pure (CInt i3)
          (CInt i5) <- pure (CInt 0)
          (CInt i6) <- pure (CInt 0)
          (CInt i7) <- pure (CInt i6)
          pure v2
        |]
      let after = [expr|
          v1 <- pure (CInt 0)
          v2 <- pure v1
          i1 <- pure 1
          i2 <- pure i1
          (CInt i3) <- pure (CInt i1)
          (CInt i4) <- pure (CInt i3)
          (CInt i5) <- pure v1
          (CInt i6) <- pure v1
          (CInt i7) <- pure (CInt i6)
          pure v2
        |]
      pipelineSrcWithCtx ctx te before after csePipeline

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
      pipelineSrcWithCtx ctx te before after csePipeline

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
              n2 <- pure (CNode a1 b2)
              pure n2
            (CBox a2) ->
              n3 <- pure (CBox a2)
              pure n3
            #default ->
              n4 <- pure n1
              pure n4
        |]
      pipelineSrcWithCtx ctx te before after csePipeline

  it "no copy propagation of def arguments" $ do
    let before = [prog|
          fun a =
            b <- pure a
            c <- pure b
            d <- pure c
            pure d
      |]
    let after = [prog|
          fun a =
            b <- pure a
            c <- pure b
            d <- pure c
            pure d
      |]
    pipelineSrc before after csePipeline

  describe "bugfix" $ do
    it "do not memoize effectful functions" $ do
      let before = [prog|
            primop effectful
              _prim_int_print :: T_Int64  -> T_Unit
            funPure a =
              pure (CData)
            funEff b =
              _prim_int_print b
              pure (CData)
            grinMain =
              c <- funPure 1
              d <- funPure 1
              e <- funEff 2
              f <- funEff 2
              pure ()
        |]
      let after = [prog|
            primop effectful
              _prim_int_print :: T_Int64  -> T_Unit
            funPure a =
              pure (CData)
            funEff b =
              _prim_int_print b
              pure (CData)
            grinMain =
              c <- funPure 1
              d <- pure c
              e <- funEff 2
              f <- funEff 2
              pure ()
        |]
      pipelineSrc before after csePipeline

    it "do not remove effectful function from the rhs position of a bind" $ do
      let before = [prog|
          primop effectful
            _prim_int_print :: T_Int64 -> T_Unit
          grinMain =
            _prim_int_print 1
            _prim_int_print 1
        |]
      let after = [prog|
          primop effectful
            _prim_int_print :: T_Int64 -> T_Unit
          grinMain =
            _prim_int_print 1
            _prim_int_print 1
        |]
      pipelineSrc before after csePipeline
