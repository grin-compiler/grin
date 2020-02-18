{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Samples.ExtendedSyntax.ArityFullRemoveSpec where

import Pipeline.ExtendedSyntax.Pipeline

import Test.Hspec
import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.Assertions

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "multi indirection - full remove" $ do
    let before = [prog|
        grinMain =
          k0 <- pure 0
          k1 <- pure 1
          k2 <- pure 10000
          v0 <- pure (CInt k1)
          v1 <- pure (CInt k2)
          p2 <- store v0
          p3 <- store v1
          v2 <- pure (Fupto p2 p3)
          p4 <- store v2
          n13' <- sum k0 p4
          _prim_int_print n13'

        sum p101 p11 =
          (Fupto p17 p18) @ _1 <- fetch p11
          (CInt n2') @ _2 <- fetch p17
          (CInt n3') @ _3 <- fetch p18
          b1' <- _prim_int_gt n2' n3'
          case b1' of
            #True @ alt1 ->
              pure p101
            #False @ alt2 ->
              k3 <- pure 1
              n4' <- _prim_int_add n2' k3
              v3 <- pure (CInt n4')
              p8 <- store v3
              v4 <- pure (Fupto p8 p18)
              p9 <- store v4
              n7'_2 <- _prim_int_add p101 n2'
              sum n7'_2 p9
      |]
    let after = [prog|
          grinMain =
            k0 <- pure 0
            k1 <- pure 1
            k2 <- pure 10000
            n13' <- sum $ k0 k1 k2
            _prim_int_print $ n13'

          sum p101 p11.1.arity.1.6.arity.1 p11.1.arity.2.6.arity.1 =
            b1' <- _prim_int_gt $ p11.1.arity.1.6.arity.1 p11.1.arity.2.6.arity.1
            case b1' of
              #True @ alt1 ->
                pure p101
              #False @ alt2 ->
                k3 <- pure 1
                n4' <- _prim_int_add $ p11.1.arity.1.6.arity.1 k3
                n7'_2 <- _prim_int_add $ p101 p11.1.arity.1.6.arity.1
                sum $ n7'_2 n4' p11.1.arity.2.6.arity.1
      |]
    let steps =
          [ T InlineEval
          , T ArityRaising
          , T BindNormalisation
          , T CommonSubExpressionElimination
          , T CopyPropagation
          , T DeadVariableElimination
          , T ArityRaising
          , T BindNormalisation
          , T CommonSubExpressionElimination
          , T CopyPropagation
          -- , T ConstantFolding
          , T DeadVariableElimination
          ]
    transformed <- pipeline defaultOpts Nothing before steps
    transformed `sameAs` after
