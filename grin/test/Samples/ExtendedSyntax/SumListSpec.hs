{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Samples.ExtendedSyntax.SumListSpec where

import Pipeline.ExtendedSyntax.Pipeline

import Test.Hspec

import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.PrimOpsPrelude
import Test.ExtendedSyntax.Assertions

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  -- TODO: Reenable before merge
  it "lazy list sum - half pipeline" $ do
    let before = withPrimPrelude [prog|
          grinMain =
            y.0 <- pure 1
            v.0 <- pure (CInt y.0)
            t1  <- store v.0
            y.1 <- pure 10000
            v.1 <- pure (CInt y.1)
            t2  <- store v.1
            v.2 <- pure (Fupto t1 t2)
            t3  <- store v.2
            v.3 <- pure (Fsum t3)
            t4  <- store v.3
            (CInt r') @ p.0 <- eval $ t4
            _prim_int_print $ r'

          upto m n =
            (CInt m') @ p.2 <- eval $ m
            (CInt n') @ p.1 <- eval $ n
            b' <- _prim_int_gt $ m' n'
            case b' of
              #True @ alt.0 ->
                v.4 <- pure (CNil)
                pure v.4
              #False @ alt.1 ->
                x.7 <- pure 1
                m1' <- _prim_int_add $ m' x.7
                v.5 <- pure (CInt m1')
                m1  <- store v.5
                v.6 <- pure (Fupto m1 n)
                p   <- store v.6
                v.7 <- pure (CCons m p)
                pure v.7

          sum l =
            l2 <- eval $ l
            case l2 of
              (CNil) @ alt.2 ->
                y.10 <- pure 0
                v.8  <- pure (CInt y.10)
                pure v.8
              (CCons x xs) @ alt.3 ->
                (CInt x') @ p.4 <- eval $ x
                (CInt s') @ p.3 <- sum $ xs
                ax' <- _prim_int_add $ x' s'
                v.9 <- pure (CInt ax')
                pure v.9

          eval q =
            v <- fetch q
            case v of
              (CInt x'1) @ alt.4 ->
                pure v
              (CNil) @ alt.5 ->
                pure v
              (CCons y ys) @ alt.6 ->
                pure v
              (Fupto a b) @ alt.7 ->
                w <- upto $ a b
                p.5 <- update q w
                pure w
              (Fsum c) @ alt.8 ->
                z <- sum $ c
                p.6 <- update q z
                pure z
      |]
    let after = [prog|
        -- grinMain =
        --   n13' <- sum 0 1 1000
        --   _prim_int_print n13'

        -- sum p10 p111 p112 =
        --   b1' <- _prim_int_gt p111 p112
        --   case b1' of
        --     #True ->
        --       pure p10
        --     #False ->
        --       n4' <- _prim_int_add p111 1
        --       n7'_2 <- _prim_int_add p10 p111
        --       sum n7'_2 n4' p112
      |]
    let steps = map T
          [ BindNormalisation
          , ConstantPropagation
          , BindNormalisation
          , CommonSubExpressionElimination
          , CopyPropagation
          , DeadVariableElimination
          , ArityRaising
          , CopyPropagation
          , DeadVariableElimination
          , ArityRaising
          , CopyPropagation
          , DeadVariableElimination
          ]

    transformed <- pipeline defaultOpts Nothing before steps
    transformed `sameAs` after
