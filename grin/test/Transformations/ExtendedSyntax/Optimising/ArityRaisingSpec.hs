{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.ExtendedSyntax.Optimising.ArityRaisingSpec where

import Transformations.ExtendedSyntax.Optimising.ArityRaising

import Data.Monoid
import Control.Arrow
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import Test.Hspec

import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.TypeEnv
import Grin.ExtendedSyntax.TypeCheck
import Transformations.ExtendedSyntax.Names (ExpChanges(..))


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "split_undefined" $ do
    let tyEnv = inferTypeEnv testProgBefore
    arityRaising 0 tyEnv testProgBefore `sameAs` (testProgAfter, NewNames)

  it "raises what can be raised, does not raise what cannot be reaised" $ do
    let before = [prog|
          grinMain =
            k0 <- pure 0
            k1 <- pure 0
            n0 <- pure (CPair k0 k0)
            n1 <- pure (CInt k1)
            p1 <- store n0
            p2 <- store n1
            foo p1 p2

          foo r q =
            q' <- fetch q
            r' <- fetch r
            _0 <- pure r'
            _1 <- pure q
            foo r q
    |]
    let after = [prog|
          grinMain =
            k0 <- pure 0
            k1 <- pure 0
            n0 <- pure (CPair k0 k0)
            n1 <- pure (CInt k1)
            p1 <- store n0
            p2 <- store n1
            do
              (CPair p1.0.0.arity.1 p1.0.0.arity.2) @ _2 <- fetch p1
              foo $ p1.0.0.arity.1 p1.0.0.arity.2 p2

          foo r.0.arity.1 r.0.arity.2 q =
            q' <- fetch q
            r' <- pure (CPair r.0.arity.1 r.0.arity.2)
            _0 <- pure r'
            _1 <- pure q
            foo $ r.0.arity.1 r.0.arity.2 q
        |]
    let tyEnv = inferTypeEnv before
    arityRaising 0 tyEnv before `sameAs` (after, NewNames)

testProgBefore :: Exp
testProgBefore = [prog|
grinMain =
  k0 <- pure 0
  v.0 <- pure (CInt k0)
  p1 <- store v.0
  k1 <- pure 1
  v.1 <- pure (CInt k1)
  p2 <- store v.1
  k2 <- pure 1000
  v.2 <- pure (CInt k2)
  p3 <- store v.2
  v.3 <- pure (Fupto p2 p3)
  p4 <- store v.3
  v.4 <- pure (Fsum p1 p4)
  p5 <- store v.4
  v.5 <- fetch p5
  (Fsum p15 p16) @ _0 <- pure v.5
  n13' <- sum $ p15 p16
  _prim_int_print $ n13'

sum p10 p11 =
  v.6 <- fetch p11
  (Fupto p17 p18) @ _1 <- pure v.6
  v.7 <- fetch p17
  (CInt n2') @ _2 <- pure v.7
  v.8 <- fetch p18
  (CInt n3') @ _3 <- pure v.8
  b1' <- _prim_int_gt $ n2' n3'
  case b1' of
    #True @ alt1 ->
      v.9 <- pure (CNil)
      case v.9 of
        (CNil) @ alt11 ->
          v.10 <- fetch p10
          (CInt n14') @ _4 <- pure v.10
          pure n14'
        (CCons.0) @ alt12 ->
          ud0 <- pure (#undefined :: T_Dead)
          ud1 <- pure (#undefined :: T_Dead)
          sum $ ud0 ud1
    #False @ alt2 ->
      k3 <- pure 1
      n4' <- _prim_int_add $ n2' k3
      v.14 <- pure (CInt n4')
      p8 <- store v.14
      v.15 <- pure (Fupto p8 p18)
      p9 <- store v.15
      v.16 <- pure (CCons p17 p9)
      case v.16 of
        (CNil) @ alt21 ->
          pure (#undefined :: T_Dead)
        (CCons p12_2 p13_2) @ alt22 ->
          v.18 <- fetch p10
          (CInt n5'_2) @ _5 <- pure v.18
          v.19 <- fetch p12_2
          (CInt n6'_2) @ _6 <- pure v.19
          n7'_2 <- _prim_int_add $ n5'_2 n6'_2
          v.20 <- pure (CInt n7'_2)
          p14_2 <- store v.20
          sum $ p14_2 p13_2
|]

testProgAfter :: Exp
testProgAfter = [prog|
grinMain =
  k0 <- pure 0
  v.0 <- pure (CInt k0)
  p1 <- store v.0
  k1 <- pure 1
  v.1 <- pure (CInt k1)
  p2 <- store v.1
  k2 <- pure 1000
  v.2 <- pure (CInt k2)
  p3 <- store v.2
  v.3 <- pure (Fupto p2 p3)
  p4 <- store v.3
  v.4 <- pure (Fsum p1 p4)
  p5 <- store v.4
  v.5 <- fetch p5
  (Fsum p15 p16) @ _0 <- pure v.5
  n13' <- do
    (CInt p15.0.0.arity.1) @ _7 <- fetch p15
    (Fupto p16.0.0.arity.1 p16.0.0.arity.2) @ _8 <- fetch p16
    sum $ p15.0.0.arity.1 p16.0.0.arity.1 p16.0.0.arity.2
  _prim_int_print $ n13'

sum p10.0.arity.1 p11.0.arity.1 p11.0.arity.2 =
  v.6 <- pure (Fupto p11.0.arity.1 p11.0.arity.2)
  (Fupto p17 p18) @ _1 <- pure v.6
  v.7 <- fetch p17
  (CInt n2') @ _2 <- pure v.7
  v.8 <- fetch p18
  (CInt n3') @ _3 <- pure v.8
  b1' <- _prim_int_gt $ n2' n3'
  case b1' of
    #True @ alt1 ->
      v.9 <- pure (CNil)
      case v.9 of
        (CNil) @ alt11 ->
          v.10 <- pure (CInt p10.0.arity.1)
          (CInt n14') @ _4 <- pure v.10
          pure n14'
        (CCons.0) @ alt12 ->
          ud0 <- pure (#undefined :: T_Dead)
          ud1 <- pure (#undefined :: T_Dead)
          do
            (CInt ud0.0.1.arity.1) @ _9 <- fetch ud0
            (Fupto ud1.0.1.arity.1 ud1.0.1.arity.2) @ _10 <- fetch ud1
            sum $ ud0.0.1.arity.1 ud1.0.1.arity.1 ud1.0.1.arity.2

    #False @ alt2 ->
      k3 <- pure 1
      n4' <- _prim_int_add $ n2' k3
      v.14 <- pure (CInt n4')
      p8 <- store v.14
      v.15 <- pure (Fupto p8 p18)
      p9 <- store v.15
      v.16 <- pure (CCons p17 p9)
      case v.16 of
        (CNil) @ alt21 ->
          pure (#undefined :: T_Dead)
        (CCons p12_2 p13_2) @ alt22 ->
          v.18 <- pure (CInt p10.0.arity.1)
          (CInt n5'_2) @ _5 <- pure v.18
          v.19 <- fetch p12_2
          (CInt n6'_2) @ _6 <- pure v.19
          n7'_2 <- _prim_int_add $ n5'_2 n6'_2
          v.20 <- pure (CInt n7'_2)
          p14_2 <- store v.20
          do
            (CInt p14_2.0.2.arity.1) @ _11 <- fetch p14_2
            (Fupto p13_2.0.2.arity.1 p13_2.0.2.arity.2) @ _12 <- fetch p13_2
            sum $ p14_2.0.2.arity.1 p13_2.0.2.arity.1 p13_2.0.2.arity.2
|]
