{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Transformations.Optimising.ArityRaisingSpec where

import Transformations.Optimising.ArityRaising
import Transformations.Names (ExpChanges(..))

import Test.Hspec
import Grin.Grin
import Grin.TH
import Test.Test hiding (newVar)
import Test.Assertions
import Grin.TypeEnv
import Grin.TypeCheck
import Data.Monoid
import Control.Arrow
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "split_undefined" $ do
    let tyEnv = inferTypeEnv testProgBefore
    arityRaising 0 tyEnv testProgBefore `sameAs` (testProgAfter, NewNames)

testProgBefore :: Exp
testProgBefore = [prog|
grinMain =
  v.0 <- pure (CInt 0)
  p1 <- store v.0
  v.1 <- pure (CInt 1)
  p2 <- store v.1
  v.2 <- pure (CInt 1000)
  p3 <- store v.2
  v.3 <- pure (Fupto p2 p3)
  p4 <- store v.3
  v.4 <- pure (Fsum p1 p4)
  p5 <- store v.4
  v.5 <- fetch p5
  (Fsum p15 p16) <- pure v.5
  n13' <- sum $ p15 p16
  _prim_int_print $ n13'

sum p10 p11 =
  v.6 <- fetch p11
  (Fupto p17 p18) <- pure v.6
  v.7 <- fetch p17
  (CInt n2') <- pure v.7
  v.8 <- fetch p18
  (CInt n3') <- pure v.8
  b1' <- _prim_int_gt $ n2' n3'
  case b1' of
    #True ->
      v.9 <- pure (CNil)
      case v.9 of
        (CNil) ->
          v.10 <- fetch p10
          (CInt n14') <- pure v.10
          pure n14'
        (CCons.0) ->
          sum $ (#undefined :: T_Dead) (#undefined :: T_Dead)
    #False ->
      n4' <- _prim_int_add $ n2' 1
      v.14 <- pure (CInt n4')
      p8 <- store v.14
      v.15 <- pure (Fupto p8 p18)
      p9 <- store v.15
      v.16 <- pure (CCons p17 p9)
      case v.16 of
        (CNil) ->
          pure (#undefined :: T_Dead)
        (CCons p12_2 p13_2) ->
          v.18 <- fetch p10
          (CInt n5'_2) <- pure v.18
          v.19 <- fetch p12_2
          (CInt n6'_2) <- pure v.19
          n7'_2 <- _prim_int_add $ n5'_2 n6'_2
          v.20 <- pure (CInt n7'_2)
          p14_2 <- store v.20
          sum $ p14_2 p13_2
|]

testProgAfter :: Exp
testProgAfter = [prog|
grinMain =
  v.0 <- pure (CInt 0)
  p1 <- store v.0
  v.1 <- pure (CInt 1)
  p2 <- store v.1
  v.2 <- pure (CInt 1000)
  p3 <- store v.2
  v.3 <- pure (Fupto p2 p3)
  p4 <- store v.3
  v.4 <- pure (Fsum p1 p4)
  p5 <- store v.4
  v.5 <- fetch p5
  (Fsum p15 p16) <- pure v.5
  n13' <- do
    (CInt p15.0.0.arity.1) <- fetch p15
    (Fupto p16.0.0.arity.1 p16.0.0.arity.2) <- fetch p16
    sum $ p15.0.0.arity.1 p16.0.0.arity.1 p16.0.0.arity.2
  _prim_int_print $ n13'

sum p10.0.arity.1 p11.0.arity.1 p11.0.arity.2 =
  v.6 <- pure (Fupto p11.0.arity.1 p11.0.arity.2)
  (Fupto p17 p18) <- pure v.6
  v.7 <- fetch p17
  (CInt n2') <- pure v.7
  v.8 <- fetch p18
  (CInt n3') <- pure v.8
  b1' <- _prim_int_gt $ n2' n3'
  case b1' of
    #True ->
      v.9 <- pure (CNil)
      case v.9 of
        (CNil) ->
          v.10 <- pure (CInt p10.0.arity.1)
          (CInt n14') <- pure v.10
          pure n14'
        (CCons.0) ->
          sum $ (#undefined :: T_Dead) (#undefined :: T_Dead) (#undefined :: T_Dead)
    #False ->
      n4' <- _prim_int_add $ n2' 1
      v.14 <- pure (CInt n4')
      p8 <- store v.14
      v.15 <- pure (Fupto p8 p18)
      p9 <- store v.15
      v.16 <- pure (CCons p17 p9)
      case v.16 of
        (CNil) ->
          pure (#undefined :: T_Dead)
        (CCons p12_2 p13_2) ->
          v.18 <- pure (CInt p10.0.arity.1)
          (CInt n5'_2) <- pure v.18
          v.19 <- fetch p12_2
          (CInt n6'_2) <- pure v.19
          n7'_2 <- _prim_int_add $ n5'_2 n6'_2
          v.20 <- pure (CInt n7'_2)
          p14_2 <- store v.20
          do
            (CInt p14_2.0.2.arity.1) <- fetch p14_2
            (Fupto p13_2.0.2.arity.1 p13_2.0.2.arity.2) <- fetch p13_2
            sum $ p14_2.0.2.arity.1 p13_2.0.2.arity.1 p13_2.0.2.arity.2
|]
