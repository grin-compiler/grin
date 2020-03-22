{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.ExtendedSyntax.StaticSingleAssignmentSpec where

import Transformations.ExtendedSyntax.StaticSingleAssignment

import Test.Hspec

import Grin.ExtendedSyntax.TH
import Test.ExtendedSyntax.Assertions

{- NOTE: Variables with names like "z<i>" are introduced just for naming.
   They are not relevant to the result of the analysis.

   Variables with names like "_<i>" are introduced just for named bindings.
   They will never be used.
-}

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "works" $ do
    let before = [prog|
        fun1 p1 p2 =
          z0 <- pure 0
          p3 <- do
            p3 <- do
              p3 <- prim_int_add z0 z0
              prim_int_add p3 z0
            p3 <- do
              p3 <- prim_int_add z0 z0
              prim_int_add p3 z0
            prim_int_add p3 z0
          prim_int_add p3 z0
      |]
    let after = [prog|
        fun1 p1 p2 =
          z0 <- pure 0
          p3 <- do
            p3_1 <- do
              p3_2 <- prim_int_add z0 z0
              prim_int_add p3_2 z0
            p3_3 <- do
              p3_4 <- prim_int_add z0 z0
              prim_int_add p3_4 z0
            prim_int_add p3_3 z0
          prim_int_add p3 z0
      |]
    (staticSingleAssignment before) `sameAs` after

  it "node" $ do
    let before = [prog|
        fun1 a =
          (CInt i)@_1 <- pure a
          pure i
        fun2 a =
          (CInt i)@_2 <- pure a
          pure i
      |]
    let after = [prog|
        fun1 a =
          (CInt i)@_1 <- pure a
          pure i
        fun2 a_1 =
          (CInt i_2)@_2 <- pure a_1
          pure i_2
      |]
    (staticSingleAssignment before) `sameAs` after
