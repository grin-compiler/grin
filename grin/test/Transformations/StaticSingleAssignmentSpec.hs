{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.StaticSingleAssignmentSpec where

import Transformations.StaticSingleAssignment

import Test.Test
import Grin.TH
import Test.Hspec
import Test.Assertions


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "works" $ do
    let before = [prog|
        fun1 p1 p2 =
          p3 <- do
            p3 <- do
              p3 <- prim_int_add 1 2
              prim_int_add p3 2
            p3 <- do
              p3 <- prim_int_add 1 2
              prim_int_add p3 2
            prim_int_add p3 3
          prim_int_add p3 4
      |]
    let after = [prog|
        fun1 p1 p2 =
          p3 <- do
            p3_1 <- do
              p3_2 <- prim_int_add 1 2
              prim_int_add p3_2 2
            p3_3 <- do
              p3_4 <- prim_int_add 1 2
              prim_int_add p3_4 2
            prim_int_add p3_3 3
          prim_int_add p3 4
      |]
    (staticSingleAssignment before) `sameAs` after

  it "node" $ do
    let before = [prog|
        fun1 a =
          (CInt i) <- pure a
          pure i
        fun2 a =
          (CInt i) <- pure a
          pure i
      |]
    let after = [prog|
        fun1 a =
          (CInt i) <- pure a
          pure i
        fun2 a_1 =
          (CInt i_2) <- pure a_1
          pure i_2
      |]
    (staticSingleAssignment before) `sameAs` after
