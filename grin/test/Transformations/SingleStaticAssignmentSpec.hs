{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module Transformations.SingleStaticAssignmentSpec where

import Transformations.SingleStaticAssignment

import Test
import GrinTH
import Test.Hspec
import Assertions


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
    (singleStaticAssignment before) `sameAs` after
