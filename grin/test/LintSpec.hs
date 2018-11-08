{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module LintSpec where

import Test.Hspec
import Grin.Grin
import Grin.TH
import Grin.Lint
import Grin.TypeEnv
import qualified Data.Map as Map


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Undefined parameter" $ do
    it "is found" $ do
      let program = [prog|
          undefinedParam p1 p2 =
            _prim_int p3 p2
        |]
      let (_, errors) = lint Nothing program
      let result = concat $ Map.elems errors
      result `shouldBe` ["undefined variable: p3"]

  describe "Variable used as a function" $ do
    it "is found" $ do
      let program = [prog|
          name1 p11 p12 =
            a1 <- name0 p11 p12
            pure a1

          name0 p01 p02 =
            b0 <- p11 p01 p02
            pure b0
        |]
      let (_, errors) = lint Nothing program
      let result = concat $ Map.elems errors
      result `shouldBe` ["non-function in function call: p11"]

  describe "Non-saturated function call" $ do
    it "is found" $ do
      let program = [prog|
          fun1 p1 p2 =
            _prim_int_add p1 p2

          fun2 =
            p3 <- fun1 3
            pure p3
        |]
      let (_, errors) = lint Nothing program
      let result = concat $ Map.elems errors
      result `shouldBe` ["non-saturated function call: fun1"]
