{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module LintSpec where

import Test.Hspec
import Grin.Grin
import Grin.TH
import Grin.Lint
import Grin.TypeEnv
import Grin.TypeCheck (inferTypeEnv)
import qualified Data.Map as Map


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do

  describe "Variable lint" $ do
    it "finds undefined variables" $ do
      let program = [prog|
          undefinedParam p1 p2 =
            _prim_int p3 p2
        |]
      let (_, errors) = lint Nothing program
      let result = concat $ Map.elems errors
      result `shouldBe` ["undefined variable: p3"]

  describe "Function call lint" $ do
    it "finds variable used as a function" $ do
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

    it "finds non-saturated function calls" $ do
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

  describe "Case lint" $ do
    it "finds location used as matched value" $ do
      let program = [prog|
          main =
            l <- store (CInt 3)
            case l of
              3 -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      let result = concat $ Map.elems errors
      result `shouldBe` ["case variable l has a location type"]

    it "finds duplicate default alternatives" $ do
      let program = [prog|
          main =
            case 3 of
              #default -> pure ()
              3 -> pure ()
              #default -> pure ()
        |]
      let (_,errors) = lint Nothing program
      let result = concat $ Map.elems errors
      result `shouldBe` ["case has more than one default alternatives"]
