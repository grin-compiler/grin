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

lintErrors :: Map.Map Int [Error] -> [String]
lintErrors = map message . concat . Map.elems

spec :: Spec
spec = do

  describe "Variable lint" $ do
    it "finds undefined variables" $ do
      let program = [prog|
          undefinedParam p1 p2 =
            _prim_int p3 p2
        |]
      let (_, errors) = lint Nothing program
      lintErrors errors `shouldBe` ["undefined variable: p3"]

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
      lintErrors errors `shouldBe` ["non-function in function call: p11"]

    it "finds non-saturated function calls" $ do
      let program = [prog|
          fun1 p1 p2 =
            _prim_int_add p1 p2

          fun2 =
            p3 <- fun1 3
            pure p3
        |]
      let (_, errors) = lint Nothing program
      lintErrors errors `shouldBe` ["non-saturated function call: fun1"]

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
      lintErrors errors `shouldBe` ["case variable l has a location type"]

    it "finds overlapping node alternatives" $ do
      let program = [prog|
          main =
            x <- pure (CInt 1)
            case x of
              (CInt a)   -> pure ()
              (CFloat b) -> pure ()
              (CInt c)   -> pure ()
        |]
      let (_,errors) = lint Nothing program
      lintErrors errors `shouldBe` ["case has overlapping node alternatives CInt"]

    it "finds overlapping literal alternatives" $ do
      let program = [prog|
          main =
            x <- pure 1
            case x of
              1 -> pure ()
              2 -> pure ()
              1 -> pure ()
        |]
      let (_,errors) = lint Nothing program
      lintErrors errors `shouldBe` ["case has overlapping literal alternatives 1"]

    it "finds non-covered node alternatives" $ do
      let program = [prog|
          main =
            l <- store (CInt 1)
            update l (CFloat 1)
            update l (CBool #True)
            v <- fetch l
            case v of
              (CInt a) -> pure ()
              (CFloat b) -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["case has non-covered alternative CBool"]

    it "does not report non-covered nodes with default branch" $ do
      let program = [prog|
          main =
            l <- store (CInt 1)
            update l (CFloat 1)
            update l (CBool #True)
            v <- fetch l
            case v of
              (CInt a) -> pure ()
              (CFloat b) -> pure ()
              #default -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` []

    it "finds duplicate default alternatives" $ do
      let program = [prog|
            main =
              case 3 of
                #default -> pure ()
                3 -> pure ()
                #default -> pure ()
          |]
      let (_,errors) = lint Nothing program
      lintErrors errors `shouldBe` ["case has more than one default alternatives"]

  describe "Store lint" $ do
    it "finds primitive value as argument." $ do
      let program = [prog|
          main =
            v <- pure 1
            l <- store v
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["store has given a primitive value: v :: T_Int64"]

  describe "Fetch lint" $ do
    it "finds non-location value as parameter" $ do
      let program = [prog|
          main =
            l <- pure 1
            x <- fetch l
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["the parameter of fetch is non-location: l"]

  describe "Update lint" $ do
    it "finds non-location value as parameter" $ do
      let program = [prog|
          main =
            l <- pure 1
            x <- update l (CInt 1)
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["the parameter of update is non-location: l"]

    it "finds primitive value as argument." $ do
      let program = [prog|
          main =
            l <- store (CInt 1)
            v <- pure 2
            x <- update l v
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["update has given a primitive value: v :: T_Int64"]
