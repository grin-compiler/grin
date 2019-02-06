{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module LintSpec where

import Test.Hspec
import Grin.Grin
import Grin.TH
import Grin.Lint
import Grin.TypeEnv
import Grin.TypeCheck (inferTypeEnv)
import qualified Data.Map as Map
import Grin.PrimOpsPrelude


runTests :: IO ()
runTests = hspec spec

lintErrors :: Map.Map Int [Error] -> [String]
lintErrors = map message . concat . Map.elems

spec :: Spec
spec = do

  describe "Variable lint" $ do
    it "finds undefined variables" $ do
      let program = withPrimPrelude [prog|
          undefinedParam p1 p2 =
            _prim_int_print p3 p2
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
      let program = withPrimPrelude [prog|
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
            n <- pure (CInt 3)
            l <- store n
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
            n0 <- pure (CInt 1)
            n1 <- pure (CFloat 1)
            n2 <- pure (CBool #True)
            l <- store n0
            update l n1
            update l n2
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
            n0 <- pure (CInt 1)
            n1 <- pure (CFloat 1)
            n2 <- pure (CBool #True)
            l <- store n0
            update l n1
            update l n2
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
      lintErrors errors `shouldBe` ["the parameter of fetch is a primitive type: l :: T_Int64"]

  describe "Update lint" $ do
    it "finds non-location value as parameter" $ do
      let program = [prog|
          main =
            l <- pure 1
            n <- pure (CInt 1)
            x <- update l n
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["the parameter of update is a primitive type: l :: T_Int64"]

    it "finds primitive value as argument." $ do
      let program = [prog|
          main =
            n <- pure (CInt 1)
            l <- store n
            v <- pure 2
            x <- update l v
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["update has given a primitive value: v :: T_Int64"]

  describe "Bind lint" $ do
    it "find ill-matching patterns" $ do
      let program = [prog|
          main =
            n <- pure (CFloat 2.0)
            (CInt x) <- pure n
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Invalid pattern match for (CInt x). Expected pattern of type: {CInt[T_Dead]}, but got: {CFloat[T_Float]}"]

    it "disregards variable patterns" $ do
      let program = [prog|
          main =
            n0 <- pure (CInt 0)
            n1 <- case n0 of
              (CInt c0) -> pure n0
              (CFloat c1) ->
                a0 <- pure (CFloat 2.0)
                pure a0
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` []

    -- NOTE: Bottom-up typing can only approximate the result of HPT.
    it "can give false positive errors" $ do
      let program = [prog|
          main =
            n0 <- case 0 of
              0 ->
                n1 <- pure (CInt 0)
                pure n1
              1 ->
                n2 <- pure (CFloat 0.0)
                pure n2
            (CInt x) <- case n0 of
              (CInt c0) -> pure n0
              (CFloat c1) ->
                a0 <- pure (CInt 0)
                pure a0
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Invalid pattern match for (CInt x). Expected pattern of type: {CInt[T_Int64]}, but got: {CFloat[T_Float],CInt[T_Int64]}"]

  describe "Producer lint" $ do
    it "finds nodes in single return statment" $ do
      let program = [prog|
          grinMain =
            pure (CInt 5)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (CInt 5)"]

    it "finds nodes in last return statment" $ do
      let program = [prog|
          grinMain =
            n <- pure (CInt 0)
            pure (CInt 5)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (CInt 5)"]

    it "finds nodes in single return statment in case alternative" $ do
      let program = [prog|
          grinMain =
            case 0 of
              0 -> pure (CInt 5)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (CInt 5)"]

    it "finds nodes in last return statment in case alternative" $ do
      let program = [prog|
          grinMain =
            case 0 of
              0 ->
                n <- pure 0
                pure (CInt 5)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (CInt 5)"]

    it "allows nodes as patterns for bindings" $ do
      let program = [prog|
          grinMain =
            n <- pure (CInt 5)
            (CInt 5) <- pure n
            pure 0
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` []

    it "finds expressions with nodes bound to non-variable patterns" $ do
      let program = [prog|
          grinMain =
            (CInt 5) <- pure (CInt 5)
            pure 0
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Syntax error - expected SimpleExp without nodes"]

    it "finds nodes in Stores" $ do
      let program = [prog|
          grinMain =
            p <- store (CInt 5)
            pure 0
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Syntax error - expected SimpleVal"]

    it "finds nodes in updates" $ do
      let program = [prog|
          grinMain =
            n <- pure (CInt 5)
            p <- store n
            update p (CInt 0)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Syntax error - expected SimpleVal"]

    -- this is optional, but makes DDE simpler
    it "finds nodes LPat of a binding with a Fetch left-hand side" $ do
      let program = [prog|
          grinMain =
            n <- pure (CInt 5)
            p <- store n
            (CInt 5) <- fetch p
            pure 0
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint (Just typeEnv) program
      lintErrors errors `shouldBe` ["Syntax error - expected SimpleVal"]