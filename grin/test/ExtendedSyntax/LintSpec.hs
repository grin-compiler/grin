{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module ExtendedSyntax.LintSpec where

import qualified Data.Map as Map

import Test.Hspec

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Lint
import Grin.ExtendedSyntax.TypeEnv
import Grin.ExtendedSyntax.TypeCheck (inferTypeEnv)
import Grin.ExtendedSyntax.PrimOpsPrelude


runTests :: IO ()
runTests = hspec spec

lintErrors :: Map.Map Int [Error] -> [String]
lintErrors = map message . concat . Map.elems

spec :: Spec
spec = do

  describe "Variable lint" $ do
    -- TODO: withPrimPrelude
    it "finds undefined variables" $ do
      let program = withPrimPrelude [prog|
        undefinedParam p1 p2 =
          _prim_int_print p3
        |]
      let (_, errors) = lint allWarnings Nothing program
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
      let (_, errors) = lint allWarnings Nothing program
      lintErrors errors `shouldBe` ["non-function in function call: p11"]

    it "finds non-saturated function calls" $ do
      let program = withPrimPrelude [prog|
          fun1 p1 p2 =
            _prim_int_add p1 p2

          fun2 =
            p3 <- fun1
            pure p3
        |]
      let (_, errors) = lint allWarnings Nothing program
      lintErrors errors `shouldBe` ["non-saturated function call: fun1"]

  describe "Case lint" $ do
    it "finds location used as case scrutinee" $ do
      let program = [prog|
          main =
            n <- pure (CNil)
            l <- store n
            case l of
              #default @ _1 -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["case variable l has non-supported pattern match type: {0}"]

    it "finds string used as case scrutinee" $ do
      let program = [prog|
          main =
            s <- pure #"string"
            case s of
              #"string" @ _1 -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["case variable s has non-supported pattern match type: T_String"]

    it "finds float used as case scrutinee" $ do
      let program = [prog|
          main =
            f <- pure 1.0
            case f of
              1.0 @ _1 -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["case variable f has non-supported pattern match type: T_Float"]

    it "finds overlapping node alternatives" $ do
      let program = [prog|
          main =
            x <- pure (CNil)
            case x of
              (CNil) @ _1     -> pure ()
              (CFloat b) @ _2 -> pure ()
              (CNil) @ _3     -> pure ()
        |]
      let (_,errors) = lint allWarnings Nothing program
      lintErrors errors `shouldBe` ["case has overlapping node alternatives CNil"]

    it "finds overlapping literal alternatives" $ do
      let program = [prog|
          main =
            x <- pure 1
            case x of
              1 @ _1 -> pure ()
              2 @ _2 -> pure ()
              1 @ _3 -> pure ()
        |]
      let (_,errors) = lint allWarnings Nothing program
      lintErrors errors `shouldBe` ["case has overlapping literal alternatives 1"]

    it "finds non-covered node alternatives" $ do
      let program = [prog|
          main =
            n0 <- pure (CZero)
            n1 <- pure (COne)
            n2 <- pure (CTwo)
            l <- store n0
            _1 <- update l n1
            _2 <- update l n2
            v <- fetch l
            case v of
              (CZero) @ _3 -> pure ()
              (COne) @ _4  -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["case has non-covered alternative CTwo"]

    it "does not report non-covered nodes with default branch" $ do
      let program = [prog|
          main =
            n0 <- pure (CZero)
            n1 <- pure (COne)
            n2 <- pure (CTwo)
            l <- store n0
            _1 <- update l n1
            _2 <- update l n2
            v <- fetch l
            case v of
              (CZero) @ _3  -> pure ()
              (COne) @ _4   -> pure ()
              #default @ _5 -> pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` []

    it "finds duplicate default alternatives" $ do
      let program = [prog|
            main =
              n <- pure 3
              case n of
                #default @ _1 -> pure ()
                3 @ _2 -> pure ()
                #default @ _3 -> pure ()
          |]
      let (_,errors) = lint allWarnings Nothing program
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
      let (_,errors) = lint allWarnings (Just typeEnv) program
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
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["the parameter of fetch is a primitive type: l :: T_Int64"]

  describe "Update lint" $ do
    it "finds non-location value as parameter" $ do
      let program = [prog|
          main =
            l <- pure 1
            n <- pure (CNil)
            x <- update l n
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["the parameter of update is a primitive type: l :: T_Int64"]

    it "finds primitive value as argument." $ do
      let program = [prog|
          main =
            n <- pure (CNil)
            l <- store n
            v <- pure 2
            x <- update l v
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_,errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["update has given a primitive value: v :: T_Int64"]

  describe "Bind lint" $ do
    it "find ill-matching patterns" $ do
      let program = [prog|
          main =
            n <- pure (COne)
            (CTwo) @ v <- pure n
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["Invalid pattern match for (CTwo) @ v. Expected pattern of type: {CTwo[]}, but got: {COne[]}"]

    it "disregards variable patterns" $ do
      let program = [prog|
          main =
            n0 <- pure (COne)
            n1 <- case n0 of
              (COne) @ _1 -> pure n0
              (CTwo) @ _2 ->
                a0 <- pure (CTwo)
                pure a0
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` []

    -- NOTE: Bottom-up typing can only approximate the result of HPT.
    it "can give false positive errors" $ do
      let program = [prog|
          main =
            zero <- pure 0
            n0 <- case zero of
              0 @ _1 ->
                n1 <- pure (COne)
                pure n1
              1 @ _2 ->
                n2 <- pure (CTwo)
                pure n2
            -- NOTE: HPT would restrict the scrutinee here, and would find that it can only have type COne.
            -- However, the bottom-up typing approach used in the linter does not recognize this fact.
            (COne) @ v <- case n0 of
              (COne) @ _3 ->
                pure n0
              (CTwo) @ _4 ->
                a0 <- pure (COne)
                pure a0
            pure ()
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["Invalid pattern match for (COne) @ v. Expected pattern of type: {COne[]}, but got: {COne[],CTwo[]}"]

  describe "Producer lint" $ do
    it "finds nodes in single return statment" $ do
      let program = [prog|
          grinMain =
            pure (COne)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (COne)"]

    it "finds nodes in last return statment" $ do
      let program = [prog|
          grinMain =
            n <- pure (COne)
            pure (CTwo)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (CTwo)"]

    it "finds nodes in single return statment in case alternative" $ do
      let program = [prog|
          grinMain =
            zero <- pure 0
            case zero of
              0 @ _1 -> pure (COne)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (COne)"]

    it "finds nodes in last return statment in case alternative" $ do
      let program = [prog|
          grinMain =
            zero <- pure 0
            case zero of
              0 @ _1 ->
                n <- pure 0
                pure (COne)
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["Last return expressions can only return non-node values: pure (COne)"]

    -- QUESTION: Is this needed with the new syntax? Undefined introduction is still an open question.
    -- this is optional, but makes DDE simpler
    xit "finds nodes LPat of a binding with a Fetch left-hand side" $ do
      let program = [prog|
          grinMain =
            n <- pure (COne)
            p <- store n
            v@(COne) <- fetch p
            pure 0
        |]
      let typeEnv = inferTypeEnv program
      let (_, errors) = lint allWarnings (Just typeEnv) program
      lintErrors errors `shouldBe` ["The result of Fetch can only be bound to a variable: v@(COne)"]
