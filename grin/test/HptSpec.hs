{-# LANGUAGE LambdaCase, QuasiQuotes #-}
module HptSpec where

import Test.Hspec
import Grin.Grin
import Grin.TH
import Grin.Lint
import Grin.TypeEnv
import Grin.TypeCheck
import qualified Data.Map as Map

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "HPT" $ do
    it "calculates nondefined functions" $ do
      let code = [prog|
        grinMain =
          a <- pure 1
          b <- nondeffun a
          pure ()
        |]
      let result = inferTypeEnv code
      let exptected = emptyTypeEnv
            { _variable = Map.fromList
                [ ("a", int64_t)
                , ("b", dead_t)
                ]
            , _function = mconcat
                [ fun_t "grinMain" [] unit_t
                , fun_t "nondeffun" [int64_t] dead_t
                ]
            }
      result `shouldBe` exptected

  describe "case" $ do
    it "default bug01" $ do
      let code = [prog|
        grinMain =
          (CInt x) <- test 1 2
          y <- pure x
          _prim_int_print y

        test a b =
          c <- _prim_int_add a b
          case c of
                0 -> pure (CInt 100)
                1 ->
                  e0 <- pure c
                  pure (CInt e0)
                #default ->
                  e1 <- pure c
                  pure (CInt e1)
        |]
      let result = inferTypeEnv code
          exptected = emptyTypeEnv
            { _variable = Map.fromList
                [ ("a",   int64_t)
                , ("b",   int64_t)
                , ("c",   int64_t)
                , ("e0",  int64_t)
                , ("e1",  int64_t)
                , ("x",   int64_t)
                , ("y",   int64_t)
                ]
            , _function = mconcat
                [ fun_t "_prim_int_add" [int64_t, int64_t] int64_t
                , fun_t "_prim_int_print" [int64_t] unit_t
                , fun_t "grinMain" [] unit_t
                , fun_t "test" [int64_t, int64_t] $ T_NodeSet $ cnode_t "Int" [T_Int64]
                ]
            }
      result `shouldBe` exptected

