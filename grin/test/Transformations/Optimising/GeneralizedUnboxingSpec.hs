{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.GeneralizedUnboxingSpec where

import Transformations.Optimising.GeneralizedUnboxing

import Test.Hspec
import Grin
import GrinTH
import Test hiding (newVar)
import Assertions
import ParseGrin
import TypeEnv
import Data.Monoid
import Control.Arrow
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  it "Figure 4.21 (extended)" $ do
    let teBefore = emptyTypeEnv
          { _function = Map.fromList
              [ ("test", (int64_t, Vector.fromList [int64_t]))
              , ("foo", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo2", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo3", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo4", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("bar", (int64_t, Vector.fromList []))
              ]
          }
    let before = [prog|
        test n = prim_int_add n 1

        foo a1 a2 a3 =
          b1 <- prim_int_add a1 a2
          b2 <- prim_int_add b1 a3
          pure (CInt b2)

        foo2 a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          foo c1 c1 a3

        foo3 a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          -- In this case the vectorisation did not happen.
          c2 <- foo c1 c1 a3
          pure c2

        foo4 a1 =
          v <- pure (CInt a1)
          pure v

        bar =
          n <- test 1
          (CInt y') <- foo a1 a2 a3
          test y'
      |]
    let teAfter = emptyTypeEnv
          { _function = Map.fromList
              [ ("test", (int64_t, Vector.fromList [int64_t]))
              , ("foo'", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo2'", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo3'", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo4'", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("bar", (int64_t, Vector.fromList []))
              ]
          , _variable = Map.fromList
              [ ("c2'", int64_t)
              , ("v'", int64_t)
              , ("y''", int64_t)
              ]
          }
    let after = [prog|
        test n = prim_int_add n 1

        foo' a1 a2 a3 =
          b1 <- prim_int_add a1 a2
          b2 <- prim_int_add b1 a3
          pure b2

        foo2' a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          foo' c1 c1 a3

        foo3' a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          c2 <- do
            c2' <- foo' c1 c1 a3
            pure (CInt c2')
          (CInt c2') <- pure c2
          pure c2'

        foo4' a1 =
          v <- pure (CInt a1)
          (CInt v') <- pure v
          pure v'

        bar =
          n <- test 1
          (CInt y') <- do
            y'' <- foo' a1 a2 a3
            pure (CInt y'')
          test y'
      |]
    generalizedUnboxing (teBefore, before) `sameAs` (teAfter, after)

  it "Step 1 for Figure 4.21" $ do
    let teBefore = emptyTypeEnv
          { _function = Map.fromList
              [ ("test", (int64_t, Vector.fromList [int64_t]))
              , ("foo", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("bar", (int64_t, Vector.fromList []))
              ]
          }
    let before = [prog|
        test n = prim_int_add n 1

        foo a1 a2 a3 =
          b1 <- prim_int_add a1 a2
          b2 <- prim_int_add b1 a3
          pure (CInt b2)

        bar =
          n <- test 1
          (CInt y') <- foo a1 a2 a3
          test y'
      |]
    functionsToUnbox (teBefore, before) `shouldBe` ["foo"]

  it "Tail calls and general unboxing" $ do
    let teBefore = emptyTypeEnv
          { _function = Map.fromList
              [ ("inside1", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("outside3", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ,(Tag C "Nat", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t]))
              , ("outside4", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t]))
              , ("outside2", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t]))
              , ("outside1", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t]))
              ]
          }
    let before = [prog|
        inside1 a1 a2 a3 =
          b1 <- prim_int_add a1 a2
          b2 <- prim_int_add b1 a3
          pure (CInt b2)

        outside4 =
          pure ()
          outside3 1

        outside3 p1 =
          case p1 of
            1 -> inside1 p1 p1 p1
            2 -> outside2 p1

        outside2 p1 =
          pure ()
          outside1 p1

        outside1 p1 =
          y <- prim_int_add p1 1
          x <- pure (CNat y)
          pure x
      |]
    functionsToUnbox (teBefore, before) `shouldBe` ["inside1", "outside2", "outside1"]

  it "Tail call function 1" $ do
    let fun = [def|
        fun x =
          l <- store x
          tail 3
      |]
    tailCalls fun `shouldBe` (Just ["tail"])

  it "Tail call function 2" $ do
    let fun = [def|
        fun x =
          l <- pure x
          case 1 of
            1 ->
              x <- prim_int_add 1 2
              tail1 x
            2 ->
              x <- prim_int_add 2 3
              tail2 x
      |]
    tailCalls fun `shouldBe` (Just ["tail1", "tail2"])

  it "Non-tail call function 2" $ do
    let fun = [def|
        fun x =
          l <- store x
          case 1 of
            1 ->
              x <- prim_int_add 1 2
              y <- tail x
              pure y
            1 ->
              x <- prim_int_add 2 3
              tail x
      |]
    tailCalls fun `shouldBe` Nothing

  it "Non-tail call function 1" $ do
    let fun = [def|
        fun x =
          l <- store x
          y <- tail 3
          pure x
      |]
    tailCalls fun `shouldBe` Nothing
