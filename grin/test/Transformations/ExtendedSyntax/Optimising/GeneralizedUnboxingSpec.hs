{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.GeneralizedUnboxingSpec where

import Transformations.ExtendedSyntax.Optimising.GeneralizedUnboxing


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector

import Test.Hspec

import Test.ExtendedSyntax.Assertions
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.TypeEnv
import Transformations.ExtendedSyntax.Names (ExpChanges(..))


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
              , ("foo2B", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo2C", (T_NodeSet
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
              , ("foo5", (T_NodeSet
                  (Map.fromList
                    [(Tag C "Int", Vector.fromList [T_Int64])
                    ])
                  , Vector.fromList [int64_t, int64_t, int64_t]))
              , ("bar", (int64_t, Vector.fromList []))
              ]
          }
    let before = [prog|
        test n =
          k0 <- pure 1
          prim_int_add n k0

        foo a1 a2 a3 =
          b1 <- prim_int_add a1 a2
          b2 <- prim_int_add b1 a3
          pure (CInt b2)

        foo2 a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          foo c1 c1 a3

        foo2B a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          do
            foo c1 c1 a3

        foo2C a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          case c1 of
            #default  @ alt1 -> pure c1
            (CInt x1) @ alt2 -> foo c1 c1 a3

        foo3 a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          -- In this case the vectorisation did not happen.
          c2 <- foo c1 c1 a3
          pure c2

        foo4 a1 =
          v <- pure (CInt a1)
          pure v

        foo5 a1 =
          n0 <- pure (CInt a1)
          p <- store n0
          fetch p

        bar =
          k1 <- pure 1
          n1 <- test k1
          (CInt y') @ _0 <- foo a1 a2 a3
          test y'
      |]
    let teAfter = emptyTypeEnv
          { _function = Map.fromList
              [ ("test", (int64_t, Vector.fromList [int64_t]))
              , ("foo.unboxed", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo2.unboxed", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo2B.unboxed", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo2C.unboxed", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo3.unboxed", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo4.unboxed", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("foo5.unboxed", (int64_t, Vector.fromList [int64_t, int64_t, int64_t]))
              , ("bar", (int64_t, Vector.fromList []))
              ]
          , _variable = Map.fromList
              [ ("unboxed.CInt.0", int64_t)
              , ("unboxed.CInt.1", int64_t)
              , ("unboxed.CInt.2", int64_t)
              , ("unboxed.CInt.3", int64_t)
              , ("unboxed.CInt.4", int64_t)
              , ("unboxed.CInt.5", int64_t)
              ]
          }
    let after = [prog|
        test n =
          k0 <- pure 1
          prim_int_add n k0

        foo.unboxed a1 a2 a3 =
          b1 <- prim_int_add a1 a2
          b2 <- prim_int_add b1 a3
          pure b2

        foo2.unboxed a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          foo.unboxed c1 c1 a3

        foo2B.unboxed a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          do
            foo.unboxed c1 c1 a3

        foo2C.unboxed a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          case c1 of
            #default @ alt1 ->
              do
                (CInt unboxed.CInt.0) @ _1 <- pure c1
                pure unboxed.CInt.0
            (CInt x1) @ alt2 ->
              foo.unboxed c1 c1 a3

        foo3.unboxed a1 a2 a3 =
          c1 <- prim_int_add a1 a2
          c2 <- do
            unboxed.CInt.4 <- foo.unboxed c1 c1 a3
            pure (CInt unboxed.CInt.4)
          do
            (CInt unboxed.CInt.1) @ _2 <- pure c2
            pure unboxed.CInt.1

        foo4.unboxed a1 =
          v <- pure (CInt a1)
          do
            (CInt unboxed.CInt.2) @ _3 <- pure v
            pure unboxed.CInt.2

        foo5.unboxed a1 =
          n0 <- pure (CInt a1)
          p <- store n0
          do
            (CInt unboxed.CInt.3) @ _4 <- fetch p
            pure unboxed.CInt.3

        bar =
          k1 <- pure 1
          n1 <- test k1
          (CInt y') @ _0 <- do
            unboxed.CInt.5 <- foo.unboxed a1 a2 a3
            pure (CInt unboxed.CInt.5)
          test y'
      |]
    generalizedUnboxing teBefore before `sameAs` (after, NewNames)

  it "Return values are in cases" $ do
    let teBefore = emptyTypeEnv
          { _function =
              fun_t "int_eq"
                [ T_NodeSet $ cnode_t "Int" [T_Int64]
                , T_NodeSet $ cnode_t "Int" [T_Int64]
                ]
                (T_NodeSet $ cnode_t "Int" [T_Int64])
          , _variable = Map.fromList
              [ ("eq0", T_NodeSet $ cnode_t "Int" [T_Int64])
              , ("eq1", T_NodeSet $ cnode_t "Int" [T_Int64])
              , ("eq0_1", int64_t)
              , ("eq1_1", int64_t)
              , ("eq2", bool_t)
              ]
          }
    let before = [prog|
        int_eq eq0 eq1 =
          (CInt eq0_1) @ alt1 <- fetch eq0
          (CInt eq1_1) @ alt2 <- fetch eq1
          eq2 <- _prim_int_eq eq0_1 eq1_1
          case eq2 of
            #False @ alt3 ->
              k0 <- pure 0
              pure (CInt k0)
            #True @ alt4 ->
              k1 <- pure 1
              pure (CInt k1)
      |]
    let teAfter = emptyTypeEnv
          { _function =
              fun_t "int_eq.unboxed"
                [ T_NodeSet $ cnode_t "Int" [T_Int64]
                , T_NodeSet $ cnode_t "Int" [T_Int64]
                ]
                int64_t
          , _variable = Map.fromList
              [ ("eq0", T_NodeSet $ cnode_t "Int" [T_Int64])
              , ("eq1", T_NodeSet $ cnode_t "Int" [T_Int64])
              , ("eq0_1", int64_t)
              , ("eq1_1", int64_t)
              , ("eq2", bool_t)
              ]
          }
    let after = [prog|
        int_eq.unboxed eq0 eq1 =
          (CInt eq0_1) @ alt1 <- fetch eq0
          (CInt eq1_1) @ alt2 <- fetch eq1
          eq2 <- _prim_int_eq eq0_1 eq1_1
          case eq2 of
            #False @ alt3 ->
              k0 <- pure 0
              pure k0
            #True @ alt4 ->
              k1 <- pure 1
              pure k1
      |]
    generalizedUnboxing teBefore before `sameAs` (after, NewNames)

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
        test n =
          k0 <- pure 1
          prim_int_add n k0

        foo a1 a2 a3 =
          b1 <- prim_int_add a1 a2
          b2 <- prim_int_add b1 a3
          pure (CInt b2)

        bar =
          k1 <- pure 1
          n <- test k1
          (CInt y') @ _1 <- foo a1 a2 a3
          test y'
      |]
    functionsToUnbox teBefore before `shouldBe` (Set.fromList ["foo"])

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
          k0 <- pure ()
          k1 <- pure 1
          _1 <- pure k0
          outside3 k2

        outside3 p1 =
          case p1 of
            1 @ alt1 -> inside1 p1 p1 p1 -- :: CInt Int
            2 @ alt2 -> outside2 p1      -- :: CNat Int

        outside2 p1 =
          k0 <- pure ()
          k1 <- pure 1
          _2 <- pure k0
          outside1 p1

        outside1 p1 =
          k2 <- pure 1
          y <- prim_int_add p1 k2
          x <- pure (CNat y)
          pure x
      |]
    functionsToUnbox teBefore before `shouldBe` mempty

  it "Tail call function 1" $ do
    let fun = [def|
        fun x =
          l <- store x
          k0 <- pure 3
          tail k0
      |]
    tailCalls fun `shouldBe` (Just ["tail"])

  it "Tail call function 2" $ do
    let fun = [def|
        fun x =
          l <- pure x
          k0 <- pure 1
          case k0 of
            1 @ alt1 ->
              k1 <- pure 1
              x <- prim_int_add k1 k1
              tail1 x
            2 @ alt2 ->
              k2 <- pure 2
              x <- prim_int_add k2 k2
              tail2 x
      |]
    tailCalls fun `shouldBe` (Just ["tail1", "tail2"])

  it "Partially tail call function 2" $ do
    let fun = [def|
        fun x =
          l <- store x
          k0 <- pure 1
          case k0 of
            1 @ alt1 ->
              k1 <- pure 1
              x <- prim_int_add k1 k1
              y <- tail x
              pure y
            2 @ alt2 ->
              k2 <- pure 2
              x <- prim_int_add k2 k2
              tail x
      |]
    tailCalls fun `shouldBe` (Just ["tail"])

  it "Non-tail call function 1" $ do
    let fun = [def|
        fun x =
          l <- store x
          k0 <- pure 3
          y <- tail k0
          pure x
      |]
    tailCalls fun `shouldBe` Nothing
