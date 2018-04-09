{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.ArityRaisingSpec where

import Transformations.Optimising.ArityRaising

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
  it "Step 1" $ do
    let teBefore = emptyTypeEnv
          { _function = Map.fromList
              [ ("non", (int64_t, Vector.fromList [int64_t]))
              , ("one", (int64_t, Vector.fromList [location_t [0]]))
              , ("two", (int64_t, Vector.fromList [location_t [0], int64_t, location_t [1]]))
              , ("bad", (int64_t, Vector.fromList [bool_t, location_t [0, 1]]))
              ]
          , _location = Vector.fromList
              [ (Map.fromList
                  [(Tag C "Int", Vector.fromList [T_Int64])
                  ])
              , (Map.fromList
                  [(Tag C "Float", Vector.fromList [T_Float])
                  ])
              ]
          }
    let before = [prog|
        non x0 =
          y0 <- prim_int_add x0 1
          pure y0

        one pi1 =
          (CInt i1) <- fetch pi1
          pure i1

        two pi2 a2 pf2 =
          (CInt i1)   <- fetch pi1
          (CFloat f2) <- fetch pf2
          pure i1

        bad i3 p3 =
          case i3 of
            #True -> (CInt x3) <- fetch p3
                     y3 <- prim_int_add x3 1
                     pure y3
            #False -> (CFloat x4) <- fetch p3
                      y4 <- round x4
                      pure x4
      |]
    examineTheParameters (teBefore, before) `shouldBe`
      (Map.fromList
        [ ("one", [ ("pi1", (Tag C "Int", Vector.fromList [T_Int64]))] )
        , ("two", [ ("pi2", (Tag C "Int", Vector.fromList [T_Int64]))
                  , ("pf2", (Tag C "Float", Vector.fromList [T_Float]))
                  ])
        ])

  it "Step 2" $ do
    let teBefore = emptyTypeEnv
          { _function = Map.fromList
              [ ("foo", (location_t [0], Vector.fromList [location_t [1]])) ]
          , _location = Vector.fromList
              [ (Map.fromList
                  [(Tag C "Cons", Vector.fromList [T_Location [0], T_Location [1]])])
              , (Map.fromList
                  [(Tag C "Nil", Vector.fromList [])])
              ]
          }
    let before = [prog|
        foo p =
          x <- prim_int_add 1 2
          r <- store (CCons p q)
          pure r
      |]
    let paramMap = examineTheParameters (teBefore, before)
    examineCallees paramMap (teBefore, before) `shouldBe` mempty

  it "Full transformation" $ do
    let teBefore = emptyTypeEnv
          { _function =
              fun_t "foo"  [int64_t, location_t [0]] int64_t <>
              fun_t "bar" [int64_t] int64_t <>
              fun_t "foo2" [location_t [0], int64_t, location_t [0]] int64_t <>
              fun_t "bar2" [int64_t] int64_t
          , _location = Vector.fromList
              [ cnode_t "Bar" [T_Int64, T_Int64]
              ]
          }
    let before = [prog|
        foo x1 y1 =
          z1 <- prim_int_add x1 1
          (CBar r1 s1) <- fetch y1
          prim_int_add r1 s1

        bar x2 =
          z2 <- prim_int_add x2 1
          y2 <- store (CBar 1 z2)
          foo 1 y2

        foo2 x3 y3 z3 =
          w3 <- prim_int_add y3 1
          (CBar w1 w2) <- fetch x3
          (CBar w3 w4) <- fetch z3
          w5 <- prim_int_add w1 w2
          w6 <- prim_int_add w3 w4
          prim_int_add w5 w6

        bar2 x4 =
          z4 <- prim_int_add x4 1
          y4 <- store (CBar 1 z4)
          w4 <- store (CBar 2 z4)
          foo2 y4 1 w4
      |]
    let teAfter = teBefore
          { _variable = Map.fromList
              [ ("y11", int64_t)
              , ("y12", int64_t)
              , ("y21", int64_t)
              , ("y22", int64_t)
              , ("x31", int64_t)
              , ("x32", int64_t)
              , ("z31", int64_t)
              , ("z32", int64_t)
              , ("y41", int64_t)
              , ("y42", int64_t)
              , ("w41", int64_t)
              , ("w42", int64_t)
              ]
          }
    let after = [prog|
        foo x1 y11 y12 =
          z1 <- prim_int_add x1 1
          (CBar r1 s1) <- pure (CBar y11 y12)
          prim_int_add r1 s1

        bar x2 =
          z2 <- prim_int_add x2 1
          y2 <- store (CBar 1 z2)
          do
            (CBar y21 y22) <- fetch y2
            foo 1 y21 y22

        foo2 x31 x32 y3 z31 z32 =
          w3 <- prim_int_add y3 1
          (CBar w1 w2) <- pure (CBar x31 x32)
          (CBar w3 w4) <- pure (CBar z31 z32)
          w5 <- prim_int_add w1 w2
          w6 <- prim_int_add w3 w4
          prim_int_add w5 w6

        bar2 x4 =
          z4 <- prim_int_add x4 1
          y4 <- store (CBar 1 z4)
          w4 <- store (CBar 2 z4)
          do
            (CBar y41 y42) <- fetch y4
            (CBar w41 w42) <- fetch w4
            foo2 y41 y42 1 w41 w42
      |]
    (teAfter, after) `sameAs` arityRaising (teBefore, before)
