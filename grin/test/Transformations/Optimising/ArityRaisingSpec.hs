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
        [ ("one", [ ("pi1",
                      (Map.fromList
                        [(Tag C "Int", Vector.fromList [T_Int64])
                        ]))] )
        ,  ("two", [ ("pi2",
                      (Map.fromList
                        [(Tag C "Int", Vector.fromList [T_Int64])
                        ]))
                   , ("pf2",
                      (Map.fromList
                        [(Tag C "Float", Vector.fromList [T_Float])
                        ]))
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
