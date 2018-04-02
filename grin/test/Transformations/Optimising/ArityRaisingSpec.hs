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
        non x =
          y <- prim_int_add x 1
          pure y

        one pi =
          (CInt i) <- fetch pi
          pure i

        two pi a pf =
          (CInt i)   <- fetch pi
          (CFloat f) <- fetch pf
          pure i
      |]
    examineTheParameters (teBefore, before)
    `shouldBe`
    Map.fromList
      [ ("one", [ ("pi",
                    (Map.fromList
                      [(Tag C "Int", Vector.fromList [T_Int64])
                      ]))] )
      ,  ("two", [ ("pi",
                    (Map.fromList
                      [(Tag C "Int", Vector.fromList [T_Int64])
                      ]))
                 , ("pf",
                    (Map.fromList
                      [(Tag C "Float", Vector.fromList [T_Float])
                      ]))
                 ])
      ]
