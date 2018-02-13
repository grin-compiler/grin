{-# LANGUAGE TypeApplications, OverloadedStrings, LambdaCase #-}
module Transformations.Simplifying.VectorisationSpec where

import AbstractInterpretation.AbstractRunGrin
import AbstractInterpretation.HPTResult
import Transformations.Simplifying.Vectorisation (vectorisation)

import Data.Monoid
import Data.Map
import Test.Hspec
import Free hiding (V)
import Grin
import Assertions

import qualified Data.Map as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
  it "Example from Figure 4.9" $ do
    let hpt = Computer
                mempty
                (Map.fromList
                  [ ("v", (Set.singleton
                            (N (RTNode (tag "Cons" 2)
                                [ Set.singleton (BAS T_Int64)
                                , Set.singleton (RTLoc 3)
                                ]))))
                  , ("l0", (Set.singleton
                             (V (BAS T_Int64))))
                  , ("l1", (Set.singleton
                             (V (BAS T_Int64))))
                  ])
                mempty

    before <- buildExpM $
      "l0" <=: store @Int 0                     $
      "v"  <=: unit @Val ("q" @: ["p1", "p2"])  $
      "l1" <=: store @Var "v"                   $
      unit @Int 1

    after <- buildExpM $
      "l0" <=: store @Int 0                                       $
      ("v0" #: ["v1", "v2"]) <=: unit @Val ("q" @: ["p1", "p2"])  $
      "l1" <=: store @Val ("v0" #: ["v1", "v2"])                  $
      unit @Int 1

    vectorisation hpt before `sameAs` after

runTests :: IO ()
runTests = hspec spec
