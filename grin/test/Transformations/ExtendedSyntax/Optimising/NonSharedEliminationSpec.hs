{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.NonSharedEliminationSpec where

import Transformations.ExtendedSyntax.Optimising.NonSharedElimination

import qualified Data.Set as Set
import qualified Data.Map as Map

import Test.Hspec
import Test.ExtendedSyntax.Util (loc, mkTySet)
import Test.ExtendedSyntax.Assertions
import Test.ExtendedSyntax.New.Test (testExprContextE)

import Grin.ExtendedSyntax.Syntax
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.Result as HPT
import AbstractInterpretation.ExtendedSyntax.Sharing.Result
import Grin.ExtendedSyntax.TH (expr)


runTests :: IO ()
runTests = hspec spec

nonSharedElimination' :: SharingResult -> Exp -> Exp
nonSharedElimination' shRes = fst . nonSharedElimination shRes

-- NOTE: The type environments are partial, because we only need pointer information.

cPtr :: Tag
cPtr = Tag C "Ptr"

spec :: Spec
spec = do
  testExprContextE $ \ctx -> do
    it "non-shared simple" $ do
      let before = [expr|
          n1 <- pure (COne)
          p1 <- store n1
          v1 <- fetch p1
          n2 <- pure (CTwo)
          _1 <- update p1 n2
          pure ()
        |]
      let after = [expr|
          n1 <- pure (COne)
          p1 <- store n1
          v1 <- fetch p1
          n2 <- pure (CTwo)
          pure ()
        |]

      let hptResult = HPTResult
            { HPT._memory   = mempty
            , HPT._register = Map.fromList [ ("p1",   loc 0)]
            , HPT._function = mempty
            }
          sharedLocs = mempty
          shResult = SharingResult hptResult sharedLocs

      nonSharedElimination' shResult (ctx before) `sameAs` (ctx after)

    it "shared simple" $ do
      let before = [expr|
          n1 <- pure (COne)
          p1 <- store n1
          v1 <- fetch p1
          n2 <- pure (CTwo)
          _1 <- update p1 n2
          v2 <- fetch p1
          pure ()
        |]
      let after = [expr|
          n1 <- pure (COne)
          p1 <- store n1
          v1 <- fetch p1
          n2 <- pure (CTwo)
          _1 <- update p1 n2
          v2 <- fetch p1
          pure ()
        |]

      let hptResult = HPTResult
            { HPT._memory   = mempty
            , HPT._register = Map.fromList [ ("p1",   loc 0)]
            , HPT._function = mempty
            }
          sharedLocs = Set.fromList [0]
          shResult = SharingResult hptResult sharedLocs

      nonSharedElimination' shResult (ctx before) `sameAs` (ctx after)

    it "shared node" $ do
      let before = [expr|
          n1 <- pure (COne)
          n2 <- pure (CTwo)
          p1 <- store n1
          n3 <- pure (CPtr p1)
          (CPtr p2) @ _1 <- pure n3
          (CPtr p3) @ _2 <- pure n3
          v1 <- fetch p2
          _3 <- update p2 n2
          v2 <- fetch p3
          pure ()
        |]
      let after = [expr|
          n1 <- pure (COne)
          n2 <- pure (CTwo)
          p1 <- store n1
          n3 <- pure (CPtr p1)
          (CPtr p2) @ _1 <- pure n3
          (CPtr p3) @ _2 <- pure n3
          v1 <- fetch p2
          _3 <- update p2 n2
          v2 <- fetch p3
          pure ()
        |]

      let hptResult = HPTResult
            { HPT._memory   = mempty
            , HPT._register = Map.fromList
                [ ("p1", loc 0)
                , ("p2", loc 0)
                , ("p3", loc 0)
                , ("n3", mkTySet [(cPtr, [[HPT.T_Location 0]])])
                ]
            , HPT._function = mempty
            }
          sharedLocs = Set.fromList [0]
          shResult = SharingResult hptResult sharedLocs

      nonSharedElimination' shResult (ctx before) `sameAs` (ctx after)

    it "shared as-pattern" $ do
      let before = [expr|
          n1 <- pure (COne)
          n2 <- pure (CTwo)
          p1 <- store n1
          n3 <- pure (CPtr p1)
          (CPtr p2) @ n4 <- pure n3
          (CPtr p3) @ _1 <- pure n4
          v1 <- fetch p2
          _2 <- update p2 n2
          v2 <- fetch p3
          pure ()
        |]
      let after = [expr|
          n1 <- pure (COne)
          n2 <- pure (CTwo)
          p1 <- store n1
          n3 <- pure (CPtr p1)
          (CPtr p2) @ n4 <- pure n3
          (CPtr p3) @ _1 <- pure n4
          v1 <- fetch p2
          _2 <- update p2 n2
          v2 <- fetch p3
          pure ()
        |]

      let hptResult = HPTResult
            { HPT._memory   = mempty
            , HPT._register = Map.fromList
                [ ("p1", loc 0)
                , ("p2", loc 0)
                , ("p3", loc 0)
                , ("n3", mkTySet [(cPtr, [[HPT.T_Location 0]])])
                , ("n4", mkTySet [(cPtr, [[HPT.T_Location 0]])])
                ]
            , HPT._function = mempty
            }
          sharedLocs = Set.fromList [0]
          shResult = SharingResult hptResult sharedLocs

      nonSharedElimination' shResult (ctx before) `sameAs` (ctx after)

    it "shared case scrutinee" $ do
      let before = [expr|
          n1 <- pure (COne)
          n2 <- pure (CTwo)
          p1 <- store n1
          n3 <- pure (CPtr p1)
          case n3 of
            (CPtr p2) @ n4 ->
              (CPtr p3) @ _1 <- pure n4
              v1 <- fetch p2
              _2 <- update p2 n2
              v2 <- fetch p3
              pure ()
        |]
      let after = [expr|
          n1 <- pure (COne)
          n2 <- pure (CTwo)
          p1 <- store n1
          n3 <- pure (CPtr p1)
          case n3 of
            (CPtr p2) @ n4 ->
              (CPtr p3) @ _1 <- pure n4
              v1 <- fetch p2
              _2 <- update p2 n2
              v2 <- fetch p3
              pure ()
        |]

      let hptResult = HPTResult
            { HPT._memory   = mempty
            , HPT._register = Map.fromList
                [ ("p1", loc 0)
                , ("p2", loc 0)
                , ("p3", loc 0)
                , ("n3", mkTySet [(cPtr, [[HPT.T_Location 0]])])
                , ("n4", mkTySet [(cPtr, [[HPT.T_Location 0]])])
                ]
            , HPT._function = mempty
            }
          sharedLocs = Set.fromList [0]
          shResult = SharingResult hptResult sharedLocs

      nonSharedElimination' shResult (ctx before) `sameAs` (ctx after)
