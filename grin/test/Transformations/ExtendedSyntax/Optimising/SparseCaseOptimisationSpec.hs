{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.ExtendedSyntax.Optimising.SparseCaseOptimisationSpec where

import Transformations.ExtendedSyntax.Optimising.SparseCaseOptimisation


import qualified Data.Map as Map
import qualified Data.Vector as Vector

import Test.Hspec
import Test.ExtendedSyntax.New.Test hiding (newVar)
import Test.ExtendedSyntax.Assertions

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.TypeEnv

-- TODO: Replace type env construction with new primitives from Test.ExtendedSyntax.Util

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  testExprContext $ \ctx -> do
    it "Figure 4.25" $ do
      let teBefore = create $
            (newVar "v" $ T_NodeSet (Map.fromList [(Tag C "Cons", Vector.fromList [T_Int64, T_Location [1]])]))
      let before = [expr|
          v <- eval l
          case v of
            (CNil)       @ alt1 -> pure 1
            (CCons x xs) @ alt2 -> pure 2
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CCons x xs) @ alt2 -> pure 2
        |]
      let Right transformed = sparseCaseOptimisation teBefore before
      ctx (teBefore, transformed) `sameAs` (ctx (teBefore, after))

    it "Negative case, full context" $ do
      let teBefore = create $
            (newVar "v" $ T_NodeSet (Map.fromList
              [ (Tag C "Nil", Vector.fromList [])
              , (Tag C "Cons", Vector.fromList [T_Int64, T_Location [1]])
              ]))
      let before = [expr|
          v <- eval l
          case v of
            (CNil)       @ alt1 -> pure 1
            (CCons x xs) @ alt2 -> pure 2
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CNil)       @ alt1 -> pure 1
            (CCons x xs) @ alt2 -> pure 2
        |]
      let Right transformed = sparseCaseOptimisation teBefore before
      ctx (teBefore, transformed) `sameAs` (ctx (teBefore, after))

    it "default" $ do
      let teBefore = create $
            (newVar "v" $ T_NodeSet (Map.fromList [(Tag C "Cons", Vector.fromList [T_Int64, T_Location [1]])]))
      let before = [expr|
          v <- eval l
          case v of
            (CNil)       @ alt1 -> pure 1
            (CCons x xs) @ alt2 -> pure 2
            #default     @ alt3 -> pure 3
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CCons x xs) @ alt2 -> pure 2
        |]
      let Right transformed = sparseCaseOptimisation teBefore before
      ctx (teBefore, transformed) `sameAs` (ctx (teBefore, after))

    it "negative case with default" $ do
      let teBefore = create $
            (newVar "v" $ T_NodeSet (Map.fromList
              [ (Tag C "Nil2", Vector.fromList [])
              , (Tag C "Cons", Vector.fromList [T_Int64, T_Location [1]])
              ]))
      let before = [expr|
          v <- eval l
          case v of
            (CNil)       @ alt1 -> pure 1
            (CCons x xs) @ alt2 -> pure 2
            #default     @ alt3 -> pure 3
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CCons x xs) @ alt2 -> pure 2
            #default     @ alt3 -> pure 3
        |]
      let Right transformed = sparseCaseOptimisation teBefore before
      ctx (teBefore, transformed) `sameAs` (ctx (teBefore, after))
