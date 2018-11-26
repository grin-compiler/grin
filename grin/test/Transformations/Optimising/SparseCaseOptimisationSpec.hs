{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Transformations.Optimising.SparseCaseOptimisationSpec where

import Transformations.Optimising.SparseCaseOptimisation

import Test.Hspec
import Grin.Grin
import Grin.TH
import Test.Test hiding (newVar)
import Test.Assertions
import Grin.TypeEnv
import qualified Data.Map as Map
import qualified Data.Vector as Vector


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
            (CNil)       -> pure 1
            (CCons x xs) -> pure 2
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CCons x xs) -> pure 2
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
            (CNil)       -> pure 1
            (CCons x xs) -> pure 2
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CNil)       -> pure 1
            (CCons x xs) -> pure 2
        |]
      let Right transformed = sparseCaseOptimisation teBefore before
      ctx (teBefore, transformed) `sameAs` (ctx (teBefore, after))

    it "default" $ do
      let teBefore = create $
            (newVar "v" $ T_NodeSet (Map.fromList [(Tag C "Cons", Vector.fromList [T_Int64, T_Location [1]])]))
      let before = [expr|
          v <- eval l
          case v of
            (CNil)       -> pure 1
            (CCons x xs) -> pure 2
            #default     -> pure 3
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CCons x xs) -> pure 2
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
            (CNil)       -> pure 1
            (CCons x xs) -> pure 2
            #default     -> pure 3
        |]
      let after = [expr|
          v <- eval l
          case v of
            (CCons x xs) -> pure 2
            #default     -> pure 3
        |]
      let Right transformed = sparseCaseOptimisation teBefore before
      ctx (teBefore, transformed) `sameAs` (ctx (teBefore, after))

    it "const tag node scrutinee" $ do
      let before = [expr|
          case (CNil) of
            (CNil)       -> pure 1
            (CCons x xs) -> pure 2
            #default     -> pure 3
        |]
      let after = [expr|
          case (CNil) of
            (CNil)       -> pure 1
        |]
      let Right transformed = sparseCaseOptimisation emptyTypeEnv before
      ctx (emptyTypeEnv, transformed) `sameAs` (ctx (emptyTypeEnv, after))

    it "literal scrutinee" $ do
      let before = [expr|
          case 5 of
            0        -> pure 1
            1        -> pure 2
            #True    -> pure 666
            #default -> pure 3
        |]
      let after = [expr|
          case 5 of
            0        -> pure 1
            1        -> pure 2
            #default -> pure 3
        |]
      let Right transformed = sparseCaseOptimisation emptyTypeEnv before
      ctx (emptyTypeEnv, transformed) `sameAs` (ctx (emptyTypeEnv, after))

    it "udefined scrutinee" $ do
      let before = [expr|
          case (#undefined :: {CNil[]}) of
            (CNil)       -> pure 1
            (CCons x xs) -> pure 2
            #default     -> pure 3
        |]
      let after = [expr|
          case (#undefined :: {CNil[]}) of
            (CNil)       -> pure 1
        |]
      let Right transformed = sparseCaseOptimisation emptyTypeEnv before
      ctx (emptyTypeEnv, transformed) `sameAs` (ctx (emptyTypeEnv, after))