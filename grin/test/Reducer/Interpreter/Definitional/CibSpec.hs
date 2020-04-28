module Reducer.Interpreter.Definitional.CibSpec where

import Data.Monoid
import Control.Exception
import Test.Hspec
import Reducer.Interpreter.Base
import Reducer.Interpreter.Definitional.Cib
import Reducer.Interpreter.Definitional.Internal
import Data.Functor.Foldable
import Data.Functor.Sum
import Grin.ExtendedSyntax.Syntax

import qualified Reducer.Interpreter.Store as Store
import qualified Data.Map as Map


test :: IO ()
test = hspec spec

spec :: Spec
spec = describe "Counting Immutable Beans operands" $ do
  it "Increment reference counters" $ do
    let exp =
          Fix $ InL $ ProgramF [] $
            [ Fix $ InL $DefF "main" [] $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Nil") [])))) (VarPat "n1") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n1"))) (VarPat "p1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Cons") ["p1"])))) (VarPat "n2") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n2"))) (VarPat "p2") $
                Fix $ InL $ EBindF (Fix (InR (IncF "p2"))) (VarPat "c1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF Unit))) (VarPat "r1") $
                Fix $ InL $ SReturnF (Var "r1")
            ]
    (Left DUnit, store) <- evalCib Map.empty "main" exp
    Store.size store `shouldBe` 2
    foldMap (\(HeapNode _ (RefCounter c)) -> All (c==2)) store `shouldBe` (All True)

  it "Decrement reference counters" $ do
    let exp =
          Fix $ InL $ ProgramF [] $
            [ Fix $ InL $DefF "main" [] $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Nil") [])))) (VarPat "n1") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n1"))) (VarPat "p1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Cons") ["p1"])))) (VarPat "n2") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n2"))) (VarPat "p2") $
                Fix $ InL $ EBindF (Fix (InR (IncF "p2"))) (VarPat "c1") $
                Fix $ InL $ EBindF (Fix (InR (IncF "p2"))) (VarPat "c2") $
                Fix $ InL $ EBindF (Fix (InR (DecF "p2"))) (VarPat "c3") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF Unit))) (VarPat "r1") $
                Fix $ InL $ SReturnF (Var "r1")
            ]
    (Left DUnit, store) <- evalCib Map.empty "main" exp
    Store.size store `shouldBe` 2
    foldMap (\(HeapNode _ (RefCounter c)) -> All (c==2)) store `shouldBe` (All True)

  it "Reset returns location if there is no reference to it" $ do
    let exp =
          Fix $ InL $ ProgramF [] $
            [ Fix $ InL $DefF "main" [] $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Nil") [])))) (VarPat "n1") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n1"))) (VarPat "p1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Cons") ["p1"])))) (VarPat "n2") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n2"))) (VarPat "p2") $
                Fix $ InL $ EBindF (Fix (InR (DecF "p2"))) (VarPat "c1") $
                Fix $ InL $ EBindF (Fix (InR (ResetF "p1"))) (VarPat "r1") $
                Fix $ InL $ SReturnF (Var "r1")
            ]
    (Left val, store) <- evalCib Map.empty "main" exp
    val `shouldBe` (DVal (SLoc (Loc 0)))

  it "Reset returns BOX if there is no reference" $ do
    let exp =
          Fix $ InL $ ProgramF [] $
            [ Fix $ InL $DefF "main" [] $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Nil") [])))) (VarPat "n1") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n1"))) (VarPat "p1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Cons") ["p1"])))) (VarPat "n2") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n2"))) (VarPat "p2") $
                Fix $ InL $ EBindF (Fix (InR (ResetF "p1"))) (VarPat "r1") $
                Fix $ InL $ SReturnF (Var "r1")
            ]
    (Right val, store) <- evalCib Map.empty "main" exp
    val `shouldBe` Box

  it "Reuse creates new reference when BOX is passed in." $ do
    let exp =
          Fix $ InL $ ProgramF [] $
            [ Fix $ InL $DefF "main" [] $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Nil") [])))) (VarPat "n1") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n1"))) (VarPat "p1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Cons") ["p1"])))) (VarPat "n2") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n2"))) (VarPat "p2") $
                Fix $ InL $ EBindF (Fix (InR (ResetF "p1"))) (VarPat "r1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "NewRef") [])))) (VarPat "r2") $
                Fix $ InL $ EBindF (Fix (InR (ReuseF "r1" "r2"))) (VarPat "r3") $
                Fix $ InL $ SReturnF (Var "r3")
            ]
    (Left val, store) <- evalCib Map.empty "main" exp
    val `shouldBe` (DVal (SLoc (Loc 2)))
    foldMap (\(HeapNode _ (RefCounter c)) -> All (c > 0)) store `shouldBe` (All True)

  it "Reuse uses reference when reference is passed in." $ do
    let exp =
          Fix $ InL $ ProgramF [] $
            [ Fix $ InL $DefF "main" [] $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "Nil") [])))) (VarPat "n1") $
                Fix $ InL $ EBindF (Fix (InL (SStoreF "n1"))) (VarPat "p1") $
                Fix $ InL $ EBindF (Fix (InR (DecF "p1"))) (VarPat "c1") $
                Fix $ InL $ EBindF (Fix (InR (ResetF "p1"))) (VarPat "r1") $
                Fix $ InL $ EBindF (Fix (InL (SReturnF (ConstTagNode (Tag C "NewRef") [])))) (VarPat "r2") $
                Fix $ InL $ EBindF (Fix (InR (ReuseF "r1" "r2"))) (VarPat "r3") $
                Fix $ InL $ SReturnF (Var "r3")
            ]
    (Left val, store) <- evalCib Map.empty "main" exp
    val `shouldBe` (DVal (SLoc (Loc 0)))
    foldMap (\(HeapNode _ (RefCounter c)) -> All (c > 0)) store `shouldBe` (All True)
