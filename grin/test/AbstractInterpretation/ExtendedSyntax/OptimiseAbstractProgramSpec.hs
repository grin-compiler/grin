{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module AbstractInterpretation.ExtendedSyntax.OptimiseAbstractProgramSpec where

import Test.Hspec

import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.PrimOpsPrelude
import AbstractInterpretation.ExtendedSyntax.IR
import AbstractInterpretation.ExtendedSyntax.Reduce
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGen
import AbstractInterpretation.ExtendedSyntax.OptimiseAbstractProgram
import Transformations.ExtendedSyntax.Conversion (convertToNew)


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "HPT calculation optimiser" $ do
    let absProgram0 = fst $ codeGen testProgram
    it "does not change the number of instructions" $ do
      let absProgram1 = optimiseAbstractProgram absProgram0
      instructionCount absProgram0 `shouldBe` instructionCount absProgram1

    xit "creates a code that runs no worse than the original" $ do
      let absProgram1 = optimiseAbstractProgram absProgram0
      let absProgram2 = optimiseAbstractProgram absProgram1
          AbsIntResult comp0 iters0 = evalAbstractProgram absProgram0
          AbsIntResult comp2 iters2 = evalAbstractProgram absProgram2
      comp2 `shouldBe` comp0
      (compare iters2 iters0) `shouldSatisfy` (`elem` [LT,EQ])

    xit "is idempotent" $ do
      let absProgram1 = optimiseAbstractProgram absProgram0
      let absProgram2 = optimiseAbstractProgram absProgram1
      (_absInstructions absProgram1) `shouldBe` (_absInstructions absProgram2)

testProgram :: Exp
testProgram = withPrimPrelude [prog|
  grinMain =
    y.0 <- pure 1
    v.0 <- pure (CInt y.0)
    t1  <- store v.0
    y.1 <- pure 10000
    v.1 <- pure (CInt y.1)
    t2  <- store v.1
    v.2 <- pure (Fupto t1 t2)
    t3  <- store v.2
    v.3 <- pure (Fsum t3)
    t4  <- store v.3
    (CInt r') @ p.0 <- eval $ t4
    _prim_int_print $ r'

  upto m n =
    (CInt m') @ p.2 <- eval $ m
    (CInt n') @ p.1 <- eval $ n
    b' <- _prim_int_gt $ m' n'
    case b' of
      #True @ alt.0 ->
        v.4 <- pure (CNil)
        pure v.4
      #False @ alt.1 ->
        x.7 <- pure 1
        m1' <- _prim_int_add $ m' x.7
        v.5 <- pure (CInt m1')
        m1  <- store v.5
        v.6 <- pure (Fupto m1 n)
        p   <- store v.6
        v.7 <- pure (CCons m p)
        pure v.7

  sum l =
    l2 <- eval $ l
    case l2 of
      (CNil) @ alt.2 ->
        y.10 <- pure 0
        v.8  <- pure (CInt y.10)
        pure v.8
      (CCons x xs) @ alt.3 ->
        (CInt x') @ p.4 <- eval $ x
        (CInt s') @ p.3 <- sum $ xs
        ax' <- _prim_int_add $ x' s'
        v.9 <- pure (CInt ax')
        pure v.9

  eval q =
    v <- fetch q
    case v of
      (CInt x'1) @ alt.4 ->
        pure v
      (CNil) @ alt.5 ->
        pure v
      (CCons y ys) @ alt.6 ->
        pure v
      (Fupto a b) @ alt.7 ->
        w <- upto $ a b
        p.5 <- update q w
        pure w
      (Fsum c) @ alt.8 ->
        z <- sum $ c
        p.6 <- update q z
        pure z
  |]
