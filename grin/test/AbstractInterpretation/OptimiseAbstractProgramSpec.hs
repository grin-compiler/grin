{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module AbstractInterpretation.OptimiseAbstractProgramSpec where

import Test.Hspec
import Grin.Grin
import Grin.TH
import Grin.PrimOpsPrelude
import AbstractInterpretation.IR
import AbstractInterpretation.Reduce
import AbstractInterpretation.HeapPointsTo.CodeGen
import AbstractInterpretation.OptimiseAbstractProgram


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
    grinMain = t1 <- store (CInt 1)
               t2 <- store (CInt 10000)
               t3 <- store (Fupto t1 t2)
               t4 <- store (Fsum t3)
               (CInt r') <- eval t4
               _prim_int_print r'

    upto m n = (CInt m') <- eval m
               (CInt n') <- eval n
               b' <- _prim_int_gt m' n'
               if b' then
                pure (CNil)
               else
                m1' <- _prim_int_add m' 1
                m1 <- store (CInt m1')
                p <- store (Fupto m1 n)
                pure (CCons m p)

    sum l = l2 <- eval l
            case l2 of
              (CNil)       -> pure (CInt 0)
              (CCons x xs) -> (CInt x') <- eval x
                              (CInt s') <- sum xs
                              ax' <- _prim_int_add x' s'
                              pure (CInt ax')

    eval q = v <- fetch q
             case v of
              (CInt x'1)    -> pure v
              (CNil)        -> pure v
              (CCons y ys)  -> pure v
              (Fupto a b)   -> w <- upto a b
                               update q w
                               pure w
              (Fsum c)      -> z <- sum c
                               update q z
                               pure z
  |]
