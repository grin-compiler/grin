{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module AbstractInterpretation.OptimiseHPTSpec where

import Test.Hspec
import Grin.Grin
import Grin.TH
import AbstractInterpretation.IR
import AbstractInterpretation.OptimiseHPT
import AbstractInterpretation.Reduce
import AbstractInterpretation.CodeGenMain


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "HPT calculation optimiser" $ do
    let Right hptProgram0 = codeGen testProgram
    it "does not change the number of instructions" $ do
      let hptProgram1 = optimiseHPT hptProgram0
      instructionCount hptProgram0 `shouldBe` instructionCount hptProgram1

    it "creates a code that runs no worse than the original" $ do
      let hptProgram1 = optimiseHPT hptProgram0
      let hptProgram2 = optimiseHPT hptProgram1
          (computer0, (HPTInfo hptIterations0)) = evalHPT hptProgram0
          (computer1, (HPTInfo hptIterations1)) = evalHPT hptProgram2
      computer1 `shouldBe` computer0
      (compare hptIterations1 hptIterations0) `shouldSatisfy` (`elem` [LT])

    xit "is idempotent" $ do
      let hptProgram1 = optimiseHPT hptProgram0
      let hptProgram2 = optimiseHPT hptProgram1
      (hptInstructions hptProgram1) `shouldBe` (hptInstructions hptProgram2)

testProgram :: Exp
testProgram = [prog|
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
