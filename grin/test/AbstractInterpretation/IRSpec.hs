{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module AbstractInterpretation.IRSpec where

import Control.Monad
import Text.Printf
import Test.Hspec
import Test.QuickCheck

import Lens.Micro

import Grin.Grin
import Grin.TH
import AbstractInterpretation.IR
import AbstractInterpretation.Reduce
import AbstractInterpretation.HeapPointsTo.CodeGen


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "HPT calculation" $ do
    let hptProgram0 = fst $ codeGen testProgram
    it "is instruction order independent" $
      forAll (randomizeInstructions . _absInstructions $ hptProgram0) $ \randomised ->
        let hptProgram1 = set (absInstructions) randomised  hptProgram0
            AbsIntResult comp0 iters0 = evalAbstractProgram hptProgram0
            AbsIntResult comp1 iters1 = evalAbstractProgram hptProgram1
        in label (printf "HPT iterations %d/%d" iters0 iters1)
                 $ comp0 == comp1

randomizeInstructions :: [Instruction] -> Gen [Instruction]
randomizeInstructions is0 = do
  is1 <- shuffle is0
  forM is1 $ \case
    If{..} -> If condition srcReg <$> randomizeInstructions instructions
    rest -> pure rest

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
