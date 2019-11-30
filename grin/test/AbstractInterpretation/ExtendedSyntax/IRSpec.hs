{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes     #-}
module AbstractInterpretation.ExtendedSyntax.IRSpec where

import Control.Monad
import Text.Printf
import Test.Hspec
import Test.QuickCheck

import Lens.Micro

import Grin.ExtendedSyntax.TH
import Grin.ExtendedSyntax.Grin
import AbstractInterpretation.ExtendedSyntax.IR
import AbstractInterpretation.ExtendedSyntax.Reduce
import AbstractInterpretation.ExtendedSyntax.HeapPointsTo.CodeGen
import Transformations.ExtendedSyntax.Conversion (convertToNew)


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
  grinMain =
    y.0 <- pure 1
    v.0 <- pure (CInt y.0)
    t1 <- store v.0
    y.1 <- pure 10000
    v.1 <- pure (CInt y.1)
    t2 <- store v.1
    y.3 <- pure t2
    y.2 <- pure t1
    v.2 <- pure (Fupto y.2 y.3)
    t3 <- store v.2
    y.4 <- pure t3
    v.3 <- pure (Fsum y.4)
    t4 <- store v.3
    x.0 <- pure t4
    (CInt r')@p.0 <- eval $ x.0
    x.1 <- pure r'
    _prim_int_print $ x.1

  upto m n =
    x.2 <- pure m
    (CInt m')@p.2 <- eval $ x.2
    x.3 <- pure n
    (CInt n')@p.1 <- eval $ x.3
    x.5 <- pure n'
    x.4 <- pure m'
    b' <- _prim_int_gt $ x.4 x.5
    case b' of
      #True @ alt.0 ->
        v.4 <- pure (CNil)
        pure v.4
      #False @ alt.1 ->
        x.7 <- pure 1
        x.6 <- pure m'
        m1' <- _prim_int_add $ x.6 x.7
        y.5 <- pure m1'
        v.5 <- pure (CInt y.5)
        m1 <- store v.5
        y.7 <- pure n
        y.6 <- pure m1
        v.6 <- pure (Fupto y.6 y.7)
        p <- store v.6
        y.9 <- pure p
        y.8 <- pure m
        v.7 <- pure (CCons y.8 y.9)
        pure v.7

  sum l =
    x.8 <- pure l
    l2 <- eval $ x.8
    case l2 of
      (CNil) @ alt.2 ->
        y.10 <- pure 0
        v.8 <- pure (CInt y.10)
        pure v.8
      (CCons x xs) @ alt.3 ->
        x.9 <- pure x
        (CInt x')@p.4 <- eval $ x.9
        x.10 <- pure xs
        (CInt s')@p.3 <- sum $ x.10
        x.12 <- pure s'
        x.11 <- pure x'
        ax' <- _prim_int_add $ x.11 x.12
        y.11 <- pure ax'
        v.9 <- pure (CInt y.11)
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
        x.14 <- pure b
        x.13 <- pure a
        w <- upto $ x.13 x.14
        ()@p.5 <- update q w
        pure w
      (Fsum c) @ alt.8 ->
        x.15 <- pure c
        z <- sum $ x.15
        ()@p.6 <- update q z
        pure z
  |]
