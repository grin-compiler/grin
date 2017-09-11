{-# language LambdaCase #-}
{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language PatternGuards #-}
{-# language PatternSynonyms #-}
{-# language NoMonomorphismRestriction #-}
{-# language ScopedTypeVariables #-}
{-# language RankNTypes #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language DataKinds #-}
{-# language KindSignatures #-}
{-# language PolyKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module CodeGen.X86.Tests (runTests) where

import Data.Monoid
import Data.Maybe
import Data.List
import Data.Bits
import Data.Int
import Data.Word

import Test.QuickCheck hiding ((.&.), label)
import Debug.Trace

import CodeGen.X86.Asm
import CodeGen.X86.CodeGen
import CodeGen.X86.FFI
import CodeGen.X86.Utils

import Foreign

foreign import ccall "dynamic" dcb :: FunPtr Bool -> Bool
instance Callable Bool where dynCCall = dcb

------------------------------------------------------------------------------

class HasSigned a where
    type Signed a
    toSigned   :: a -> Signed a
    fromSigned :: Signed a -> a
    shiftMask  :: a

instance HasSigned Word8  where
    type Signed Word8  = Int8
    toSigned   = fromIntegral
    fromSigned = fromIntegral
    shiftMask  = 0x1f

instance HasSigned Word16 where
    type Signed Word16 = Int16
    toSigned   = fromIntegral
    fromSigned = fromIntegral
    shiftMask  = 0x1f

instance HasSigned Word32 where
    type Signed Word32 = Int32
    toSigned   = fromIntegral
    fromSigned = fromIntegral
    shiftMask  = 0x1f

instance HasSigned Word64 where
    type Signed Word64 = Int64
    toSigned   = fromIntegral
    fromSigned = fromIntegral
    shiftMask  = 0x3f

------------------------------------------------------------------------------

prop_integral x@(Integral y) = x == y

------------------------------------------------------------------------------

instance Arbitrary Size  where arbitrary = elements [S8, S16, S32, S64]

instance Arbitrary Scale where arbitrary = elements [s1, s2, s4, s8]

arbVal :: Size -> Gen Int64
arbVal S8  = fromIntegral <$> (arbitrary :: Gen Int8)
arbVal S16 = fromIntegral <$> (arbitrary :: Gen Int16)
arbVal S32 = fromIntegral <$> (arbitrary :: Gen Int32)
arbVal S64 = fromIntegral <$> (arbitrary :: Gen Int64)

genReg8 :: Gen (Reg S8)
genReg8 = elements ((NormalReg <$> [0..15]) ++ (HighReg <$> [0..3]))
genReg16 :: Gen (Reg S16)
genReg16 = NormalReg <$> elements [0..15]
genReg32 :: Gen (Reg S32)
genReg32 = NormalReg <$> elements [0..15]
genReg64 :: Gen (Reg S64)
genReg64 = NormalReg <$> elements [0..15]

instance IsSize s => Arbitrary (Reg s) where
    arbitrary = f (ssize :: SSize s) where
        f :: SSize s -> Gen (Reg s)
        f SSize8  = genReg8
        f SSize16 = genReg16
        f SSize32 = genReg32
        f SSize64 = genReg64

genRegs = RegOp <$> arbitrary

genIPBase = pure $ ipRel $ Label 0

instance Arbitrary (Addr S64) where
    arbitrary = suchThat (Addr <$> base <*> disp <*> index) ok
      where
        ok (Addr Nothing _ NoIndex) = False
        ok (Addr Nothing _ (IndexReg sc _)) = sc == s1
        ok _ = True
        base = oneof
            [ return Nothing
            , Just <$> arbitrary
            ]
        disp = oneof
            [ return NoDisp
            , Disp <$> arbitrary
            ]
        index = oneof
            [ return NoIndex
            , IndexReg <$> arbitrary <*> iregs
            ]
        iregs = NormalReg <$> elements ([0..15] \\ [4])      -- sp cannot be index

genMems = MemOp <$> (arbitrary :: Gen (Addr S64))

instance IsSize s => Arbitrary (Operand RW s) where
    arbitrary = oneof
        [ genRegs
        , genMems
        , genIPBase
        ]

instance IsSize s => Arbitrary (Operand R s) where
    arbitrary = oneof
        [ fromIntegral <$> oneof (arbVal <$> [S8, S16, S32, S64])
        , genRegs
        , genMems
        , genIPBase
        ]

instance Arbitrary CodeLine where
    arbitrary = oneof
        [ op2 Add_
        , op2 Or_
        , op2 Adc_
        , op2 Sbb_
        , op2 And_
        , op2 Sub_
        , op2 Xor_
        , op2 Cmp_
        , op2 Test_
        , op2' Rol_
        , op2' Ror_
        , op2' Rcl_
        , op2' Rcr_
        , op2' Shl_
        , op2' Shr_
        , op2' Sar_
        , op2'' Mov_
        ]
      where
        op2 :: (forall s . IsSize s => Operand RW s -> Operand R s -> CodeLine) -> Gen CodeLine
        op2 op = oneof
            [ f op (arbitrary :: Gen (Operand RW S8))  arbitrary
            , f op (arbitrary :: Gen (Operand RW S16)) arbitrary
            , f op (arbitrary :: Gen (Operand RW S32)) arbitrary
            , f op (arbitrary :: Gen (Operand RW S64)) arbitrary
            ]
          where
            f :: forall s . IsSize s => (Operand RW s -> Operand R s -> CodeLine) -> Gen (Operand RW s) -> Gen (Operand R s) -> Gen CodeLine
            f op a b = uncurry op <$> suchThat ((,) <$> a <*> b) (\(a, b) -> noHighRex (regs a <> regs b) && ok' a b && okk a b)

        op2'' :: (forall s . IsSize s => Operand RW s -> Operand R s -> CodeLine) -> Gen CodeLine
        op2'' op = oneof
            [ f op (arbitrary :: Gen (Operand RW S8))  arbitrary
            , f op (arbitrary :: Gen (Operand RW S16)) arbitrary
            , f op (arbitrary :: Gen (Operand RW S32)) arbitrary
            , f op (arbitrary :: Gen (Operand RW S64)) arbitrary
            ]
          where
            f :: forall s . IsSize s => (Operand RW s -> Operand R s -> CodeLine) -> Gen (Operand RW s) -> Gen (Operand R s) -> Gen CodeLine
            f op a b = uncurry op <$> suchThat ((,) <$> a <*> b) (\(a, b) -> noHighRex (regs a <> regs b) && ok' a b && oki a b)

        op2' :: (forall s . IsSize s => Operand RW s -> Operand R S8 -> CodeLine) -> Gen CodeLine
        op2' op = oneof
            [ f op (arbitrary :: Gen (Operand RW S8))  arb
            , f op (arbitrary :: Gen (Operand RW S16)) arb
            , f op (arbitrary :: Gen (Operand RW S32)) arb
            , f op (arbitrary :: Gen (Operand RW S64)) arb
            ]
          where
            arb = oneof
                [ fromIntegral <$> (arbitrary :: Gen Word8)
                , return cl
                ]

            f :: forall s . IsSize s => (Operand RW s -> Operand R S8 -> CodeLine) -> Gen (Operand RW s) -> Gen (Operand R S8) -> Gen CodeLine
            f op a b = uncurry op <$> suchThat ((,) <$> a <*> b) (\(a, b) -> noHighRex (regs a <> regs b) && ok' a b && okk a b && noteqreg a b)

        noteqreg a b = x == nub x where x = map phisicalReg $ regs a ++ regs b

        okk (size -> s) (ImmOp (Immediate i)) = isJust (integralToBytes True (no64 s) i)
        okk _ _ = True

        -- TODO: remove
        ok' RegOp{} RegOp{} = True
        ok' a b | isMemOp a && isMemOp b = False
        ok' a b = noteqreg a b

        oki x@RegOp{} (ImmOp (Immediate i)) = isJust (integralToBytes True (size x) i)
        oki a b = okk a b

---------------------------------------------------

evalOp :: forall a . (HasSigned a, Integral a, Integral (Signed a), FiniteBits (Signed a), Num a, FiniteBits a) => CodeLine -> Bool -> a -> a -> ((Bool, Bool), a)
evalOp op c = case op of
    Add_{}  -> mk (+)
    Or_{}   -> mk (.|.)
    Adc_{}  -> mk $ if c then \a b -> a + b + 1 else (+)
    Sbb_{}  -> mk $ if c then \a b -> a - b - 1 else (-)
    And_{}  -> mk (.&.)
    Sub_{}  -> mk (-)
    Xor_{}  -> mk xor
    Cmp_{}  -> mk_ (-) (\a b -> a)
    Test_{} -> mk_ (.&.) (\a b -> a)
    Mov_{}  -> \a b -> ((c, False), b)
    Shl_{}  -> \a b -> let i = fromIntegral (b .&. shiftMask) in ((if i == 0 then c else a `testBit` (finiteBitSize a - i), False), a `shiftL` i)
    Shr_{}  -> \a b -> let i = fromIntegral (b .&. shiftMask) in ((if i == 0 then c else a `testBit` (i-1), False), a `shiftR` i)
    Sar_{}  -> \a b -> let i = fromIntegral (b .&. shiftMask) in ((if i == 0 then c else toSigned a `testBit'` (i-1), False), fromSigned (toSigned a `shiftR` i))
    Rol_{}  -> \a b -> let i = fromIntegral (b .&. shiftMask) in ((if i == 0 then c else a `testBit` ((finiteBitSize a - i) `mod` finiteBitSize a), False), a `roL` i)
    Ror_{}  -> \a b -> let i = fromIntegral (b .&. shiftMask) in ((if i == 0 then c else a `testBit` ((i-1) `mod` finiteBitSize a), False), a `roR` i)
    Rcl_{}  -> \a b -> let i = fromIntegral (b .&. shiftMask) `mod` (finiteBitSize a + 1) in ((if i == 0 then c else a `testBit` (finiteBitSize a - i), False), rcL c a i)
    Rcr_{}  -> \a b -> let i = fromIntegral (b .&. shiftMask) `mod` (finiteBitSize a + 1) in ((if i == 0 then c else a `testBit` (i-1), False), rcR c a i)

  where
    mk :: (forall b . (Num b, Bits b, Integral b) => b -> b -> b) -> a -> a -> ((Bool, Bool), a)
    mk f = mk_ f f

    mk_ :: (forall b . (Num b, Bits b, Integral b) => b -> b -> b) -> (a -> a -> a) -> a -> a -> ((Bool, Bool), a)
    mk_ f g a b = ((extend (f a b) /= f (extend a) (extend b), sextend (f a b) /= f (sextend a) (sextend b)), g a b)

    extend :: a -> Integer
    extend = fromIntegral
    sextend :: a -> Integer
    sextend = fromIntegral . toSigned

    rcL c a 0 = a
    rcL c a i = (if c then setBit else clearBit) (a `shiftL` i .|. a `shiftR` (finiteBitSize a - i + 1)) (i - 1)

    rcR c a 0 = a
    rcR c a i = (if c then setBit else clearBit) (a `shiftR` i .|. a `shiftL` (finiteBitSize a - i + 1)) (finiteBitSize a - i)

    roL a i = a `shiftL` j .|. a `shiftR` (finiteBitSize a - j)
      where
        j = i `mod` finiteBitSize a

    roR a i = a `shiftR` j .|. a `shiftL` (finiteBitSize a - j)
      where
        j = i `mod` finiteBitSize a

    testBit' a i
        | isSigned a && i >= finiteBitSize a = testBit a (finiteBitSize a - 1)
        | otherwise = testBit a i


data InstrTest = IT String Code

instance Show InstrTest where show (IT s _) = s

instance Arbitrary InstrTest where
    arbitrary = do
        i <- arbitrary
        cF <- arbitrary
        let   fff :: forall s s' r . (IsSize s, IsSize s') => CodeLine -> (Operand RW s -> Operand r s' -> CodeLine) -> Operand RW s -> Operand r s' -> Gen InstrTest
              fff op op' a b = do
                let
                    (f1: f2: _) = map RegOp $ filter (`notElem` (regi a ++ regi b)) $ NormalReg <$> [8..15]
                    regi = map phisicalReg . regs

                    ff :: Operand RW s -> Operand k s' -> Gen (Int64, Int64, Code -> Code)
                    ff a@(RegOp x) (RegOp x') | Just Refl <- sizeEqCheck x x', x == x' = do
                        (av, inita) <- mkVal f2 a
                        return (av, av, inita)
                    ff (MemOp (Addr (Just x) _ _)) (RegOp x') | phisicalReg (SReg x) == phisicalReg (SReg x') = error "TODO" {-do
                        (av, inita) <- mkVal a
                        return (av, av, inita) -}
                    ff a_ b_ = do
                        (av, inita) <- mkVal f2 a_
                        (bv, initb) <- mkVal f2 b_
                        return (av, bv, inita . initb)

                (av, bv, initab) <- ff a b
                let
                    code = mdo
                        mapM_ push sr
                        mov f1 rsp
                        pushf
                        pop rax
                        push rax
                        popf
                        initab (initcf >> cc >> mova)
                        mkRes
                        mov rsp f1 {- <> traceReg "X" rdx' -}
                        mapM_ pop $ reverse sr
                        ret

                    sr = [rsi, rdi, rbx, rbp, r12, r13, r14, r15]

                    cc = mkCodeLine i
                    initcf = if cF then stc else clc
                    mova = case a of
                        RegOp (NormalReg 0x2) -> return ()
                        _ -> mov rdx' a
                    mkRes = otest i $ if_ (if cF' then C else NC) (xor_ rax rax) $ do
                        xor_ rax rax
                        mov rcx res
                        cmp rcx' rdx'
                        unless NZ $ inc rax
                    isShift = \case
                        Rol_{} -> True
                        Ror_{} -> True
                        Rcl_{} -> True
                        Rcr_{} -> True
                        Shl_{} -> True
                        Shr_{} -> True
                        Sar_{} -> True
                        _ -> False
                    otest i x | isShift i = x
                    otest _ x = if_ (if oF' then O else NO) (xor_ rax rax) x

                    rcx' = resizeOperand rcx :: Operand RW s
                    rdx' = resizeOperand rdx :: Operand RW s
                    sa = size a

                    ((cF', oF'), res) = case sa of
                        S8  -> fromIntegral <$> evalOp op cF (fromIntegral av) (fromIntegral bv :: Word8)
                        S16 -> fromIntegral <$> evalOp op cF (fromIntegral av) (fromIntegral bv :: Word16)
                        S32 -> fromIntegral <$> evalOp op cF (fromIntegral av) (fromIntegral bv :: Word32)
                        S64 -> fromIntegral <$> evalOp op cF (fromIntegral av) (fromIntegral bv :: Word64)

                    msg = unlines [show cc, "input a: " ++ show av, "input b: " ++ show bv, "input flags: " ++ show cF, "output: " ++ show res, "output flags: " ++ show cF' ++ " " ++ show oF']

                return $ traceShow cc $ IT msg code

        case i of
            Add_ a_ b_ -> fff i Add_ a_ b_
            Or_  a_ b_ -> fff i Or_  a_ b_
            Adc_ a_ b_ -> fff i Adc_ a_ b_
            Sbb_ a_ b_ -> fff i Sbb_ a_ b_
            And_ a_ b_ -> fff i And_ a_ b_
            Sub_ a_ b_ -> fff i Sub_ a_ b_
            Xor_ a_ b_ -> fff i Xor_ a_ b_
            Cmp_ a_ b_ -> fff i Cmp_ a_ b_
            Test_ a_ b_ -> fff i Test_ a_ b_
            Rol_ a_ b_ -> fff i Rol_ a_ b_
            Ror_ a_ b_ -> fff i Ror_ a_ b_
            Rcl_ a_ b_ -> fff i Rcl_ a_ b_
            Rcr_ a_ b_ -> fff i Rcr_ a_ b_
            Shl_ a_ b_ -> fff i Shl_ a_ b_
            Shr_ a_ b_ -> fff i Shr_ a_ b_
            Sar_ a_ b_ -> fff i Sar_ a_ b_
            Mov_ a_ b_ -> fff i Mov_ a_ b_

      where
        mkVal :: IsSize s => Operand RW S64 -> Operand k s -> Gen (Int64, Code -> Code)
        mkVal _ o@(ImmOp (Immediate w)) = return (w, id)
        mkVal _ o@(RegOp x) = do
            v <- arbVal $ size o
            return (v, (mov (RegOp x) (fromIntegral v) >>))
        mkVal helper x@(IPMemOp LabelRelValue{}) = do
            v <- arbVal $ size x
            return (v, \c -> mdo
                        jmp l
                        db $ toBytes v
                        l <- label
                        c)
        mkVal helper o@(MemOp (Addr (Just x) d i)) = do
            v <- arbVal $ size o
            (vi, setvi) <- case i of
                NoIndex -> return (0, return ())
                IndexReg sc i -> do
                    x <- arbVal $ size i
                    return (scaleFactor sc * x, mov (RegOp i) (fromIntegral x))
            let
                d' = (vi :: Int64) + case d of
                    NoDisp -> 0
                    Disp v -> fromIntegral v
                rx = resizeOperand $ RegOp x :: Operand RW S64
            return (v, ((leaData rx v >> mov helper (fromIntegral d') >> sub rx helper >> setvi) >>))
        mkVal helper o@(MemOp (Addr Nothing d (IndexReg sc x))) = do
            v <- arbVal $ size o
            let
                d' = case d of
                    NoDisp -> 0 :: Int64
                    Disp v -> fromIntegral v
                rx = resizeOperand $ RegOp x :: Operand RW S64
            return (v, ((leaData rx v >> mov helper (fromIntegral d') >> sub rx helper) >>))


propInstr (IT _ c) = compile c :: Bool

tests num = quickCheckWith stdArgs { maxSuccess = num } propInstr

-----------------------------------------

return []

-- | Run all tests
runTests = do
    $quickCheckAll
    tests 2000

