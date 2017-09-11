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
module CodeGen.X86.CodeGen where

import Numeric
import Data.Maybe
import Data.Monoid
import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.Int
import Data.Word
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Tardis
import Debug.Trace

import CodeGen.X86.Asm

------------------------------------------------------- utils

takes [] _ = []
takes (i: is) xs = take i xs: takes is (drop i xs)

iff b a = if b then a else mempty

indicator :: Integral a => Bool -> a
indicator False = 0x0
indicator True  = 0x1

pattern FJust a = First (Just a)
pattern FNothing = First Nothing

integralToBytes :: (Bits a, Integral a) => Bool{-signed-} -> Size -> a -> Maybe Bytes
integralToBytes False S64 w = toBytes <$> (toIntegralSized w :: Maybe Word64)
integralToBytes False S32 w = toBytes <$> (toIntegralSized w :: Maybe Word32)
integralToBytes False S16 w = toBytes <$> (toIntegralSized w :: Maybe Word16)
integralToBytes False S8  w = toBytes <$> (toIntegralSized w :: Maybe Word8)
integralToBytes True  S64 w = toBytes <$> (toIntegralSized w :: Maybe Int64)
integralToBytes True  S32 w = toBytes <$> (toIntegralSized w :: Maybe Int32)
integralToBytes True  S16 w = toBytes <$> (toIntegralSized w :: Maybe Int16)
integralToBytes True  S8  w = toBytes <$> (toIntegralSized w :: Maybe Int8)

------------------------------------------------------- register packed with its size

data SReg where
    SReg :: IsSize s => Reg s -> SReg

phisicalReg :: SReg -> Reg S64
phisicalReg (SReg (HighReg x)) = NormalReg x
phisicalReg (SReg (NormalReg x)) = NormalReg x

isHigh (SReg HighReg{}) = True
isHigh _ = False

regs :: IsSize s => Operand r s -> [SReg]
regs = \case
    MemOp (Addr r _ i) -> foldMap (pure . SReg) r ++ case i of NoIndex -> []; IndexReg _ x -> [SReg x]
    RegOp r -> [SReg r]
    _ -> mempty

isRex (SReg x@(NormalReg r)) = r .&. 0x8 /= 0 || size x == S8 && r `shiftR` 2 == 1
isRex _ = False

noHighRex r = not $ any isHigh r && any isRex r

no64 S64 = S32
no64 s = s

------------------------------------------------------- code builder

type CodeBuilderRes = [Either Int (Int, Word8)]

type CodeBuilderTardis = Tardis (Int, [Int]) (Int, [Int], LabelState)

data CodeBuilder = CodeBuilder
    { minLen, maxLen :: Int
    , getCodeBuilder :: WriterT CodeBuilderRes CodeBuilderTardis ()
    }

codeBuilderLength (CodeBuilder a b _) | a == b = a

type LabelState = [[(Size, Int, Int)]]

instance Monoid CodeBuilder where
    mempty = CodeBuilder 0 0 $ return ()
    CodeBuilder mi ma a `mappend` CodeBuilder mi' ma' b = CodeBuilder (min mi mi') (max ma ma') $ a >> b

codeBytes :: [Word8] -> CodeBuilder
codeBytes bs = CodeBuilder n n $ do
    c <- lift $ mdo
        (c, ls, ps) <- getPast
        sendFuture (c + n, ls, ps)
        sendPast (ma + n, mls)
        ~(ma, mls) <- getFuture
        return c
    tell $ Right <$> zip [c..] bs
  where
    n = length bs

codeByte :: Word8 -> CodeBuilder
codeByte = codeBytes . (:[])

mkRef :: Size -> Int -> Label -> CodeBuilder
mkRef s@(sizeLen -> sn) offset (Label l_) = CodeBuilder sn sn $ do
    bs <- lift $ mdo
        (n, ls, ps) <- getPast
        sendFuture (n + sn, ls, ps')
        sendPast (ma + sn, mls)
        ~(ma, mls) <- getFuture
        let i = ls !! (- l - 1)
            vx = i - n - offset
            z = case s of
                S8  -> case vx of
                    Integral j -> toBytes (j :: Int8)
                    _ -> error $ show vx ++ " does not fit into an Int8"
                S32  -> case vx of
                    Integral j -> toBytes (j :: Int32)
                    _ -> error $ show vx ++ " does not fit into an Int32"
            ~(bs, ps')
                | l < 0 = (z, ps)
                | otherwise = ([], ins l (s, n, - n - offset) ps)
            l = l_ - length ls
        return $ zip [n..] bs
    tell $ Right <$> bs

ins :: Int -> a -> [[a]] -> [[a]]
ins 0 a [] = [a]: []
ins 0 a (as:ass) = (a:as): ass
ins n a [] = []: ins (n-1) a []
ins n a (as: ass) = as: ins (n-1) a ass

mkAutoRef :: [(Size, Bytes)] -> Label -> CodeBuilder
mkAutoRef ss (Label l_) = CodeBuilder (minimum sizes) (maximum sizes) $ do
    bs <- lift $ mdo
        (n, ls, ps) <- getPast
        sendFuture (n + sn, ls, ps')
        sendPast (ma + maximum sizes, mls)
        ~(ma, mls) <- getFuture
        let i = ls !! (- l - 1)
            vx = i - n
            z = g ss
            g [] = error $ show vx ++ " does not fit into auto size"
            g ((s, c): ss) = case (s, vx - length c - sizeLen s) of
                (S8,  Integral j) -> c <> toBytes (j :: Int8)
                (S32, Integral j) -> c <> toBytes (j :: Int32)
                _ -> g ss
            ~(sn, bs, ps')
                | l < 0 = (length z, z, ps)
                | otherwise = (nz, z', ins l (s, n + length z', - n - nz) ps)
            nz = length z' + sizeLen s
            ma' = mls !! l
            vx' = ma - ma'
            (z', s) = g' ss
            g' [] = error $ show vx' ++ " does not fit into auto size"
            g' ((s, c): ss) = case (s, vx') of
                (S8,  Integral (j :: Int8)) -> (c, s)
                (S32, Integral (j :: Int32)) -> (c, s)
                _ -> g' ss
            l = l_ - length ls
        return $ zip [n..] bs
    tell $ Right <$> bs
  where
    sizes = map (\(s, c) -> sizeLen s + length c) ss

-- prebuild code
preBuild :: Code -> Code
preBuild c = CodeM $ tell $ Prebuilt (compactCode (buildCode lc)) lc
  where
    lc = withLabels c

------------------------------------------------------- code to code builder

instance Show Code where
    show = show . withLabels

instance Show LCode where
    show c = unlines $ zipWith3 showLine is (takes (zipWith (-) (tail is ++ [s]) is) bs) ss
      where
        ss = snd . runWriter . flip evalStateT 0 . showCode $ c
        (x, s) = buildCode c
        bs = V.toList $ compactCode (x, s)
        is = [i | Left i <- x]

        showLine addr [] s = s
        showLine addr bs s = [showNibble i addr | i <- [5,4..0]] ++ " " ++ pad (2 * maxbytes) (concatMap showByte bs) ++ " " ++ s

        pad i xs = xs ++ replicate (i - length xs) ' '

        maxbytes = 12

compactCode :: (CodeBuilderRes, Int) -> V.Vector Word8
compactCode (x, s) = V.replicate s 0 V.// [p | Right p <- x]

buildTheCode :: Code -> (CodeBuilderRes, Int)
buildTheCode = buildCode . withLabels

buildCode :: LCode -> (CodeBuilderRes, Int)
buildCode x = (r, len)
  where
    ((_, r), (_, (len, _, _))) = flip runTardis ((0, []), (0, [], [])) . runWriterT . getCodeBuilder . mkCodeBuilder $ x

mkCodeBuilder :: LCode -> CodeBuilder
mkCodeBuilder = \case
    CodeLine x _ -> x
    Prebuilt v _ -> mkCodeBuilder' (Align_ 4) <> codeBytes (V.toList v)
    AppendCode x _ _ -> x
    EmptyCode -> mempty

newtype CodeM a = CodeM {unCodeM :: StateT Int (Writer LCode) a}
    deriving (Functor, Applicative, Monad, MonadFix)

type Code = CodeM ()

withLabels :: Code -> LCode
withLabels =
    snd . runWriter . flip evalStateT 0 . unCodeM

-- multi-byte nop operations
nops :: Int -> Bytes
nops = \case
    0 -> []
    1 -> [0x90]
    2 -> [0x66, 0x90]
    3 -> [0x0f, 0x1f, 0x00]
    4 -> [0x0f, 0x1f, 0x40, 0x00]
    5 -> [0x0f, 0x1f, 0x44, 0x00, 0x00]
    6 -> [0x66, 0x0f, 0x1f, 0x44, 0x00, 0x00]
    7 -> [0x0f, 0x1f, 0x80, 0x00, 0x00, 0x00, 0x00]
    8 -> [0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]
    9 -> [0x66, 0x0f, 0x1f, 0x84, 0x00, 0x00, 0x00, 0x00, 0x00]
    ((+(-2)) -> Integral x) -> [0xeb] ++ toBytes (x :: Int8) ++ replicate (fromIntegral x) 0x00
    ((+(-5)) -> Integral x) -> [0xe9] ++ toBytes (x :: Int32) ++ replicate (fromIntegral x) 0x00

mkCodeBuilder' :: CodeLine -> CodeBuilder
mkCodeBuilder' = \case
    Add_  a b -> op2 0x0 a b
    Or_   a b -> op2 0x1 a b
    Adc_  a b -> op2 0x2 a b
    Sbb_  a b -> op2 0x3 a b
    And_  a b -> op2 0x4 a b
    Sub_  a b -> op2 0x5 a b
    Xor_  a b -> op2 0x6 a b
    Cmp_  a b -> op2 0x7 a b

    Rol_ a b -> shiftOp 0x0 a b
    Ror_ a b -> shiftOp 0x1 a b
    Rcl_ a b -> shiftOp 0x2 a b
    Rcr_ a b -> shiftOp 0x3 a b
    Shl_ a b -> shiftOp 0x4 a b -- sal
    Shr_ a b -> shiftOp 0x5 a b
    Sar_ a b -> shiftOp 0x7 a b

    Xchg_ x@RegA r -> xchg_a r
    Xchg_ r x@RegA -> xchg_a r
    Xchg_ dest src -> op2' 0x43 dest' src
      where
        (dest', src') = if isMemOp src then (src, dest) else (dest, src)

    Test_ dest (mkImmNo64 (size dest) -> FJust (_, im)) -> case dest of
        RegA -> regprefix'' dest 0x54 mempty im
        _ -> regprefix'' dest 0x7b (reg8 0x0 dest) im
    Test_ dest (noImm "" -> src) -> op2' 0x42 dest' src'
      where
        (dest', src') = if isMemOp src then (src, dest) else (dest, src)

    Mov_ dest@(RegOp r) ((if size dest == S64 then mkImmU S32 <> mkImm S64 else mkImm (size dest)) -> FJust ((se, si), im))
        | (se, si, size dest) /= (True, S32, S64) -> regprefix si dest (oneReg (0x16 .|. indicator (size dest /= S8)) r) im
        | otherwise -> regprefix'' dest 0x63 (reg8 0x0 dest) im
    Mov_ dest@(size -> s) (mkImmNo64 s -> FJust (_, im)) -> regprefix'' dest 0x63 (reg8 0x0 dest) im
    Mov_ dest src -> op2' 0x44 dest $ noImm (show (dest, src)) src

    Cmov_ (Condition c) dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte (0x40 .|. c) <> reg2x8 dest src
    Bsf dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xbc <> reg2x8 dest src
    Bsr dest src | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xbd <> reg2x8 dest src
    Bt  src dest | size dest /= S8 -> regprefix2 src dest $ codeByte 0x0f <> codeByte 0xa3 <> reg2x8 dest src

    Lea_ dest src | size dest /= S8 -> regprefix2' (resizeOperand' dest src) dest 0x46 $ reg2x8 dest src
      where
        resizeOperand' :: IsSize s1 => Operand x s1 -> Operand RW s2 -> Operand RW s1
        resizeOperand' _ = resizeOperand

    Not_  a -> op1 0x7b 0x2 a
    Neg_  a -> op1 0x7b 0x3 a
    Inc_  a -> op1 0x7f 0x0 a
    Dec_  a -> op1 0x7f 0x1 a
    Bswap a@RegOp{} | size a >= S32 -> op1 0x07 0x1 a
    Bswap a  -> error $ "wrong bswap operand: " ++ show a

    Call_ (ImmOp (LabelRelValue S32 l)) -> codeByte 0xe8 <> mkRef S32 4 l
    Call_ a -> op1' 0xff 0x2 a

    Movd_ a@OpXMM b -> sse 0x6e a b
    Movd_ b a@OpXMM -> sse 0x7e a b
    Movq_ b a@OpXMM -> sse 0xd6 a b
    Movdqa_ a@OpXMM b -> sse 0x6f a b
    Movdqa_ b a@OpXMM -> sse 0x7f a b
    Paddb_  a b -> sse 0xfc a b
    Paddw_  a b -> sse 0xfd a b
    Paddd_  a b -> sse 0xfe a b
    Paddq_  a b -> sse 0xd4 a b
    Psubb_  a b -> sse 0xf8 a b
    Psubw_  a b -> sse 0xf9 a b
    Psubd_  a b -> sse 0xfa a b
    Psubq_  a b -> sse 0xfb a b
    Pxor_   a b -> sse 0xef a b
    Psllw_  a b -> sseShift 0x71 0x2 0xd1 a b
    Pslld_  a b -> sseShift 0x72 0x2 0xd2 a b
    Psllq_  a b -> sseShift 0x73 0x2 0xd3 a b
    Pslldq_ a b -> sseShift 0x73 0x7 undefined a b
    Psrlw_  a b -> sseShift 0x71 0x6 0xf1 a b
    Psrld_  a b -> sseShift 0x72 0x6 0xf2 a b
    Psrlq_  a b -> sseShift 0x73 0x6 0xf3 a b
    Psrldq_ a b -> sseShift 0x73 0x3 undefined a b
    Psraw_  a b -> sseShift 0x71 0x4 0xe1 a b
    Psrad_  a b -> sseShift 0x72 0x4 0xe2 a b

    Pop_ dest@(RegOp r) -> regprefix S32 dest (oneReg 0x0b r) mempty
    Pop_ dest -> regprefix S32 dest (codeByte 0x8f <> reg8 0x0 dest) mempty

    Push_ (mkImmS S8 -> FJust (_, im)) -> codeByte 0x6a <> im
    Push_ (mkImm S32 -> FJust (_, im)) -> codeByte 0x68 <> im
    Push_ dest@(RegOp r) -> regprefix S32 dest (oneReg 0x0a r) mempty
    Push_ dest -> regprefix S32 dest (codeByte 0xff <> reg8 0x6 dest) mempty

    Ret_   -> codeByte 0xc3
    Nop_   -> codeByte 0x90
    PushF_ -> codeByte 0x9c
    PopF_  -> codeByte 0x9d
    Cmc_   -> codeByte 0xf5
    Clc_   -> codeByte 0xf8
    Stc_   -> codeByte 0xf9
    Cli_   -> codeByte 0xfa
    Sti_   -> codeByte 0xfb
    Cld_   -> codeByte 0xfc
    Std_   -> codeByte 0xfd

    J_ (Condition c) (Just S8)  l -> codeByte (0x70 .|. c) <> mkRef S8 1 l
    J_ (Condition c) (Just S32) l -> codeByte 0x0f <> codeByte (0x80 .|. c) <> mkRef S32 4 l
    J_ (Condition c) Nothing    l -> mkAutoRef [(S8, [0x70 .|. c]), (S32, [0x0f, 0x80 .|. c])] l

    Jmp_ (Just S8)  l -> codeByte 0xeb <> mkRef S8 1 l
    Jmp_ (Just S32) l -> codeByte 0xe9 <> mkRef S32 4 l
    Jmp_ Nothing    l -> mkAutoRef [(S8, [0xeb]), (S32, [0xe9])] l

    Jmpq_ (ImmOp (LabelRelValue S32 l)) -> mkAutoRef [(S8, [0xeb]), (S32, [0xe9])] l
    Jmpq_ a -> op1' 0xff 0x4 a

    Label_ -> CodeBuilder 0 0 $ do
        bs <- lift $ mdo
            (n, ls, ps) <- getPast
            sendFuture (n, n: ls, ps')
            sendPast (ma, ma: mls)
            ~(ma, mls) <- getFuture
            let (bs, ps') = case ps of
                    [] -> ([], [])
                    corr: ps -> (concatMap g corr, ps)
                g (size, p, v) = zip [p..] $ case (size, v + n) of
                    (S8, Integral v) -> toBytes (v :: Int8)
                    (S32, Integral v) -> toBytes (v :: Int32)
                    (s, i) -> error $ show i ++ " doesn't fit into " ++ show s
            return bs
        tell $ Right <$> bs


    Data_ x -> codeBytes x

    Align_ s -> CodeBuilder 0 (s-1) $ do
        bs <- lift $ mdo
            (n, ls, ps) <- getPast
            sendFuture (n', ls, ps)
            sendPast (ma + s-1, mls)
            ~(ma, mls) <- getFuture
            let n' = fromIntegral $ ((fromIntegral n - 1 :: Int64) .|. (fromIntegral s - 1)) + 1
            return $ zip [n..] $ nops $ n' - n
        tell $ Right <$> bs

  where
    convertImm :: Bool{-signed-} -> Size -> Operand r s -> First ((Bool, Size), CodeBuilder)
    convertImm a b (ImmOp (Immediate c)) = First $ (,) (a, b) . codeBytes <$> integralToBytes a b c
    convertImm True b (ImmOp (LabelRelValue s d)) | b == s = FJust $ (,) (True, b) $ mkRef s (sizeLen s) d
    convertImm _ _ _ = FNothing

    mkImmS, mkImmU, mkImm, mkImmNo64 :: Size -> Operand r s -> First ((Bool, Size), CodeBuilder)
    mkImmS = convertImm True
    mkImmU = convertImm False
    mkImm s = mkImmS s <> mkImmU s
    mkImmNo64 s = mkImm (no64 s)

    xchg_a :: IsSize s => Operand r s -> CodeBuilder
    xchg_a dest@(RegOp r) | size dest /= S8 = regprefix (size dest) dest (oneReg 0x12 r) mempty
    xchg_a dest = regprefix'' dest 0x43 (reg8 0x0 dest) mempty

    toCode :: HasBytes a => a -> CodeBuilder
    toCode = codeBytes . toBytes

    sizePrefix_ :: [SReg] -> Size -> Operand r s -> Word8 -> CodeBuilder -> CodeBuilder -> CodeBuilder
    sizePrefix_ rs s r x c im
        | noHighRex rs = pre <> c <> displacement r <> im
        | otherwise = error "cannot use high register in rex instruction"
      where
        pre = case s of
            S8  -> mem32pre r <> maybePrefix40
            S16 -> codeByte 0x66 <> mem32pre r <> prefix40 x
            S32 -> mem32pre r <> prefix40 x
            S64 -> mem32pre r <> prefix40 (0x8 .|. x)
            S128 -> mem32pre r <> codeByte 0x66 <> maybePrefix40

        mem32pre :: Operand r s -> CodeBuilder
        mem32pre (MemOp r@Addr{}) | size r == S32 = codeByte 0x67
        mem32pre _ = mempty

        prefix40 x = iff (x /= 0) $ prefix40_ x
        prefix40_ x = codeByte $ 0x40 .|. x

        maybePrefix40 = iff (any isRex rs || x /= 0) (prefix40_ x)

        displacement :: Operand r s -> CodeBuilder
        displacement (IPMemOp (Immediate d)) = toCode d
        displacement (IPMemOp (LabelRelValue s@S32 d)) = mkRef s (sizeLen s + fromIntegral (codeBuilderLength im)) d
        displacement (MemOp (Addr b d i)) = mkSIB b i <> dispVal b d
          where
            mkSIB _ (IndexReg s (NormalReg 0x4)) = error "sp cannot be used as index"
            mkSIB _ (IndexReg s i) = f s $ reg8_ i
            mkSIB Nothing _ = f s1 0x4
            mkSIB (Just (reg8_ -> 0x4)) _ = f s1 0x4
            mkSIB _ _ = mempty

            f (Scale s) i = codeByte $ s `shiftL` 6 .|. i `shiftL` 3 .|. maybe 0x5 reg8_ b

            dispVal Just{} (Disp (Integral (d :: Int8))) = toCode d
            dispVal _ (Disp d) = toCode d
            dispVal Nothing _ = toCode (0 :: Int32)      -- [rbp] --> [rbp + 0]
            dispVal (Just (reg8_ -> 0x5)) _ = codeByte 0      -- [rbp] --> [rbp + 0]
            dispVal _ _ = mempty
        displacement _ = mempty

    reg8_ :: Reg t -> Word8
    reg8_ (NormalReg r) = r .&. 0x7
    reg8_ (HighReg r) = r .|. 0x4
    reg8_ (XMM r) = r .&. 0x7

    regprefix :: IsSize s => Size -> Operand r s -> CodeBuilder -> CodeBuilder -> CodeBuilder
    regprefix s r c im = sizePrefix_ (regs r) s r (extbits r) c im

    regprefix2 :: (IsSize s1, IsSize s) => Operand r1 s1 -> Operand r s -> CodeBuilder -> CodeBuilder
    regprefix2 r r' c = sizePrefix_ (regs r <> regs r') (size r) r (extbits r' `shiftL` 2 .|. extbits r) c mempty

    regprefix'' :: IsSize s => Operand r s -> Word8 -> CodeBuilder -> CodeBuilder -> CodeBuilder
    regprefix'' r p c = regprefix (size r) r $ extension r p <> c

    regprefix2' :: (IsSize s1, IsSize s) => Operand r1 s1 -> Operand r s -> Word8 -> CodeBuilder -> CodeBuilder
    regprefix2' r r' p c = regprefix2 r r' $ extension r p <> c

    sse :: IsSize s => Word8 -> Operand r S128 -> Operand r' s -> CodeBuilder
    sse op a@OpXMM b = regprefix S128 b (codeByte 0x0f <> codeByte op <> reg2x8 a b) mempty

    sseShift :: Word8 -> Word8 -> Word8 -> Operand RW S128 -> Operand r S8 -> CodeBuilder
    sseShift op x op' a@OpXMM b@(mkImmU S8 -> FJust (_, i)) = regprefix S128 b (codeByte 0x0f <> codeByte op <> reg8 x a) i
    -- TODO: xmm argument

    extension :: HasSize a => a -> Word8 -> CodeBuilder
    extension x p = codeByte $ p `shiftL` 1 .|. indicator (size x /= S8)

    extbits :: Operand r s -> Word8
    extbits = \case
        MemOp (Addr b _ i) -> maybe 0 indexReg b .|. case i of NoIndex -> 0; IndexReg _ x -> indexReg x `shiftL` 1
        RegOp r -> indexReg r
        _ -> 0
      where
        indexReg (NormalReg r) = r `shiftR` 3 .&. 1
        indexReg _ = 0

    reg8 :: Word8 -> Operand r s -> CodeBuilder
    reg8 w x = codeByte $ operMode x `shiftL` 6 .|. w `shiftL` 3 .|. rc x
      where
        operMode :: Operand r s -> Word8
        operMode (MemOp (Addr (Just (reg8_ -> 0x5)) NoDisp _)) = 0x1   -- [rbp] --> [rbp + 0]
        operMode (MemOp (Addr Nothing _ _)) = 0x0
        operMode (MemOp (Addr _ NoDisp _))  = 0x0
        operMode (MemOp (Addr _ (Disp (Integral (_ :: Int8))) _))  = 0x1
        operMode (MemOp (Addr _ Disp{} _))  = 0x2
        operMode IPMemOp{}                  = 0x0
        operMode _                          = 0x3

        rc :: Operand r s -> Word8
        rc (MemOp (Addr (Just r) _ NoIndex)) = reg8_ r
        rc MemOp{}   = 0x04      -- SIB byte
        rc IPMemOp{} = 0x05
        rc (RegOp r) = reg8_ r

    op2 :: IsSize s => Word8 -> Operand RW s -> Operand r s -> CodeBuilder
    op2 op dest@RegA src@(mkImmNo64 (size dest) -> FJust (_, im)) | size dest == S8 || isNothing (getFirst $ mkImmS S8 src)
        = regprefix'' dest (op `shiftL` 2 .|. 0x2) mempty im
    op2 op dest (mkImmS S8 <> mkImmNo64 (size dest) -> FJust ((_, k), im))
        = regprefix'' dest (0x40 .|. indicator (size dest /= S8 && k == S8)) (reg8 op dest) im
    op2 op dest src = op2' (op `shiftL` 2) dest $ noImm "1" src

    noImm :: String -> Operand r s -> Operand RW s
    noImm _ (RegOp r) = RegOp r
    noImm _ (MemOp a) = MemOp a
    noImm _ (IPMemOp a) = IPMemOp a
    noImm er _ = error $ "immediate value of this size is not supported: " ++ er

    op2' :: IsSize s => Word8 -> Operand RW s -> Operand RW s -> CodeBuilder
    op2' op dest src@RegOp{} = op2g op dest src
    op2' op dest@RegOp{} src = op2g (op .|. 0x1) src dest

    op2g :: (IsSize t, IsSize s) => Word8 -> Operand r s -> Operand r' t -> CodeBuilder
    op2g op dest src = regprefix2' dest src op $ reg2x8 src dest

    reg2x8 :: (IsSize s, IsSize s') => Operand r s -> Operand r' s' -> CodeBuilder
    reg2x8 (RegOp r) x = reg8 (reg8_ r) x

    op1_ :: IsSize s => Word8 -> Word8 -> Operand r s -> CodeBuilder -> CodeBuilder
    op1_ r1 r2 dest im = regprefix'' dest r1 (reg8 r2 dest) im

    op1 :: IsSize s => Word8 -> Word8 -> Operand r s -> CodeBuilder
    op1 a b c = op1_ a b c mempty

    op1' :: Word8 -> Word8 -> Operand r S64 -> CodeBuilder
    op1' r1 r2 dest = regprefix S32 dest (codeByte r1 <> reg8 r2 dest) mempty

    shiftOp :: IsSize s => Word8 -> Operand RW s -> Operand r S8 -> CodeBuilder
    shiftOp c dest (ImmOp (Immediate 1)) = op1 0x68 c dest
    shiftOp c dest (mkImmU S8 -> FJust (_, i)) = op1_ 0x60 c dest i
    shiftOp c dest RegCl = op1 0x69 c dest
    shiftOp _ _ _ = error "invalid shift operands"

    oneReg :: Word8 -> Reg t -> CodeBuilder
    oneReg x r = codeByte $ x `shiftL` 3 .|. reg8_ r

pattern OpXMM <- RegOp XMM{}

-------------------------------------------------------------- asm codes

data LCode where
    Prebuilt    :: V.Vector Word8 -> LCode -> LCode
    EmptyCode   :: LCode
    AppendCode  :: CodeBuilder -> LCode -> LCode -> LCode
    CodeLine    :: CodeBuilder -> CodeLine -> LCode

instance Monoid LCode where
    mempty  = EmptyCode
    mappend a b = AppendCode (mkCodeBuilder a <> mkCodeBuilder b) a b

ret     = mkCodeLine Ret_
nop     = mkCodeLine Nop_
pushf   = mkCodeLine PushF_
popf    = mkCodeLine PopF_
cmc     = mkCodeLine Cmc_
clc     = mkCodeLine Clc_
stc     = mkCodeLine Stc_
cli     = mkCodeLine Cli_
sti     = mkCodeLine Sti_
cld     = mkCodeLine Cld_
std     = mkCodeLine Std_
inc a   = mkCodeLine (Inc_ a)
dec a   = mkCodeLine (Dec_ a)
not_ a  = mkCodeLine (Not_ a)
neg a   = mkCodeLine (Neg_ a)
bswap a = mkCodeLine (Bswap a)
bsf a b = mkCodeLine (Bsf a b)
bsr a b = mkCodeLine (Bsr a b)
bt a b  = mkCodeLine (Bt  a b)
add a b = mkCodeLine (Add_ a b)
or_  a b = mkCodeLine (Or_  a b)
adc a b = mkCodeLine (Adc_ a b)
sbb a b = mkCodeLine (Sbb_ a b)
and_ a b = mkCodeLine (And_ a b)
sub a b = mkCodeLine (Sub_ a b)
xor_ a b = mkCodeLine (Xor_ a b)
cmp a b = mkCodeLine (Cmp_ a b)
test a b = mkCodeLine (Test_ a b)
mov a b = mkCodeLine (Mov_ a b)
cmov c a b = mkCodeLine (Cmov_ c a b)
rol a b = mkCodeLine (Rol_ a b)
ror a b = mkCodeLine (Ror_ a b)
rcl a b = mkCodeLine (Rcl_ a b)
rcr a b = mkCodeLine (Rcr_ a b)
shl a b = mkCodeLine (Shl_ a b)
shr a b = mkCodeLine (Shr_ a b)
sar a b = mkCodeLine (Sar_ a b)
xchg a b = mkCodeLine (Xchg_ a b)
movd   a b = mkCodeLine (Movd_   a b)
movq   a b = mkCodeLine (Movq_   a b)
movdqa a b = mkCodeLine (Movdqa_ a b)
paddb  a b = mkCodeLine (Paddb_  a b)
paddw  a b = mkCodeLine (Paddw_  a b)
paddd  a b = mkCodeLine (Paddd_  a b)
paddq  a b = mkCodeLine (Paddq_  a b)
psubb  a b = mkCodeLine (Psubb_  a b)
psubw  a b = mkCodeLine (Psubw_  a b)
psubd  a b = mkCodeLine (Psubd_  a b)
psubq  a b = mkCodeLine (Psubq_  a b)
pxor   a b = mkCodeLine (Pxor_   a b)
psllw  a b = mkCodeLine (Psllw_  a b)
pslld  a b = mkCodeLine (Pslld_  a b)
psllq  a b = mkCodeLine (Psllq_  a b)
pslldq a b = mkCodeLine (Pslldq_ a b)
psrlw  a b = mkCodeLine (Psrlw_  a b)
psrld  a b = mkCodeLine (Psrld_  a b)
psrlq  a b = mkCodeLine (Psrlq_  a b)
psrldq a b = mkCodeLine (Psrldq_ a b)
psraw  a b = mkCodeLine (Psraw_  a b)
psrad  a b = mkCodeLine (Psrad_  a b)
lea a b = mkCodeLine (Lea_ a b)
j a c   = mkCodeLine (J_ a Nothing c)
pop a   = mkCodeLine (Pop_ a)
push a  = mkCodeLine (Push_ a)
call a  = mkCodeLine (Call_ a)
jmpq a  = mkCodeLine (Jmpq_ a)
jmp b   = mkCodeLine (Jmp_ Nothing b)
db a    = mkCodeLine (Data_ a)
align a = mkCodeLine (Align_ a)

label :: CodeM Label
label = do
    i <- CodeM get
    CodeM $ put $ i+1
    mkCodeLine Label_
    return $ Label i

mkCodeLine :: CodeLine -> Code
mkCodeLine x = CodeM $ tell $ CodeLine (tellAddr <> mkCodeBuilder' x) x

tellAddr = CodeBuilder 0 0 $ do
    (c, _, _) <- lift getPast
    tell [Left c]

-------------


showCode = \case
    EmptyCode  -> return ()
    AppendCode _ a b -> showCode a >> showCode b
    Prebuilt _ c -> showCodeLine (Align_ 4) >> codeLine "{" >> showCode c >> codeLine "}"
    CodeLine _ x -> showCodeLine x

