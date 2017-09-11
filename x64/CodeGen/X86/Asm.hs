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
{-# language StandaloneDeriving #-}
module CodeGen.X86.Asm where

import Numeric
import Data.List
import Data.Bits
import Data.Int
import Data.Word
import Control.Monad
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

------------------------------------------------------- utils

everyNth n [] = []
everyNth n xs = take n xs: everyNth n (drop n xs)

showNibble :: (Integral a, Bits a) => Int -> a -> Char
showNibble n x = toEnum (b + if b < 10 then 48 else 87)
  where
    b = fromIntegral $ x `shiftR` (4*n) .&. 0x0f

showByte b = [showNibble 1 b, showNibble 0 b]

showHex' x = "0x" ++ showHex x ""

pattern Integral xs <- (toIntegralSized -> Just xs)

------------------------------------------------------- byte sequences

type Bytes = [Word8]

class HasBytes a where toBytes :: a -> Bytes

instance HasBytes Word8  where toBytes w = [w]
instance HasBytes Word16 where toBytes w = [fromIntegral w, fromIntegral $ w `shiftR` 8]
instance HasBytes Word32 where toBytes w = [fromIntegral $ w `shiftR` n | n <- [0, 8.. 24]]
instance HasBytes Word64 where toBytes w = [fromIntegral $ w `shiftR` n | n <- [0, 8.. 56]]

instance HasBytes Int8  where toBytes w = toBytes (fromIntegral w :: Word8)
instance HasBytes Int16 where toBytes w = toBytes (fromIntegral w :: Word16)
instance HasBytes Int32 where toBytes w = toBytes (fromIntegral w :: Word32)
instance HasBytes Int64 where toBytes w = toBytes (fromIntegral w :: Word64)

------------------------------------------------------- sizes

-- | The size of a register (in bits)
data Size = S1 | S8 | S16 | S32 | S64 | S128
    deriving (Eq, Ord)

instance Show Size where
    show = \case
        S1   -> "bit"
        S8   -> "byte"
        S16  -> "word"
        S32  -> "dword"
        S64  -> "qword"
        S128 -> "oword"

mkSize  1 = S8
mkSize  2 = S16
mkSize  4 = S32
mkSize  8 = S64
mkSize 16 = S128

sizeLen = \case
    S8   ->  1
    S16  ->  2
    S32  ->  4
    S64  ->  8
    S128 -> 16

class HasSize a where size :: a -> Size

instance HasSize Word8  where size _ = S8
instance HasSize Word16 where size _ = S16
instance HasSize Word32 where size _ = S32
instance HasSize Word64 where size _ = S64
instance HasSize Int8   where size _ = S8
instance HasSize Int16  where size _ = S16
instance HasSize Int32  where size _ = S32
instance HasSize Int64  where size _ = S64

-- | Singleton type for size
data SSize (s :: Size) where
    SSize1   :: SSize S1
    SSize8   :: SSize S8
    SSize16  :: SSize S16
    SSize32  :: SSize S32
    SSize64  :: SSize S64
    SSize128 :: SSize S128

instance HasSize (SSize s) where
    size = \case
        SSize1   -> S1
        SSize8   -> S8
        SSize16  -> S16
        SSize32  -> S32
        SSize64  -> S64
        SSize128 -> S128

class IsSize (s :: Size) where
    ssize :: SSize s

instance IsSize S1   where ssize = SSize1
instance IsSize S8   where ssize = SSize8
instance IsSize S16  where ssize = SSize16
instance IsSize S32  where ssize = SSize32
instance IsSize S64  where ssize = SSize64
instance IsSize S128 where ssize = SSize128

data EqT s s' where
    Refl :: EqT s s

sizeEqCheck :: forall s s' f g . (IsSize s, IsSize s') => f s -> g s' -> Maybe (EqT s s')
sizeEqCheck _ _ = case (ssize :: SSize s, ssize :: SSize s') of
    (SSize8 , SSize8)  -> Just Refl
    (SSize16, SSize16) -> Just Refl
    (SSize32, SSize32) -> Just Refl
    (SSize64, SSize64) -> Just Refl
    _ -> Nothing

------------------------------------------------------- scale

-- | The scaling of an index. (replace with Size?)
newtype Scale = Scale Word8
    deriving (Eq)

s1 = Scale 0x0
s2 = Scale 0x1
s4 = Scale 0x2
s8 = Scale 0x3

toScale = \case
    1 -> s1
    2 -> s2
    4 -> s4
    8 -> s8

scaleFactor (Scale i) = case i of
    0x0 -> 1
    0x1 -> 2
    0x2 -> 4
    0x3 -> 8

------------------------------------------------------- operand

-- | An operand can be an immediate, a register, a memory address or RIP-relative (memory address relative to the instruction pointer)
data Operand :: Access -> Size -> * where
    ImmOp     :: Immediate Int64 -> Operand R s
    RegOp     :: Reg s -> Operand rw s
    MemOp     :: IsSize s' => Addr s' -> Operand rw s
    IPMemOp   :: Immediate Int32 -> Operand rw s

addr :: IsSize s => Address s -> Operand rw s'
addr = MemOp . makeAddr

-- | `addr` with specialized type
addr8 :: IsSize s => Address s -> Operand rw S8
addr8 = addr

-- | `addr` with specialized type
addr16 :: IsSize s => Address s -> Operand rw S16
addr16 = addr

-- | `addr` with specialized type
addr32 :: IsSize s => Address s -> Operand rw S32
addr32 = addr

-- | `addr` with specialized type
addr64 :: IsSize s => Address s -> Operand rw S64
addr64 = addr

data Immediate a
    = Immediate a
    | LabelRelValue Size{-size hint-} Label

-- Type of labels
newtype Label = Label {unLabel :: Int}

instance Show Label where
    show (Label i) = ".l" ++ show i

-- | Operand access modes
data Access
    = R     -- ^ readable operand
    | RW    -- ^ readable and writeable operand

-- | A register.
data Reg :: Size -> * where
    NormalReg :: Word8 -> Reg s      -- \"normal\" registers are for example @AL@, @BX@, @ECX@ or @RSI@
    HighReg   :: Word8 -> Reg S8     -- \"high\" registers are @AH@, @BH@, @CH@ etc
    XMM       :: Word8 -> Reg S128   -- XMM registers

deriving instance Eq (Reg s)
deriving instance Ord (Reg s)

-- | A (relative) address is made up base a base register, a displacement, and a (scaled) index.
-- For example in @[eax+4*ecx+20]@ the base register is @eax@, the displacement is @20@ and the
-- index is @4*ecx@.
data Addr s = Addr
    { baseReg        :: BaseReg s
    , displacement   :: Displacement
    , indexReg       :: IndexReg s
    }
    deriving (Eq)

type BaseReg s    = Maybe (Reg s)
data IndexReg s   = NoIndex | IndexReg Scale (Reg s)
    deriving (Eq)
type Displacement = Maybe Int32

pattern NoDisp = Nothing
pattern Disp a = Just a

-- | intruction pointer (RIP) relative address
ipRel :: Label -> Operand rw s
ipRel l = IPMemOp $ LabelRelValue S32 l

ipRelValue l = ImmOp $ LabelRelValue S32 l

-- | `ipRel` with specialized type
ipRel8 :: Label -> Operand rw S8
ipRel8 = ipRel

instance IsSize s => Show (Reg s) where
    show = \case
        XMM i -> "xmm" ++ show i
        HighReg i -> (["ah"," ch", "dh", "bh"] ++ repeat err) !! fromIntegral i
        r@(NormalReg i) -> (!! fromIntegral i) . (++ repeat err) $ case size r of
            S8   -> ["al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil"] ++ map (++ "b") r8
            S16  -> r0 ++ map (++ "w") r8
            S32  -> map ('e':) r0 ++ map (++ "d") r8
            S64  -> map ('r':) r0 ++ r8
          where
            r0 = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"]
            r8 = ["r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"]
      where
        err = error $ "show @ RegOp" -- ++ show (s, i)

instance IsSize s => Show (Addr s) where
    show (Addr b d i) = showSum $ shb b ++ shd d ++ shi i
      where
        shb Nothing = []
        shb (Just x) = [(True, show x)]
        shd NoDisp = []
        shd (Disp x) = [(signum x /= (-1), show (abs x))]
        shi NoIndex = []
        shi (IndexReg sc x) = [(True, show' (scaleFactor sc) ++ show x)]
        show' 1 = ""
        show' n = show n ++ " * "
        showSum [] = "0"
        showSum ((True, x): xs) = x ++ g xs
        showSum ((False, x): xs) = "-" ++ x ++ g xs
        g = concatMap (\(a, b) -> f a ++ b)
        f True = " + "
        f False = " - "

instance IsSize s => Show (Operand a s) where
    show = \case
        ImmOp w -> show w
        RegOp r -> show r
        r@(MemOp a) -> show (size r) ++ " [" ++ show a ++ "]"
        r@(IPMemOp x) -> show (size r) ++ " [" ++ "rel " ++ show x ++ "]"
      where
        showp x | x < 0 = " - " ++ show (-x)
        showp x = " + " ++ show x

instance Show a => Show (Immediate a) where
    show (Immediate x) = show x
    show (LabelRelValue s x) = show x

instance IsSize s => HasSize (Operand a s) where
    size _ = size (ssize :: SSize s)

instance IsSize s => HasSize (Addr s) where
    size _ = size (ssize :: SSize s)

instance IsSize s => HasSize (Address s) where
    size _ = size (ssize :: SSize s)

instance IsSize s => HasSize (BaseReg s) where
    size _ = size (ssize :: SSize s)

instance IsSize s => HasSize (Reg s) where
    size _ = size (ssize :: SSize s)

instance IsSize s => HasSize (IndexReg s) where
    size _ = size (ssize :: SSize s)

instance (rw ~ R) => Num (Operand rw s) where
    negate (ImmOp (Immediate x)) = ImmOp $ Immediate $ negate x
    fromInteger (Integral x) = ImmOp $ Immediate x
    fromInteger z = error $ show z ++ " does not fit into " -- ++ show s
    (+) = error "(+) @Operand"
    (-) = error "(-) @Operand"
    (*) = error "(*) @Operand"
    abs = error "abs @Operand"
    signum = error "signum @Operand"

instance Monoid (Addr s) where
    mempty = Addr (getFirst mempty) (getFirst mempty) mempty
    Addr a b c `mappend` Addr a' b' c' = Addr (getFirst $ First a <> First a') (getFirst $ First b <> First b') (c <> c')

instance Monoid (IndexReg s) where
    mempty = NoIndex
    i `mappend` NoIndex = i
    NoIndex `mappend` i = i

base :: Reg s -> Addr s
base x = Addr (Just x) NoDisp NoIndex

index :: Scale -> Reg s -> Addr s
index sc x = Addr Nothing NoDisp (IndexReg sc x)

index' :: Int -> Reg s -> Addr s
index' sc x = Addr Nothing NoDisp (IndexReg (toScale sc) x)

index1 = index s1
index2 = index s2
index4 = index s4
index8 = index s8

disp :: (Bits a, Integral a) => a -> Addr s
disp (Integral x)
    | x == 0 = mempty
    | otherwise = Addr Nothing (Disp x) NoIndex

data Address :: Size -> * where
    Address :: [(Int, Reg s)] -> Int -> Address s

scaleAddress :: (Int -> Int) -> Address s -> Address s
scaleAddress f (Address rs d) = Address (first f <$> rs) $ f d

instance Num (Address s) where
    fromInteger d = Address [] $ fromInteger d
    negate = scaleAddress negate
    Address [] t * a = scaleAddress (t*) a
    a * Address [] t = scaleAddress (t*) a
    Address rs d + Address rs' d' = Address (f rs rs') (d + d') where
        f [] rs = rs
        f rs [] = rs
        f (p@(t, r): rs) (p'@(t', r'): rs') = case compare r r' of
            LT -> p: f rs (p': rs')
            GT -> p': f (p: rs) rs'
            EQ | t + t' == 0 -> f rs rs'
               | otherwise   -> (t + t', r): f rs rs'
    abs = error "abs @Address"
    signum = error "signum @Address"

makeAddr :: Address s -> Addr s
makeAddr (Address [(1, r)] d) = base r <> disp d
makeAddr (Address [(t, r)] d) = index' t r <> disp d
makeAddr (Address [(1, r), (1, r'@(NormalReg 0x4))] d) = base r' <> index1 r <> disp d
makeAddr (Address [(1, r), (t, r')] d) = base r <> index' t r' <> disp d
makeAddr (Address [(t, r'), (1, r)] d) = base r <> index' t r' <> disp d

class FromReg c where
    fromReg :: Reg s -> c s

instance FromReg Reg where
    fromReg = id

instance FromReg (Operand r) where
    fromReg = RegOp

instance FromReg Address where
    fromReg r = Address [(1, r)] 0

reg = fromReg . NormalReg

rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 :: FromReg c => c S64
rax  = reg 0x0
rcx  = reg 0x1
rdx  = reg 0x2
rbx  = reg 0x3
rsp  = reg 0x4
rbp  = reg 0x5
rsi  = reg 0x6
rdi  = reg 0x7
r8   = reg 0x8
r9   = reg 0x9
r10  = reg 0xa
r11  = reg 0xb
r12  = reg 0xc
r13  = reg 0xd
r14  = reg 0xe
r15  = reg 0xf

eax, ecx, edx, ebx, esp, ebp, esi, edi, r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d :: FromReg c => c S32
eax  = reg 0x0
ecx  = reg 0x1
edx  = reg 0x2
ebx  = reg 0x3
esp  = reg 0x4
ebp  = reg 0x5
esi  = reg 0x6
edi  = reg 0x7
r8d  = reg 0x8
r9d  = reg 0x9
r10d = reg 0xa
r11d = reg 0xb
r12d = reg 0xc
r13d = reg 0xd
r14d = reg 0xe
r15d = reg 0xf

ax, cx, dx, bx, sp, bp, si, di, r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w :: FromReg c => c S16
ax   = reg 0x0
cx   = reg 0x1
dx   = reg 0x2
bx   = reg 0x3
sp   = reg 0x4
bp   = reg 0x5
si   = reg 0x6
di   = reg 0x7
r8w  = reg 0x8
r9w  = reg 0x9
r10w = reg 0xa
r11w = reg 0xb
r12w = reg 0xc
r13w = reg 0xd
r14w = reg 0xe
r15w = reg 0xf

al, cl, dl, bl, spl, bpl, sil, dil, r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b :: FromReg c => c S8
al   = reg 0x0
cl   = reg 0x1
dl   = reg 0x2
bl   = reg 0x3
spl  = reg 0x4
bpl  = reg 0x5
sil  = reg 0x6
dil  = reg 0x7
r8b  = reg 0x8
r9b  = reg 0x9
r10b = reg 0xa
r11b = reg 0xb
r12b = reg 0xc
r13b = reg 0xd
r14b = reg 0xe
r15b = reg 0xf

ah, ch, dh, bh :: FromReg c => c S8
ah   = fromReg $ HighReg 0x0
ch   = fromReg $ HighReg 0x1
dh   = fromReg $ HighReg 0x2
bh   = fromReg $ HighReg 0x3

xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7 :: FromReg c => c S128
xmm0 = fromReg $ XMM 0x0
xmm1 = fromReg $ XMM 0x1
xmm2 = fromReg $ XMM 0x2
xmm3 = fromReg $ XMM 0x3
xmm4 = fromReg $ XMM 0x4
xmm5 = fromReg $ XMM 0x5
xmm6 = fromReg $ XMM 0x6
xmm7 = fromReg $ XMM 0x7

pattern RegA = RegOp (NormalReg 0x0)

pattern RegCl :: Operand r S8
pattern RegCl = RegOp (NormalReg 0x1)

--------------------------------------------------------------

resizeOperand :: IsSize s' => Operand RW s -> Operand RW s'
resizeOperand (RegOp x) = RegOp $ resizeRegCode x
resizeOperand (MemOp a) = MemOp a
resizeOperand (IPMemOp a) = IPMemOp a

resizeRegCode :: Reg s -> Reg s'
resizeRegCode (NormalReg i) = NormalReg i

pattern MemLike <- (isMemOp -> True)

isMemOp MemOp{} = True
isMemOp IPMemOp{} = True
isMemOp _ = False

-------------------------------------------------------------- condition

newtype Condition = Condition Word8

pattern O   = Condition 0x0
pattern NO  = Condition 0x1
pattern B   = Condition 0x2
pattern C   = Condition 0x2
pattern NB  = Condition 0x3
pattern NC  = Condition 0x3
pattern E   = Condition 0x4
pattern Z   = Condition 0x4
pattern NE  = Condition 0x5
pattern NZ  = Condition 0x5
pattern NA  = Condition 0x6
pattern BE  = Condition 0x6
pattern A   = Condition 0x7
pattern NBE = Condition 0x7
pattern S   = Condition 0x8
pattern NS  = Condition 0x9
pattern P   = Condition 0xa
pattern NP  = Condition 0xb
pattern L   = Condition 0xc
pattern NL  = Condition 0xd
pattern NG  = Condition 0xe
pattern LE  = Condition 0xe
pattern G   = Condition 0xf
pattern NLE = Condition 0xf

instance Show Condition where
    show (Condition x) = case x of
        0x0 -> "o"
        0x1 -> "no"
        0x2 -> "c"
        0x3 -> "nc"
        0x4 -> "z"
        0x5 -> "nz"
        0x6 -> "be"
        0x7 -> "nbe"
        0x8 -> "s"
        0x9 -> "ns"
        0xa -> "p"
        0xb -> "np"
        0xc -> "l"
        0xd -> "nl"
        0xe -> "le"
        0xf -> "nle"

pattern N cc <- (notCond -> cc)
  where N = notCond

notCond :: Condition -> Condition
notCond (Condition c) = Condition $ c `xor` 1

-------------------------------------------------------------- asm code lines

data CodeLine where
    Ret_, Nop_, PushF_, PopF_, Cmc_, Clc_, Stc_, Cli_, Sti_, Cld_, Std_   :: CodeLine

    Inc_, Dec_, Not_, Neg_, Bswap                               :: IsSize s => Operand RW s -> CodeLine
    Add_, Or_, Adc_, Sbb_, And_, Sub_, Xor_, Cmp_, Test_, Mov_, Bsf, Bsr :: IsSize s => Operand RW s -> Operand r s -> CodeLine
    Rol_, Ror_, Rcl_, Rcr_, Shl_, Shr_, Sar_                 :: IsSize s => Operand RW s -> Operand r S8 -> CodeLine
    Bt :: IsSize s => Operand r s -> Operand RW s -> CodeLine

    Movdqa_, Paddb_, Paddw_, Paddd_, Paddq_, Psubb_, Psubw_, Psubd_, Psubq_, Pxor_ :: Operand RW S128 -> Operand r S128 -> CodeLine
    Psllw_, Pslld_, Psllq_, Pslldq_, Psrlw_, Psrld_, Psrlq_, Psrldq_, Psraw_, Psrad_ :: Operand RW S128 -> Operand r S8 -> CodeLine
    Movd_, Movq_ :: (IsSize s, IsSize s') => Operand RW s -> Operand r s' -> CodeLine

    Cmov_ :: IsSize s => Condition -> Operand RW s -> Operand RW s -> CodeLine
    Xchg_ :: IsSize s => Operand RW s -> Operand RW s -> CodeLine
    Lea_  :: (IsSize s, IsSize s') => Operand RW s -> Operand RW s' -> CodeLine

    Pop_  :: Operand RW S64 -> CodeLine
    Push_ :: Operand r  S64 -> CodeLine

    Call_ :: Operand r S64 -> CodeLine
    Jmpq_ :: Operand r S64 -> CodeLine

    J_    :: Condition -> Maybe Size -> Label -> CodeLine
    Jmp_  :: Maybe Size -> Label -> CodeLine

    Label_ :: CodeLine

    Data_  :: Bytes -> CodeLine
    Align_ :: Int   -> CodeLine

------------------------- show code lines

newLabel = do
    i <- get
    put $ i + 1
    return $ Label i

codeLine x = tell [x]

showOp0 s = codeLine s
showOp s a = showOp0 $ s ++ " " ++ a
showOp1 s a = showOp s $ show a
showOp2 s a b = showOp s $ show a ++ ", " ++ show b

showCodeLine :: CodeLine -> StateT Int (Writer [String]) ()
showCodeLine = \case
    Add_  op1 op2 -> showOp2 "add"  op1 op2
    Or_   op1 op2 -> showOp2 "or"   op1 op2
    Adc_  op1 op2 -> showOp2 "adc"  op1 op2
    Sbb_  op1 op2 -> showOp2 "sbb"  op1 op2
    And_  op1 op2 -> showOp2 "and"  op1 op2
    Sub_  op1 op2 -> showOp2 "sub"  op1 op2
    Xor_  op1 op2 -> showOp2 "xor"  op1 op2
    Cmp_  op1 op2 -> showOp2 "cmp"  op1 op2
    Test_ op1 op2 -> showOp2 "test" op1 op2
    Bsf   op1 op2 -> showOp2 "bsf"  op1 op2
    Bsr   op1 op2 -> showOp2 "bsr"  op1 op2
    Bt    op1 op2 -> showOp2 "bt"   op1 op2
    Rol_  op1 op2 -> showOp2 "rol"  op1 op2
    Ror_  op1 op2 -> showOp2 "ror"  op1 op2
    Rcl_  op1 op2 -> showOp2 "rcl"  op1 op2
    Rcr_  op1 op2 -> showOp2 "rcr"  op1 op2
    Shl_  op1 op2 -> showOp2 "shl"  op1 op2
    Shr_  op1 op2 -> showOp2 "shr"  op1 op2
    Sar_  op1 op2 -> showOp2 "sar"  op1 op2
    Mov_  op1 op2 -> showOp2 "mov"  op1 op2
    Cmov_ cc op1 op2 -> showOp2 ("cmov" ++ show cc) op1 op2
    Lea_  op1 op2 -> showOp2 "lea"  op1 op2
    Xchg_ op1 op2 -> showOp2 "xchg" op1 op2
    Movd_   op1 op2 -> showOp2 "movd"   op1 op2
    Movq_   op1 op2 -> showOp2 "movq"   op1 op2
    Movdqa_ op1 op2 -> showOp2 "movdqa" op1 op2
    Paddb_  op1 op2 -> showOp2 "paddb"  op1 op2
    Paddw_  op1 op2 -> showOp2 "paddw"  op1 op2
    Paddd_  op1 op2 -> showOp2 "paddd"  op1 op2
    Paddq_  op1 op2 -> showOp2 "paddq"  op1 op2
    Psubb_  op1 op2 -> showOp2 "psubb"  op1 op2
    Psubw_  op1 op2 -> showOp2 "psubw"  op1 op2
    Psubd_  op1 op2 -> showOp2 "psubd"  op1 op2
    Psubq_  op1 op2 -> showOp2 "psubq"  op1 op2
    Pxor_   op1 op2 -> showOp2 "pxor"   op1 op2
    Psllw_  op1 op2 -> showOp2 "psllw"  op1 op2
    Pslld_  op1 op2 -> showOp2 "pslld"  op1 op2
    Psllq_  op1 op2 -> showOp2 "psllq"  op1 op2
    Pslldq_ op1 op2 -> showOp2 "pslldq" op1 op2
    Psrlw_  op1 op2 -> showOp2 "psrlw"  op1 op2
    Psrld_  op1 op2 -> showOp2 "psrld"  op1 op2
    Psrlq_  op1 op2 -> showOp2 "psrlq"  op1 op2
    Psrldq_ op1 op2 -> showOp2 "psrldq" op1 op2
    Psraw_  op1 op2 -> showOp2 "psraw"  op1 op2
    Psrad_  op1 op2 -> showOp2 "psrad"  op1 op2
    Inc_  op -> showOp1 "inc"  op
    Dec_  op -> showOp1 "dec"  op
    Not_  op -> showOp1 "not"  op
    Neg_  op -> showOp1 "neg"  op
    Bswap op -> showOp1 "bswap" op
    Pop_  op -> showOp1 "pop"  op
    Push_ op -> showOp1 "push" op
    Call_ op -> showOp1 "call" op
    Jmpq_ op -> showOp1 "jmp"  op
    Ret_   -> showOp0 "ret"
    Nop_   -> showOp0 "nop"
    PushF_ -> showOp0 "pushf"
    PopF_  -> showOp0 "popf"
    Cmc_   -> showOp0 "cmc"
    Clc_   -> showOp0 "clc"
    Stc_   -> showOp0 "stc"
    Cli_   -> showOp0 "cli"
    Sti_   -> showOp0 "sti"
    Cld_   -> showOp0 "cld"
    Std_   -> showOp0 "std"

    Align_ s -> codeLine $ ".align " ++ show s
    Data_ x
        | 2 * length (filter isPrint x) > length x -> showOp "db" $ show (toEnum . fromIntegral <$> x :: String)
        | otherwise -> showOp "db" $ intercalate ", " (show <$> x)
      where
        isPrint c = c >= 32 && c <= 126

    J_ cc s l -> showOp ("j" ++ show cc) $ (case s of Just S8 -> "short "; Just S32 -> "near "; _ -> "") ++ show l
    Jmp_ s  l -> showOp "jmp" $ (case s of Just S8 -> "short "; Just S32 -> "near "; _ -> "") ++ show l
    Label_    -> newLabel >>= codeLine . show

