{-# language CPP #-}
{-# language BangPatterns #-}
{-# language NoMonomorphismRestriction #-}
{-# language ScopedTypeVariables #-}
{-# language DataKinds #-}
{-# language ForeignFunctionInterface #-}
{-# language RecursiveDo #-}
module CodeGen.X86.Utils where

import Data.Char
import Data.Monoid
import Control.Monad
import Foreign
import System.Environment
import Debug.Trace

import CodeGen.X86.Asm
import CodeGen.X86.CodeGen
import CodeGen.X86.CallConv

-------------------------------------------------------------- derived constructs

-- | execute code unless condition is true
unless cc x = mdo
    j cc l
    x
    l <- label
    return ()

-- | do while loop construction
doWhile cc x = do
    l <- label
    x
    j cc l

-- | if-then-else
if_ cc a b = mdo
    j (N cc) l1
    a
    jmp l2
    l1 <- label
    b
    l2 <- label
    return ()

leaData r d = mdo
    lea r $ ipRel8 l1
    jmp l2
    l1 <- label
    db $ toBytes d
    l2 <- label
    return ()

------------------------------------------------------------------------------ 

foreign import ccall "static stdio.h &printf" printf :: FunPtr a

------------------------------------------------------------------------------ 
-- * utils

mov' :: forall s s' r . IsSize s' => Operand RW s -> Operand r s' -> Code
mov' a b = mov (resizeOperand a :: Operand RW s') b

newtype CString = CString String

instance HasBytes CString where
    toBytes (CString cs) = mconcat $ toBytes . (fromIntegral :: Int -> Word8) . fromEnum <$> (cs ++ "\0")

-- | we should implement PUSHA and POPA later
all_regs_except_rsp :: [Operand rw S64]
all_regs_except_rsp = [ rax, rcx, rdx, rbx, {- rsp, -} rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15 ]

push_all = sequence_ [ push r | r <-         all_regs_except_rsp ]
pop_all  = sequence_ [ pop  r | r <- reverse all_regs_except_rsp ]

traceReg :: IsSize s => String -> Operand RW s -> Code
traceReg d r = do
    pushf
    push_all
    mov' arg2 r
    leaData arg1 (CString $ show r ++ " = %" ++ s ++ d ++ "\n")
    xor_ rax rax
    callFun r11 printf
    pop_all
    popf
  where
    s = case size r of
        S8  -> "hh"
        S16 -> "h"
        S32 -> ""
        S64 -> "l"

