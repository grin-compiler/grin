-- | Calling conventions. There are basically only two: System V (Linux, OSX, BSD) and Win64\/fastcall

{-# language NoMonomorphismRestriction #-}
{-# language CPP #-}
{-# language BangPatterns #-}
{-# language DataKinds #-}
module CodeGen.X86.CallConv where

import Foreign
import Data.Monoid

import CodeGen.X86.Asm
import CodeGen.X86.CodeGen

------------------------------------------------------------------------------ 

#if defined (mingw32_HOST_OS) || defined (mingw64_HOST_OS)

-- On Win64 the caller have to reserve 32 byte "shadow space" on the stack (and clean up after)
callFun :: Operand RW S64 -> FunPtr a -> Code
callFun r p = do
    sub rsp 32
    mov r (fromIntegral $ ptrToIntPtr $ castFunPtrToPtr p)
    call r
    add rsp 32

#elif defined (darwin_HOST_OS)

-- OSX requires 16 byte alignment of the stack...
callFun :: Operand RW S64 -> FunPtr a -> Code
callFun r p = do
    push r15              -- we will use r15 (non-volatile) to store old rsp
    mov r15 15            -- 0xf
    not_ r15              -- 0xffff ... fff0
    and_ r15 rsp          -- align rsp into r15
    xchg r15 rsp          -- new rsp = aligned, r15 = old rsp
    mov r (fromIntegral $ ptrToIntPtr $ castFunPtrToPtr p)
    call r
    mov rsp r15           -- restore rsp
    pop r15               -- restore r15

#else

-- helper to call a function
callFun :: Operand RW S64 -> FunPtr a -> Code
callFun r p = do
    mov r $ fromIntegral $ ptrToIntPtr $ castFunPtrToPtr p
    call r

#endif

------------------------------------------------------------------------------ 

-- | Save the non-volatile registers, execute the code, restore the registers 
-- and return after.
--
-- Note: R12..R15 should be preserved on both Windows and Linux (or 
-- System V convention in general). This is the responsability of the 
-- user (this function won't save them, but you can use "saveR12R15" in 
-- addition to this).
--
saveNonVolatile :: Code -> Code
saveNonVolatile code = prologue >> code >> epilogue >> ret

-- | Saves R12, R13, R14 and R15 (on the stack).
saveR12R15 :: Code -> Code
saveR12R15 code = do
  push r12
  push r13
  push r14
  push r15
  code
  pop r15
  pop r14
  pop r13
  pop r12

------------------------------------------------------------------------------ 
-- calling conventions

#if defined (mingw32_HOST_OS) || defined (mingw64_HOST_OS)

---------- Win64 calling convention ----------

arg1 = rcx
arg2 = rdx
arg3 = r8
arg4 = r9
-- rest of the arguments on the stack

result = rax

prologue = do
    push rbp
    push rbx
    push rdi
    push rsi

epilogue = do
    pop rsi
    pop rdi
    pop rbx
    pop rbp

#else

---------- System V calling convention ----------

arg1 = rdi
arg2 = rsi
arg3 = rdx
arg4 = rcx
arg5 = r8
arg6 = r9
-- rest of the arguments on the stack

result = rax

prologue = do
    push rbp
    push rbx

epilogue = do
    pop rbx
    pop rbp

#endif

------------------------------------------------------------------------------ 

