{-# LANGUAGE FlexibleInstances #-}
module CodeGen.X86.Examples where

import Foreign

import CodeGen.X86

foreign import ccall "dynamic" callWW :: FunPtr (Word64 -> Word64) -> Word64 -> Word64
instance Callable (Word64 -> Word64) where dynCCall = callWW

foreign import ccall "dynamic" callPW :: FunPtr (Ptr a -> Word64) -> Ptr a -> Word64
instance Callable (Ptr a -> Word64) where dynCCall = callPW

foreign import ccall "dynamic" callIO :: FunPtr (IO ()) -> IO ()
instance Callable (IO ()) where dynCCall = callIO

foreign import ccall "wrapper" createPtrWord64_Word64 :: (Word64 -> Word64) -> IO (FunPtr (Word64 -> Word64))
instance CallableHs (Word64 -> Word64) where createHsPtr = createPtrWord64_Word64

------------------------------------------------------------------------------ 
-- * examples

-- | Example: identity function in Assembly (look at the source code)
--
-- Input: @rdi@ on Linux \/ System V, @rcx@ on Win64
--
-- Output: @rax@
idCode = do
    mov result arg1
    ret

idFun :: Word64 -> Word64
idFun = compile idCode 

-- | Example: Fibonacci function in Assembly
fibCode = saveNonVolatile $ do
    mov rdi arg1
    inc rdi
    xor_ rdx rdx
    mov rax 1
    doWhile NZ $ do
        mov rcx rax
        mov rax rdx
        add rdx rcx
        dec rdi

fibFun :: Word64 -> Word64
fibFun = compile fibCode 

-- | Example: trace a register in Assembly
tracedFibCode = saveNonVolatile $ do
    mov rdi arg1
    inc rdi
    xor_ rdx rdx
    mov rax 1
    doWhile NZ $ do
        mov rcx rax
        mov rax rdx
        add rdx rcx
        dec rdi
        traceReg "d" rax

tracedFibFun :: Word64 -> Word64
tracedFibFun = compile tracedFibCode 

-- | Example: call Haskell @fib@ function from Assembly
callHsCode = do
    callFun r11 (hsPtr fib)
    ret

fib :: Word64 -> Word64
fib n = go n 0 1
  where
    go 0 a b = b `seq` a
    go n a b = go (n-1) b (a+b)

callHsFun :: Word64 -> Word64
callHsFun = compile callHsCode 

-- | Example: call C @printf@ function from Assembly
--
callCCode name = saveNonVolatile $ do
    leaData arg1 $ CString "Hello %s!\n"
    leaData arg2 $ CString name
    xor_ rax rax                     -- zero XMM arguments ?????
    callFun r11 printf

callCFun :: String -> IO ()
callCFun name = compile $ callCCode name

-------------------------------------------------------

memTestFun :: Word64 -> IO Bool
memTestFun v = do
    r <- mallocBytes 8                -- this is not required to be aligned (and in any case malloc aligns to machine words)
    pokeByteOff r 0 (v :: Word64)
    let code = saveNonVolatile $ do
            mov rdi arg1
            mov rax (addr rdi)
    return $ compile code (r :: Ptr Word8) == v


