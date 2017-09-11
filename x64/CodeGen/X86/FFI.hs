{-# language CPP #-}
{-# language ForeignFunctionInterface #-}
{-# language BangPatterns #-}
{-# language ViewPatterns #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language TypeFamilies #-}
module CodeGen.X86.FFI where

-------------------------------------------------------

import Control.Monad
import Control.Exception (evaluate)
import Control.DeepSeq
import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import System.IO.Unsafe

import Control.DeeperSeq
import CodeGen.X86.Asm
import CodeGen.X86.CodeGen

-------------------------------------------------------

-- this should be queried from the OS dynamically...
#define PAGE_SIZE 4096

#if defined (mingw32_HOST_OS) || defined (mingw64_HOST_OS) 

import System.Win32.Types
import System.Win32.Mem
import Foreign.Marshal.Alloc

#endif

class (MapResult a, NFData (Result a)) => Callable a where dynCCall :: FunPtr a -> a

{-# NOINLINE callForeignPtr #-}
callForeignPtr :: Callable a => IO (ForeignPtr a) -> a
callForeignPtr p_ = mapResult f (dynCCall $ cast $ unsafeForeignPtrToPtr p)
  where
    cast = castPtrToFunPtr :: Ptr a -> FunPtr a
    p = unsafePerformIO p_
    {-# NOINLINE f #-}
    f x = unsafePerformIO $ evaluate (force x) <* touchForeignPtr p

class MapResult a => CallableHs a where createHsPtr :: a -> IO (FunPtr a)

hsPtr :: CallableHs a => a -> FunPtr a
hsPtr x = unsafePerformIO $ createHsPtr x
{- TODO
hsPtr' :: CallableHs a => a -> ForeignPtr a
hsPtr' x = unsafePerformIO $ do
    p <- createHsPtr x
    newForeignPtr (freeHaskellFunPtr p) (castFunPtrToPtr p)
-}

#if defined (mingw32_HOST_OS) || defined (mingw64_HOST_OS) 
-- note: GHC 64 bit also defines mingw32 ...

foreign import ccall "static malloc.h  _aligned_malloc" c_aligned_malloc :: CSize -> CSize -> IO (Ptr a)
foreign import ccall "static malloc.h  _aligned_free"   c_aligned_free   :: Ptr a -> IO ()
foreign import ccall "static malloc.h &_aligned_free"   ptr_aligned_free :: FunPtr (Ptr a -> IO ())

#elif defined (linux_HOST_OS)

-- on Linux too, we should use posix_memalign...
foreign import ccall "static stdlib.h memalign"   memalign :: CSize -> CSize -> IO (Ptr a)
foreign import ccall "static stdlib.h &free"      stdfree  :: FunPtr (Ptr a -> IO ())
foreign import ccall "static sys/mman.h mprotect" mprotect :: Ptr a -> CSize -> CInt -> IO CInt

#elif defined (darwin_HOST_OS) || defined (freebsd_HOST_OS) || defined (openbsd_HOST_OS) || defined (netbsd_HOST_OS) 

foreign import ccall "static stdlib.h posix_memalign"   posix_memalign :: Ptr (Ptr a) -> CSize -> CSize -> IO CInt
foreign import ccall "static stdlib.h &free"            stdfree        :: FunPtr (Ptr a -> IO ())
foreign import ccall "static sys/mman.h mprotect"       mprotect       :: Ptr a -> CSize -> CInt -> IO CInt

#endif

-------------------------------------------------------

#if defined (mingw32_HOST_OS) || defined (mingw64_HOST_OS) 

flag_PAGE_EXECUTE_READWRITE :: Word32
flag_PAGE_EXECUTE_READWRITE = 0x40 

{-# NOINLINE compile #-}
compile :: Callable a => Code -> a
compile x = callForeignPtr $ do
    let (bytes, size) = buildTheCode x
    arr <- c_aligned_malloc (fromIntegral size) PAGE_SIZE
    _ <- virtualProtect (castPtr arr) (fromIntegral size) flag_PAGE_EXECUTE_READWRITE
    forM_ [p | Right p <- bytes] $ uncurry $ pokeByteOff arr    
    newForeignPtr ptr_aligned_free arr 

#elif defined linux_HOST_OS

{-# NOINLINE compile #-}
compile :: Callable a => Code -> a
compile x = callForeignPtr $ do
    let (bytes, fromIntegral -> size) = buildTheCode x
    arr <- memalign PAGE_SIZE size
    _ <- mprotect arr size 0x7 -- READ, WRITE, EXEC
    forM_ [p | Right p <- bytes] $ uncurry $ pokeByteOff arr
    newForeignPtr stdfree arr

#elif defined (darwin_HOST_OS) || defined (freebsd_HOST_OS) || defined (openbsd_HOST_OS) || defined (netbsd_HOST_OS) 

-- | This calls @posix_memalign()@
posixMemAlign 
  :: CSize               -- ^ alignment
  -> CSize               -- ^ size
  -> IO (Ptr a)
posixMemAlign alignment size0 =
  alloca $ \pp -> do
    let a    = max alignment 8
        size = mod (size0 + a - 1) a      -- size *must* be a multiple of both alignment and sizeof(void*)
    res <- posix_memalign pp alignment size
    case res of
      0 -> peek pp
      _ -> error "posix_memalign failed"
      
{-# NOINLINE compile #-}
compile :: Callable a => Code -> a
compile x = callForeignPtr $ do
    let (bytes, fromIntegral -> size) = buildTheCode x
    arr <- posixMemAlign PAGE_SIZE size
    _ <- mprotect arr size 0x7 -- READ, WRITE, EXEC
    forM_ [p | Right p <- bytes] $ uncurry $ pokeByteOff arr
    newForeignPtr stdfree arr

#endif

