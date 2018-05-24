{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reducer.LLVM.JIT where

import Grin
import Data.String

import LLVM.Target
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST as AST

import LLVM.OrcJIT

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

import Data.Int
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int64) -> IO Int64

foreign import ccall "wrapper"
  wrapIntPrint :: (Int64 -> IO Int64) -> IO (FunPtr (Int64 -> IO Int64))

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

myIntPrintImpl :: Int64 -> IO Int64
myIntPrintImpl i = print i >> pure i

resolver :: CompileLayer l => MangledSymbol -> l -> MangledSymbol -> IO JITSymbol
resolver intPrint compileLayer symbol
  | symbol == intPrint = do
      funPtr <- wrapIntPrint myIntPrintImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      return (JITSymbol addr (JITSymbolFlags False True))
  | otherwise = findSymbol compileLayer symbol True

nullResolver :: MangledSymbol -> IO JITSymbol
nullResolver s = return (JITSymbol 0 (JITSymbolFlags False False))

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail return <=< runExceptT

grinHeapSize :: Int
grinHeapSize = 100 * 1024 * 1024

eagerJit :: AST.Module -> String -> IO Grin.Val
eagerJit amod mainName =
    withTestModule amod $ \mod ->
      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            intPrint <- mangleSymbol compileLayer "_prim_int_print"
            withModule
              compileLayer
              mod
              (SymbolResolver (resolver intPrint compileLayer) nullResolver) $
              \moduleSet -> do
                mainSymbol <- mangleSymbol compileLayer (fromString mainName)
                JITSymbol mainFn _ <- findSymbol compileLayer mainSymbol True
                heapSymbol <- mangleSymbol compileLayer (fromString "_heap_ptr_")
                JITSymbol heapWordPtr _ <- findSymbol compileLayer heapSymbol True
                -- allocate GRIN heap
                heapPointer <- callocBytes grinHeapSize :: IO (Ptr Int8)
                poke (wordPtrToPtr heapWordPtr :: Ptr Int64) (fromIntegral $ minusPtr heapPointer nullPtr)
                -- run function
                result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                -- TODO: read back the result and build the haskell value represenation
                -- free GRIN heap
                free heapPointer
                return $ Unit
