{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module JITLLVM where

import LLVM.Target
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST as AST

import LLVM.OrcJIT

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

import Data.Int
import Foreign.Ptr

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int64) -> IO Int64

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

resolver :: MangledSymbol -> IRCompileLayer l -> MangledSymbol -> IO JITSymbol
resolver testFunc compileLayer symbol
  = findSymbol compileLayer symbol True

nullResolver :: MangledSymbol -> IO JITSymbol
nullResolver s = return (JITSymbol 0 (JITSymbolFlags False False))

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail return <=< runExceptT

eagerJit :: AST.Module -> IO Int64
eagerJit amod =
    withTestModule amod $ \mod ->
      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            asm <- moduleLLVMAssembly mod
            --BS.putStrLn asm
            testFunc <- mangleSymbol compileLayer "grinMain"
            withModule
              compileLayer
              mod
              (SymbolResolver (resolver testFunc compileLayer) nullResolver) $
              \moduleSet -> do
                mainSymbol <- mangleSymbol compileLayer "grinMain"
                JITSymbol mainFn _ <- findSymbol compileLayer mainSymbol True
                result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                return result
