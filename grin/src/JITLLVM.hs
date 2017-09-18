{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module JITLLVM where

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

eagerJit :: AST.Module -> String -> IO Grin.Val
eagerJit amod mainName =
    withTestModule amod $ \mod ->
      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            intPrint <- mangleSymbol compileLayer "intPrint"
            withModule
              compileLayer
              mod
              (SymbolResolver (resolver intPrint compileLayer) nullResolver) $
              \moduleSet -> do
                mainSymbol <- mangleSymbol compileLayer (fromString mainName)
                JITSymbol mainFn _ <- findSymbol compileLayer mainSymbol True
                result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
                return $ Unit
