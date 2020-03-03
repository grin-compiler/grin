{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Reducer.LLVM.JIT where

import Grin.Grin (Val(..))
import Reducer.Base (RTVal(..))
import Data.String

import LLVM.Target
import LLVM.Context
import LLVM.Module
import qualified LLVM.AST as AST

import LLVM.OrcJIT
import qualified LLVM.Internal.OrcJIT.CompileLayer as CL

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import System.Exit

import Data.Int
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import qualified Data.Map.Strict as Map

foreign import ccall "dynamic"
  mkMain :: FunPtr (IO Int64) -> IO Int64

foreign import ccall "wrapper"
  wrapIntPrint :: (Int64 -> IO ()) -> IO (FunPtr (Int64 -> IO ()))

foreign import ccall "wrapper"
  wrapRuntimeError :: (Int64 -> IO ()) -> IO (FunPtr (Int64 -> IO ()))

withTestModule :: AST.Module -> (LLVM.Module.Module -> IO a) -> IO a
withTestModule mod f = withContext $ \context -> withModuleFromAST context mod f

myIntPrintImpl :: Int64 -> IO ()
myIntPrintImpl i = print i

myRuntimeErrorImpl :: Int64 -> IO ()
myRuntimeErrorImpl i = exitWith $ ExitFailure (fromIntegral i)

resolver :: CompileLayer l => MangledSymbol -> MangledSymbol -> l -> MangledSymbol -> IO (Either JITSymbolError JITSymbol)
resolver intPrint runtimeError compileLayer symbol
  | symbol == intPrint = do
      funPtr <- wrapIntPrint myIntPrintImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      pure $ Right (JITSymbol addr defaultJITSymbolFlags)
  | symbol == runtimeError = do
      funPtr <- wrapIntPrint myRuntimeErrorImpl
      let addr = ptrToWordPtr (castFunPtrToPtr funPtr)
      pure $ Right (JITSymbol addr defaultJITSymbolFlags)
  | otherwise = CL.findSymbol compileLayer symbol True

nullResolver :: MangledSymbol -> IO (Either JITSymbolError JITSymbol)
nullResolver s = putStrLn "nullresolver" >> pure (Left (JITSymbolError "unknown symbol"))

failInIO :: ExceptT String IO a -> IO a
failInIO = either fail pure <=< runExceptT

grinHeapSize :: Int
grinHeapSize = 100 * 1024 * 1024

-- IMPORTANT: JIT does not support FFI yet, only _prim_int_print and __runtime_error are hardwired
eagerJit :: AST.Module -> String -> IO RTVal
eagerJit amod mainName = do
  resolvers <- newIORef Map.empty
  withTestModule amod $ \mod ->
    withHostTargetMachine $ \tm ->
    withExecutionSession $ \es ->
    withObjectLinkingLayer es (\k -> fmap (\rs -> rs Map.! k) (readIORef resolvers)) $ \linkingLayer ->
    withIRCompileLayer linkingLayer tm $ \compileLayer -> do
      intPrint <- mangleSymbol compileLayer "_prim_int_print"
      runtimeError <- mangleSymbol compileLayer "__runtime_error"
      withModuleKey es $ \k ->
        withSymbolResolver es (SymbolResolver (resolver intPrint runtimeError compileLayer)) $ \resolver -> do
          modifyIORef' resolvers (Map.insert k resolver)
          withModule compileLayer k mod $ do
            mainSymbol <- mangleSymbol compileLayer (fromString mainName)
            Right (JITSymbol mainFn _) <- CL.findSymbol compileLayer mainSymbol True
            heapSymbol <- mangleSymbol compileLayer (fromString "_heap_ptr_")
            Right (JITSymbol heapWordPtr _) <- CL.findSymbol compileLayer heapSymbol True
            -- allocate GRIN heap
            heapPointer <- callocBytes grinHeapSize :: IO (Ptr Int8)
            poke (wordPtrToPtr heapWordPtr :: Ptr Int64) (fromIntegral $ minusPtr heapPointer nullPtr)
            -- run function
            result <- mkMain (castPtrToFunPtr (wordPtrToPtr mainFn))
            -- TODO: read back the result and build the haskell value represenation
            -- free GRIN heap
            free heapPointer
            pure RT_Unit
