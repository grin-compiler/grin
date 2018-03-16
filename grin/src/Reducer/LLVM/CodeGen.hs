{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings #-}

module Reducer.LLVM.CodeGen
  ( codeGen
  , toLLVM
  ) where

import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable
import Lens.Micro.Platform

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List (unzip4)

import LLVM.AST hiding (callingConvention, functionAttributes)
import LLVM.AST.Type as LLVM
import qualified LLVM.AST.Typed as LLVM
import LLVM.AST.Constant as C hiding (Add, ICmp)
import LLVM.AST.IntegerPredicate
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FunctionAttribute as FA
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

import Grin
import Pretty
import TypeEnv hiding (Type, typeOfVal)
import qualified TypeEnv
import Reducer.LLVM.Base
import Reducer.LLVM.PrimOps
import Reducer.LLVM.TypeGen
import Reducer.LLVM.InferType

toLLVM :: String -> AST.Module -> IO BS.ByteString
toLLVM fname mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.writeFile fname llvm
  pure llvm

codeGenLit :: Lit -> C.Constant
codeGenLit = \case
  LInt64 v  -> Int {integerBits=64, integerValue=fromIntegral v}
  LWord64 v -> Int {integerBits=64, integerValue=fromIntegral v}
  LFloat v  -> C.Float {floatValue=F.Single v}
  LBool v   -> Int {integerBits=1, integerValue=if v then 1 else 0}

codeGenVal :: Val -> CG Operand
codeGenVal val = case val of
  -- TODO: var tag node support
  ConstTagNode tag args -> do
    opTag <- ConstantOperand <$> getTagId tag
    opArgs <- mapM codeGenVal args

    T_NodeSet ns <- typeOfVal val
    ty <- typeOfVal val
    let cgTy = toCGType ty
        TaggedUnion{..} = cgTaggedUnion cgTy
        -- set node items
        build agg (item, TUIndex{..}) = do
          codeGenLocalVar "node" (cgLLVMType cgTy) $ AST.InsertValue
            { aggregate = agg
            , element   = item
            , indices'  = [1 + tuStructIndex, tuArrayIndex]
            , metadata  = []
            }

        -- set tag
    agg0 <- codeGenLocalVar "node" (cgLLVMType cgTy) $ AST.InsertValue
            { aggregate = undef tuLLVMType
            , element   = opTag
            , indices'  = [0]
            , metadata  = []
            }
    foldM build agg0 $ zip opArgs $ V.toList $ Map.findWithDefault undefined tag tuMapping

  ValTag tag  -> ConstantOperand <$> getTagId tag
  Unit        -> pure unit
  Lit lit     -> pure . ConstantOperand . codeGenLit $ lit
  Var name    -> do
                  Map.lookup name <$> gets _constantMap >>= \case
                      -- QUESTION: what is this?
                      Nothing -> do
                                  ty <- getVarType name
                                  pure $ LocalReference (cgLLVMType ty) (mkName name)
                      Just operand  -> pure operand

  _ -> error $ printf "codeGenVal: %s" (show $ pretty val)

getCPatConstant :: CPat -> CG Constant
getCPatConstant = \case
  TagPat  tag       -> getTagId tag
  LitPat  lit       -> pure $ codeGenLit lit
  NodePat tag args  -> getTagId tag

getCPatName :: CPat -> String
getCPatName = \case
  TagPat  tag   -> tagName tag
  LitPat  lit   -> case lit of
    LInt64 v  -> "int_" ++ show v
    LWord64 v -> "word_" ++ show v
    LBool v   -> "bool_" ++ show v
    LFloat v  -> error "pattern match on float is not supported"
  NodePat tag _ -> tagName tag
 where
  tagName (Tag c name) = printf "%s%s" (show c) name

-- https://stackoverflow.com/questions/6374355/llvm-assembly-assign-integer-constant-to-register
{-
  NOTE: if the cata result is a monad then it means the codegen is sequential

  IDEA: write an untyped codegen ; which does not rely on HPT calculated types
-}

toModule :: Env -> AST.Module
toModule Env{..} = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = reverse _envDefinitions
  }

{-
  type of:
    ok - SApp        Name [SimpleVal]
    ok - SReturn     Val
    ?? - SStore      Val
    ?? - SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
    ok - SUpdate     Name Val
-}
codeGen :: TypeEnv -> Exp -> AST.Module
codeGen typeEnv = toModule . flip execState (emptyEnv {_envTypeEnv = typeEnv}) . para folder where
  folder :: ExpF (Exp, CG Result) -> CG Result
  folder = \case
    SReturnF val -> do
      ty <- typeOfVal val
      O (toCGType ty) <$> codeGenVal val
    SBlockF a -> snd $ a

    EBindF (leftExp, leftResultM) lpat (_,rightResultM) -> do
      leftResult <- leftResultM
      case lpat of
          VarTagNode{} -> error $ printf "TODO: codegen not implemented %s" (show $ pretty lpat)
          ConstTagNode tag args -> do
            (cgTy,operand) <- getOperand "node" leftResult
            let mapping = tuMapping $ cgTaggedUnion cgTy
            -- bind node pattern variables
            forM_ (zip (V.toList $ Map.findWithDefault undefined tag mapping) args) $ \(TUIndex{..}, arg) -> case arg of
              Var argName -> do
                let indices = [1 + tuStructIndex, tuArrayIndex]
                emit [(mkName argName) := AST.ExtractValue {aggregate = operand, indices' = indices, metadata = []}]
              _ -> pure ()
          Var name -> do
            getOperand name leftResult >>= addConstant name . snd
            case leftExp of
              -- NOTE: increase the heap ponter where the location information is avalable ; weird solution
              SStore{}  -> codeGenIncreaseHeapPointer name
              _         -> pure ()

          _ -> getOperand "tmp" leftResult >> pure ()
      rightResultM

    SAppF name args -> do
      (retType, argTypes) <- getFunctionType name
      operands <- mapM codeGenVal args
      operandsTypes <- mapM (\x -> toCGType <$> typeOfVal x) args
      -- convert values to function argument type
      convertedArgs <- sequence $ zipWith3 codeGenValueConversion operandsTypes operands argTypes
      if isPrimName name
        then codeGenPrimOp name args convertedArgs
        else do
          -- call to top level functions
          let wrappedResultType = withHeapPointer $ cgLLVMType retType -- add the returning heap pointer
              functionType      = FunctionType
                { resultType    = wrappedResultType
                , argumentTypes = locationLLVMType : map cgLLVMType argTypes
                , isVarArg      = False
                }
          -- HINT: pass heap pointer as the first argument
          heapPointer <- gets _envHeapPointer
          -- HINT: wrapped result = heap pointer + function result
          wrappedResult <- codeGenLocalVar (printf "%s_result" name) wrappedResultType $ Call
            { tailCallKind        = Just Tail
            , callingConvention   = CC.C
            , returnAttributes    = []
            , function            = Right . ConstantOperand $ GlobalReference (ptr functionType) (mkName name)
            , arguments           = zip (heapPointer : convertedArgs) (repeat [])
            , functionAttributes  = []
            , metadata            = []
            }
          -- extract the new heap pointer
          newHeapPointer <- codeGenLocalVar heapPointerName locationLLVMType $ AST.ExtractValue
            { aggregate = wrappedResult
            , indices'  = [0]
            , metadata  = []
            }
          modify' $ \env -> env {_envHeapPointer = newHeapPointer}
          -- extract the  real result from the wrapped result
          pure . I retType $ AST.ExtractValue
            { aggregate = wrappedResult
            , indices'  = [1]
            , metadata  = []
            }

    AltF _ a -> snd a

    ECaseF val alts -> typeOfVal val >>= \case -- distinct implementation for tagged unions and simple types
      T_SimpleType{} -> do
        opVal <- codeGenVal val
        codeGenCase opVal alts $ \_ -> pure ()

      T_NodeSet nodeSet -> do
        tuVal <- codeGenVal val
        tagVal <- codeGenExtractTag tuVal
        let valTU = taggedUnion nodeSet
        codeGenCase tagVal alts $ \case
          NodePat tag args -> case Map.lookup tag $ tuMapping valTU of
            Nothing -> pure ()
            Just mapping -> do
            --let mapping = Map.findWithDefault undefined tag $ tuMapping valTU
              -- bind cpat variables
              forM_ (zip args $ V.toList mapping) $ \(argName, TUIndex{..}) -> do
                let indices = [1 + tuStructIndex, tuArrayIndex]
                emit [(mkName argName) := AST.ExtractValue {aggregate = tuVal, indices' = indices, metadata = []}]

          _ -> error "not implemented"

    DefF name args (_,body) -> do
      -- clear def local state
      -- set heap pointer
      localHeapPointerName <- uniqueName heapPointerName
      let heapPointerParameter = Parameter locationLLVMType localHeapPointerName []
          clearDefState = modify' $ \env -> env
            { _envBasicBlocks       = mempty
            , _envInstructions      = mempty
            , _constantMap          = mempty
            , _currentBlockName     = mkName ""
            , _envBlockInstructions = mempty
            , _envBlockOrder        = mempty
            , _envHeapPointer       = LocalReference locationLLVMType localHeapPointerName
            }
      clearDefState
      activeBlock (mkName $ name ++ ".entry")
      (cgTy,result) <- body >>= getOperand (printf "%s_result" name)
      let wrappedRetType = withHeapPointer $ cgLLVMType cgTy
      -- return the heap pointer + function result
      heapPointer <- gets _envHeapPointer
      wrappedResult0 <- codeGenLocalVar (printf "%s_wrapped_result" name) wrappedRetType $ AST.InsertValue
        { aggregate = undef wrappedRetType
        , element   = heapPointer
        , indices'  = [0]
        , metadata  = []
        }
      wrappedResult1 <- codeGenLocalVar (printf "%s_wrapped_result" name) wrappedRetType $ AST.InsertValue
        { aggregate = wrappedResult0
        , element   = result
        , indices'  = [1]
        , metadata  = []
        }

      closeBlock $ Ret
        { returnOperand = Just wrappedResult1
        , metadata'     = []
        }
      errorBlock
      blockInstructions <- Map.delete (mkName "") <$> gets _envBlockInstructions
      unless (Map.null blockInstructions) $ error $ printf "unclosed blocks in %s\n  %s" name (show blockInstructions)
      blocks <- gets _envBasicBlocks
      (retType, argTypes) <- getFunctionType name
      when (retType /= cgTy) $ error $ printf "return type mismatch for %s\n  retTy: %s\n  cgTy: %s\n" name (show retType) (show cgTy)
      let def = GlobalDefinition functionDefaults
            { name        = mkName name
            , parameters  = (heapPointerParameter : [Parameter (cgLLVMType argType) (mkName a) [] | (a, argType) <- zip args argTypes], False) -- HINT: False - no var args
            , returnType  = wrappedRetType -- includes the heap pointer
            , basicBlocks = Map.elems blocks
            , callingConvention = CC.C
            , functionAttributes = [Right $ FA.StringAttribute "no-jump-tables" "true"]
            }
      clearDefState
      modify' (\env@Env{..} -> env {_envDefinitions = def : _envDefinitions})
      pure $ O unitCGType unit

    ProgramF defs -> do
      -- register prim fun lib
      registerPrimFunLib
      sequence_ (map snd defs) >> pure (O unitCGType unit)

    SFetchIF name Nothing -> do
      -- load tag
      tagAddress <- codeGenVal $ Var name
      tagVal <- codeGenLocalVar "tag" tagLLVMType $ Load
        { volatile        = False
        , address         = tagAddress
        , maybeAtomicity  = Nothing
        , alignment       = 1
        , metadata        = []
        }
      -- switch on possible tags
      TypeEnv{..} <- gets _envTypeEnv
      let T_SimpleType (T_Location locs) = Map.findWithDefault undefined name _variable
          nodeSet       = mconcat [_location V.! loc | loc <- locs]
          resultCGType  = toCGType $ T_NodeSet nodeSet
          resultTU      = cgTaggedUnion resultCGType
      codeGenTagSwitch tagVal nodeSet $ \tag items -> do
        let nodeCGType  = toCGType $ T_NodeSet $ Map.singleton tag items
            nodeTU      = cgTaggedUnion nodeCGType
        nodeAddress <- codeGenBitCast "nodeAddress" tagAddress (ptr $ tuLLVMType nodeTU)
        nodeVal <- codeGenLocalVar "node" (cgLLVMType nodeCGType) $ Load
          { volatile        = False
          , address         = nodeAddress
          , maybeAtomicity  = Nothing
          , alignment       = 1
          , metadata        = []
          }
        (resultCGType,) <$> copyTaggedUnion nodeVal nodeTU resultTU

    SStoreF val -> do
      nodeLocation <- gets _envHeapPointer
      codeGenStoreNode val nodeLocation
      pure $ O locationCGType nodeLocation

    SUpdateF name val -> do
      nodeLocation <- codeGenVal $ Var name
      codeGenStoreNode val nodeLocation
      pure $ O unitCGType unit

    expF -> error $ printf "missing codegen for:\n%s" (show $ pretty $ embed $ fmap fst expF)

codeGenStoreNode :: Val -> Operand -> CG ()
codeGenStoreNode val nodeLocation = do
  tuVal <- codeGenVal val
  tagVal <- codeGenExtractTag tuVal
  T_NodeSet nodeSet <- typeOfVal val
  let valueTU = taggedUnion nodeSet
  codeGenTagSwitch tagVal nodeSet $ \tag items -> do
    let nodeTU = taggedUnion $ Map.singleton tag items
    nodeVal <- copyTaggedUnion tuVal valueTU nodeTU
    nodeAddress <- codeGenBitCast "nodeAddress" nodeLocation (ptr $ tuLLVMType nodeTU)
    emit [Do Store
      { volatile        = False
      , address         = nodeAddress
      , value           = nodeVal
      , maybeAtomicity  = Nothing
      , alignment       = 1
      , metadata        = []
      }]
    pure $ (unitCGType, unit)
  pure ()

codeGenCase :: Operand -> [(Alt, CG Result)] -> (CPat -> CG ()) -> CG Result
codeGenCase opVal alts bindingGen = do
  curBlockName <- gets _currentBlockName

  -- save heap pointer operand
  heapPointer <- gets _envHeapPointer

  (altDests, altValues, altCGTypes, altHeapPointers) <- fmap unzip4 . forM alts $ \(Alt cpat _, altBody) -> do
    altCPatVal <- getCPatConstant cpat
    altEntryBlock <- uniqueName ("switch." ++ getCPatName cpat)
    activeBlock altEntryBlock

    -- restore saved heap pointer operand
    modify' $ \env -> env {_envHeapPointer = heapPointer}
    bindingGen cpat

    altResult <- altBody
    (altCGTy, altOp) <- getOperand "altResult" altResult
    -- capture alternative's heap pointer and return along with altOp
    altHeapPointer <- gets _envHeapPointer

    lastAltBlock <- gets _currentBlockName

    pure ((altCPatVal, altEntryBlock), (altOp, lastAltBlock, altCGTy), altCGTy, (altHeapPointer, lastAltBlock))

  let resultCGType = commonCGType altCGTypes
  switchExit <- uniqueName "switch.exit" -- this is the next block

  altConvertedValues <- forM altValues $ \(altOp, lastAltBlock, altCGTy) -> do
    activeBlock lastAltBlock
    -- HINT: convert alt result to common type
    convertedAltOp <- codeGenValueConversion altCGTy altOp resultCGType
    closeBlock $ Br
      { dest      = switchExit
      , metadata' = []
      }
    pure (convertedAltOp, lastAltBlock)

  activeBlock curBlockName
  closeBlock $ Switch
        { operand0'   = opVal
        , defaultDest = mkName "error_block" -- QUESTION: do we want to catch this error?
        , dests       = altDests
        , metadata'   = []
        }

  activeBlock switchExit

  -- update heap pointer with the one comes from the alternatives
  newHeapPointer <- codeGenLocalVar heapPointerName locationLLVMType $ Phi
    { type'           = locationLLVMType
    , incomingValues  = {-(heapPointer, curBlockName) : -}altHeapPointers
    , metadata        = []
    }
  modify' $ \env -> env {_envHeapPointer = newHeapPointer}

  pure . I resultCGType $ Phi
    { type'           = cgLLVMType resultCGType
    , incomingValues  = {-(undef (cgLLVMType resultCGType), curBlockName) : -}altConvertedValues
    , metadata        = []
    }

-- merge heap pointers from alt branches
codeGenTagSwitch :: Operand -> NodeSet -> (Tag -> Vector SimpleType -> CG (CGType, Operand)) -> CG Result
codeGenTagSwitch tagVal nodeSet tagAltGen | Map.size nodeSet > 1 = do
  let possibleNodes = Map.toList nodeSet
  curBlockName <- gets _currentBlockName

  -- save heap pointer operand
  heapPointer <- gets _envHeapPointer

  (altDests, altValues, altCGTypes, altHeapPointers) <- fmap unzip4 . forM possibleNodes $ \(tag, items) -> do
    let cpat = TagPat tag
    altEntryBlock <- uniqueName ("tag.switch." ++ getCPatName cpat)
    altCPatVal <- getCPatConstant cpat
    activeBlock altEntryBlock

    -- restore saved heap pointer operand
    modify' $ \env -> env {_envHeapPointer = heapPointer}

    (altCGTy, altOp) <- tagAltGen tag items

    -- capture alternative's heap pointer and return along with altOp
    altHeapPointer <- gets _envHeapPointer

    lastAltBlock <- gets _currentBlockName

    pure ((altCPatVal, altEntryBlock), (altOp, lastAltBlock, altCGTy), altCGTy, (altHeapPointer, lastAltBlock))

  let resultCGType = commonCGType altCGTypes
  switchExit <- uniqueName "tag.switch.exit" -- this is the next block

  altConvertedValues <- forM altValues $ \(altOp, lastAltBlock, altCGTy) -> do
    activeBlock lastAltBlock
    -- HINT: convert alt result to common type
    convertedAltOp <- codeGenValueConversion altCGTy altOp resultCGType
    closeBlock $ Br
      { dest      = switchExit
      , metadata' = []
      }
    pure (convertedAltOp, lastAltBlock)

  activeBlock curBlockName
  closeBlock $ Switch
    { operand0'   = tagVal
    , defaultDest = mkName "error_block" -- QUESTION: do we want to catch this error?
    , dests       = altDests
    , metadata'   = []
    }

  activeBlock switchExit

  -- update heap pointer with the one comes from the alternatives
  newHeapPointer <- codeGenLocalVar heapPointerName locationLLVMType $ Phi
    { type'           = locationLLVMType
    , incomingValues  = {-(heapPointer, curBlockName) : -}altHeapPointers
    , metadata        = []
    }
  modify' $ \env -> env {_envHeapPointer = newHeapPointer}

  pure . I resultCGType $ Phi
    { type'           = cgLLVMType resultCGType
    , incomingValues  = {-(undef (cgLLVMType resultCGType), curBlockName) : -}altConvertedValues
    , metadata        = []
    }

codeGenTagSwitch tagVal nodeSet tagAltGen | [(tag, items)] <- Map.toList nodeSet = do
  uncurry O <$> tagAltGen tag items

-- heap pointer related functions

withHeapPointer :: LLVM.Type -> LLVM.Type
withHeapPointer ty = StructureType
  { isPacked      = True
  , elementTypes  = [locationLLVMType, ty]
  }

codeGenIncreaseHeapPointer :: String -> CG ()
codeGenIncreaseHeapPointer name = do
  -- increase heap pointer
  CG_SimpleType {cgType = T_SimpleType (T_Location [loc])} <- getVarType name
  nodeSet <- use $ envTypeEnv.location.ix loc
  heapPointer0 <- gets _envHeapPointer

  let tuPtrTy = ptr $ tuLLVMType $ taggedUnion nodeSet
  heapPointer1 <- codeGenBitCast heapPointerName heapPointer0 tuPtrTy
  newHeapPointer0 <- codeGenLocalVar heapPointerName tuPtrTy $ AST.GetElementPtr
    { inBounds  = True
    , address   = heapPointer1
    , indices   = [ConstantOperand $ C.Int 32 1]
    , metadata  = []
    }
  -- cast to tag pointer ; generic heap pointer type
  newHeapPointer <- codeGenBitCast heapPointerName newHeapPointer0 locationLLVMType
  modify' $ \env -> env {_envHeapPointer = newHeapPointer}

external :: Type -> AST.Name -> [(Type, AST.Name)] -> CG ()
external retty label argtys = modify' (\env@Env{..} -> env {_envDefinitions = def : _envDefinitions}) where
  def = GlobalDefinition $ functionDefaults
    { name        = label
    , linkage     = L.External
    , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
    , returnType  = retty
    , basicBlocks = []
    }

-- available primitive functions
registerPrimFunLib :: CG ()
registerPrimFunLib = do
  external i64 (mkName "_prim_int_print") [(i64, mkName "x")]

errorBlock = do
  activeBlock $ mkName "error_block"
  let functionType = FunctionType
                { resultType    = i64
                , argumentTypes = [i64]
                , isVarArg      = False
                }

  codeGenLocalVar "error_result" i64 $ Call
            { tailCallKind        = Just Tail
            , callingConvention   = CC.C
            , returnAttributes    = []
            , function            = Right . ConstantOperand $ GlobalReference (ptr functionType) (mkName "_prim_int_print")
            , arguments           = zip [ConstantOperand $ C.Int 64 666] (repeat [])
            , functionAttributes  = []
            , metadata            = []
            }
  closeBlock $ Unreachable []
