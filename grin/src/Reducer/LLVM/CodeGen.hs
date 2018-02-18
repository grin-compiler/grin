{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings #-}

module Reducer.LLVM.CodeGen
  ( codeGen
  , toLLVM
  ) where

import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V

import LLVM.AST hiding (callingConvention)
import LLVM.AST.Type as LLVM
import LLVM.AST.AddrSpace
import LLVM.AST.Constant as C hiding (Add, ICmp)
import LLVM.AST.IntegerPredicate
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Float as F
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

import Grin
import Pretty
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

    TypeEnv.T_NodeSet ns <- typeOfVal val
    let TaggedUnion{..} = taggedUnion ns
        agg0 = I $ AST.InsertValue
            { aggregate = undef tuLLVMType
            , element   = opTag
            , indices'  = [0]
            , metadata  = []
            }
        build mAgg (item, TUIndex{..}) = do
          agg <- getOperand mAgg
          pure $ I $ AST.InsertValue
            { aggregate = agg
            , element   = item
            , indices'  = [1 + tuStructIndex, tuArrayIndex]
            , metadata  = []
            }

    agg <- foldM build agg0 $ zip opArgs $ V.toList $ tuMapping Map.! tag
    getOperand agg

  ValTag tag  -> ConstantOperand <$> getTagId tag
  Unit        -> unit
  Lit lit     -> pure . ConstantOperand . codeGenLit $ lit
  Var name    -> do
                  Map.lookup name <$> gets _constantMap >>= \case
                      -- QUESTION: what is this?
                      Nothing -> do
                                  ty <- getVarType name
                                  pure $ LocalReference ty (mkName name) -- TODO: lookup in constant map
                      Just operand  -> pure operand

  _ -> error $ printf "codeGenVal: %s" (show $ pretty val)

getCPatConstant :: CPat -> CG Constant
getCPatConstant = \case
  TagPat  tag       -> getTagId tag
  LitPat  lit       -> pure $ codeGenLit lit
  NodePat tag args  -> getTagId tag -- TODO

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
  , moduleDefinitions = _envDefinitions
  }

{-
  type of:
    ok - SApp        Name [SimpleVal]
    ok - SReturn     Val
    ?? - SStore      Val
    ?? - SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
    ok - SUpdate     Name Val
-}
codeGen :: TypeEnv.TypeEnv -> Exp -> AST.Module
codeGen typeEnv = toModule . flip execState (emptyEnv {_envTypeEnv = typeEnv}) . para folder where
  folder :: ExpF (Exp, CG Result) -> CG Result
  folder = \case
    SReturnF val -> O <$> codeGenVal val <*> typeOfVal val
    SBlockF a -> snd $ a
    {-
    EBindF (SStore{}, sexp) (Var name) (_, exp) -> do
      error "TODO"
    -}
    EBindF (_,sexp) pat (_,exp) -> do
      sexp >>= \case
        I instruction -> case pat of
          Var name -> emit [(mkName name) := instruction]
          -- TODO: node binding
          _ -> emit [Do instruction]
        O operand _ -> case pat of
          Var name -> addConstant name operand
          _ -> pure () -- TODO: perform binding
      exp

    SAppF name args -> do
      operands <- mapM codeGenVal args
      if isPrimName name
        then codeGenPrimOp name args operands
        else do
          -- call to top level functions
          (retType, argTypes) <- getFunctionType name
          pure . I $ Call
            { tailCallKind        = Just Tail
            , callingConvention   = CC.C
            , returnAttributes    = []
            , function            = Right $ ConstantOperand $ GlobalReference (ptr FunctionType {resultType = retType, argumentTypes = argTypes, isVarArg = False}) (mkName name)
            , arguments           = zip operands (repeat [])
            , functionAttributes  = []
            , metadata            = []
            }

    AltF _ a -> snd a
    ECaseF val alts -> do
      opVal <- codeGenVal val
      let switchExit  = mkName "switch.exit" -- TODO: generate unique name ; this is the next block
      rec
        curBlockName <- gets _currentBlockName
        closeBlock $ Switch
              { operand0'   = opVal
              , defaultDest = switchExit -- QUESTION: do we want to catch this error?
              , dests       = altDests
              , metadata'   = []
              }
        (altDests, altValues) <- fmap unzip . forM alts $ \(Alt cpat _, altBody) -> do
          let altBlockName  = mkName ("switch." ++ getCPatName cpat) -- TODO: generate unique names
          altCPatVal <- getCPatConstant cpat
          addBlock altBlockName $ do
            case cpat of
              NodePat tags args -> forM_ args $ \argName -> do
                emit [(mkName argName) := AST.ExtractValue {aggregate = opVal, indices' = [0], metadata = []}] -- TODO
              _ -> pure ()
            result <- altBody
            resultOp <- getOperand result
            closeBlock $ Br
              { dest      = switchExit
              , metadata' = []
              }
            pure ((altCPatVal, altBlockName), (resultOp, altBlockName))
      startNewBlock switchExit
      pure . I $ Phi
        { type'           = i64 -- TODO :: Type,
        , incomingValues  = (undef i64, curBlockName) : altValues
        , metadata        = []
        }

    DefF name args (_,body) -> do
      -- clear def local state
      let clearDefState = modify' (\env -> env {_envBasicBlocks = mempty, _envInstructions = mempty, _constantMap = mempty})
      clearDefState
      startNewBlock (mkName $ name ++ ".entry")
      result <- body >>= getOperand
      closeBlock $ Ret
        { returnOperand = Just result
        , metadata'     = []
        }
      blocks <- gets _envBasicBlocks
      (retType, argTypes) <- getFunctionType name
      let def = GlobalDefinition functionDefaults
            { name        = mkName name
            , parameters  = ([Parameter argType (mkName a) [] | (a, argType) <- zip args argTypes], False) -- HINT: False - no var args
            , returnType  = retType
            , basicBlocks = blocks
            , callingConvention = CC.C
            }
      clearDefState
      modify' (\env@Env{..} -> env {_envDefinitions = def : _envDefinitions})
      O <$> unit <*> pure undefined

    ProgramF defs -> do
      -- register prim fun lib
      registerPrimFunLib
      sequence_ (map snd defs) >> O <$> unit <*> pure undefined

    SStoreF val -> do
      {-
        TODO:
          - case on possible tags
              - convert to singleton node set
              - store
      -}
      TypeEnv.T_NodeSet ns <- typeOfVal val
      let TaggedUnion{..} = taggedUnion ns
          opAddress = ConstantOperand $ Null $ ptr tuLLVMType
      opVal <- codeGenVal val
      -- TODO: allocate memory; calculate types properly
      if Map.size ns == 1
        then do
          pure . I $ Store
            { volatile        = False
            , address         = opAddress
            , value           = opVal
            , maybeAtomicity  = Nothing
            , alignment       = 0
            , metadata        = []
            }
        else do
          pure undefined

    SFetchIF name Nothing -> do
      {-
        read tag
        tag case
          ->  1 - cast node type
              2 - load node
              3 - create undef tagged union
              4 - fill tagged union
        return tagged union operand
      -}
      -- TODO: alter address according mIdx; using getelementptr
      opAddress <- codeGenVal $ Var name
      pure . I $ Load
        { volatile        = False
        , address         = opAddress
        , maybeAtomicity  = Nothing
        , alignment       = 0
        , metadata        = []
        }

    SUpdateF name val -> do
      opAddress <- codeGenVal $ Var name
      opVal <- codeGenVal val
      pure . I $ Store
        { volatile        = False
        , address         = opAddress
        , value           = opVal
        , maybeAtomicity  = Nothing
        , alignment       = 0
        , metadata        = []
        }

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
