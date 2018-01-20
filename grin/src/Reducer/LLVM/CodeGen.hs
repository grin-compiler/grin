{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings #-}

module Reducer.LLVM.CodeGen
  ( codeGen
  , toLLVM
  ) where

import Debug.Trace
import Text.Show.Pretty
import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Grin
import AbstractInterpretation.AbstractRunGrin (HPTResult(..), emptyComputer)

import LLVM.AST hiding (callingConvention)
import LLVM.AST.Type
import LLVM.AST.AddrSpace
import LLVM.AST.Constant hiding (Add, ICmp)
import LLVM.AST.IntegerPredicate
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST as AST
import LLVM.AST.Global
import LLVM.Context
import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

{-
  Supported language:
    Int64 + nodes from Int64
    intPrint
    intGT
    intAdd
-}
--trace _ = id

toLLVM :: String -> AST.Module -> IO BS.ByteString
toLLVM fname mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.writeFile fname llvm
  pure llvm


{-
    b2     -> {BAS}
    n13    -> {BAS,sum}
    n18    -> {BAS}
    n28    -> {BAS}
    n29    -> {BAS}
    n30    -> {BAS}
    n31    -> {BAS}
    sum    -> {BAS,sum}
-}

-- TODO: create Tag map ; get as parameter ; store in reader environment
{-
  question: how to calculate from grin or hpt result?
-}
tagMap :: Map Tag (Type, Constant)
tagMap = Map.fromList
  [ (Tag Grin.C "False" 0, (i1, Int 1 0))
  , (Tag Grin.C "True" 0,  (i1, Int 1 1))
  ]

-- TODO: create Type map ; calculate once ; store in reader environment
{-
  question: how to calculate from grin or hpt result?
    ANSWER: lookup from HPT result ; function name = result type ; argument names = input type

  TODO:
    in pre passes build ; store in env
      function type map (llvm type)
      variable map (llvm type)
-}
typeMap :: Map Grin.Name Type
typeMap = Map.fromList
  [ ("b2" , i64)
  , ("n13", i64)
  , ("n18", i64)
  , ("n28", i64)
  , ("n29", i64)
  , ("n30", i64)
  , ("n31", i64)
  , ("sum", fun i64 [i64, i64, i64])
  , ("_prim_intPrint", fun i64 [i64])
  , ("grinMain", fun i64 [])
  , ("upto", fun (struct [i64]) [i64, i64])
  , ("eval", fun (struct [i64]) [struct [i64]])
  ] where
    struct elems = StructureType { isPacked = False, elementTypes = elems }
    ptr ty = PointerType { pointerReferent = ty, pointerAddrSpace = AddrSpace 0}
    fun ret args = ptr FunctionType {resultType = ret, argumentTypes = args, isVarArg = False}

getType :: Grin.Name -> Type
getType name = case Map.lookup name typeMap of
  Nothing -> trace ("getType - unknown variable " ++ name) $ PointerType i64 (AddrSpace 0)
  Just ty -> ty

getTagId :: Tag -> Constant
getTagId tag = case Map.lookup tag tagMap of
  Nothing -> trace ("getTag - unknown tag " ++ show tag) $ Int 64 0
  Just (ty, c) -> c

{-
data Val
  = ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag)
  | VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  | ValTag        Tag
  | Unit
  -- simple val
  | Lit Lit
  | Var Name
-}

data Env
  = Env
  { envDefinitions    :: [Definition]
  , envBasicBlocks    :: [BasicBlock]
  , envInstructions   :: [Named Instruction]
  , constantMap       :: Map Grin.Name Operand
  , currentBlockName  :: AST.Name
  , envTempCounter    :: Int
  , envHPTResult      :: HPTResult
  }

emptyEnv = Env
  { envDefinitions    = mempty
  , envBasicBlocks    = mempty
  , envInstructions   = mempty
  , constantMap       = mempty
  , currentBlockName  = mkName ""
  , envTempCounter    = 0
  , envHPTResult      = emptyComputer
  }

type CG = State Env

emit :: [Named Instruction] -> CG ()
emit instructions = modify' (\env@Env{..} -> env {envInstructions = envInstructions ++ instructions})

addConstant :: Grin.Name -> Operand -> CG ()
addConstant name operand = modify' (\env@Env{..} -> env {constantMap = Map.insert name operand constantMap})

unit :: CG Operand
unit = pure $ ConstantOperand $ Undef VoidType

undef :: Type -> Operand
undef = ConstantOperand . Undef

codeGenVal :: Val -> CG Operand
codeGenVal = \case
  Unit          -> unit
  Var name      -> Map.lookup name <$> gets constantMap >>= \case
                      Nothing -> pure $ LocalReference (getType name) (mkName name) -- TODO: lookup in constant map
                      Just operand  -> pure operand
  Lit (LInt64 v) -> pure $ ConstantOperand $ Int 64 (fromIntegral v)
  ValTag tag -> pure $ ConstantOperand $ getTagId tag
  -- TODO: support nodes
  --ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag)
  VarTagNode tagVar args -> do
    opTag <- codeGenVal $ Var tagVar
    opArgs <- mapM codeGenVal args -- complete node (variable tag)
    {-
      TODO:
        - create the struct type
            - type of tag
            - type of args
        - create struct with constants
        - fot the rest emit insertvalue
    -}
    pure $ ConstantOperand $ Struct
      { structName    = Nothing
      , isPacked      = True -- or False?
      , memberValues  = replicate (1 + length opArgs) (Undef i64)-- TODO :: [ Constant ]
      }

  val -> error $ "codeGenVal: " ++ show val

getCPatConstant :: CPat -> Constant
getCPatConstant = \case
  NodePat tag _      -> getTagId tag
  TagPat  tag        -> getTagId tag
  LitPat  (LInt64 v) -> Int 64 (fromIntegral v)

getCPatName :: CPat -> String
getCPatName = \case
  NodePat tag _      -> tagName tag
  TagPat  tag        -> tagName tag
  LitPat  (LInt64 v) -> "int_" ++ show v
 where
  tagName (Tag c name n) = printf "%s%s%d" (show c) name n

data Result
  = I Instruction
  | O Operand

-- https://stackoverflow.com/questions/6374355/llvm-assembly-assign-integer-constant-to-register
{-
  NOTE: if the cata result is a monad then it means the codegen is sequential

  IDEA: write an untyped codegen ; which does not rely on HPT calculated types
-}

-- utils
closeBlock :: Terminator -> CG ()
closeBlock tr = modify' (\env@Env{..} -> env {envInstructions = mempty, envBasicBlocks = envBasicBlocks ++ [BasicBlock currentBlockName envInstructions (Do tr)]})

startNewBlock :: AST.Name -> CG ()
startNewBlock name = modify' (\env@Env{..} -> env {envInstructions = mempty, currentBlockName = name})

addBlock :: AST.Name -> CG a -> CG a
addBlock name block = do
  instructions <- gets envInstructions
  curBlockName <- gets currentBlockName
  startNewBlock name
  result <- block
  modify' (\env@Env{..} -> env {envInstructions = instructions, currentBlockName = curBlockName})
  pure result

uniqueTempName :: CG AST.Name
uniqueTempName = state (\env@Env{..} -> (mkName $ printf "tmp%d" envTempCounter, env {envTempCounter = succ envTempCounter}))

getOperand :: Result -> CG Operand
getOperand = \case
  O a -> pure a
  I i -> do
          tmp <- uniqueTempName
          emit [tmp := i]
          pure $ LocalReference i64 tmp -- TODO: handle type

toModule :: Env -> AST.Module
toModule Env{..} = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = envDefinitions
  }

codeGen :: HPTResult -> Exp -> AST.Module
codeGen hptResult = toModule . flip execState (emptyEnv {envHPTResult = hptResult}) . para folder where
  folder :: ExpF (Exp, CG Result) -> CG Result
  folder = \case
    SReturnF val -> O <$> codeGenVal val
    SBlockF a -> snd $ a
    EBindF (_,sexp) pat (_,exp) -> do
      sexp >>= \case
        I instruction -> case pat of
          Var name -> emit [(mkName name) := instruction]
          _ -> emit [Do instruction]
        O operand -> case pat of
          Var name -> addConstant name operand
          _ -> pure () -- TODO: perform binding
      exp

    -- primops calls
    --SAppF "intPrint" [a] -> pure . O $ undef i64 -- TODO
    SAppF "_prim_intGT" [a, b] -> do
      [opA, opB] <- mapM codeGenVal [a, b]
      pure . I $ ICmp
        { iPredicate  = SGT
        , operand0    = opA
        , operand1    = opB
        , metadata    = []
        }
    SAppF "_prim_intAdd" [a, b] -> do
      [opA, opB] <- mapM codeGenVal [a, b]
      pure . I $ Add
        { nsw       = False
        , nuw       = False
        , operand0  = opA
        , operand1  = opB
        , metadata  = []
        }

    -- call to top level functions
    SAppF name args -> do
      operands <- mapM codeGenVal args
      pure . I $ Call
        { tailCallKind        = Just Tail
        , callingConvention   = CC.C
        , returnAttributes    = []
        , function            = Right $ ConstantOperand $ GlobalReference (getType name) (mkName name)
        , arguments           = zip operands (repeat [])
        , functionAttributes  = []
        , metadata            = []
        }

    AltF _ a -> snd a
    ECaseF val alts -> do
      opVal <- codeGenVal val
      let switchExit  = mkName "switch.exit" -- TODO: generate unique name ; this is the next block
      rec
        curBlockName <- gets currentBlockName
        closeBlock $ Switch
              { operand0'   = opVal
              , defaultDest = switchExit -- QUESTION: do we want to catch this error?
              , dests       = altDests
              , metadata'   = []
              }
        (altDests, altValues) <- fmap unzip . forM alts $ \(Alt cpat _, altBody) -> do
          let altBlockName  = mkName ("switch." ++ getCPatName cpat) -- TODO: generate unique names
              altCPatVal    = getCPatConstant cpat
          addBlock altBlockName $ do
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
      let clearDefState = modify' (\env -> env {envBasicBlocks = mempty, envInstructions = mempty, constantMap = mempty})
      clearDefState
      startNewBlock (mkName $ name ++ ".entry")
      result <- body >>= getOperand
      closeBlock $ Ret
        { returnOperand = Just result
        , metadata'     = []
        }
      blocks <- gets envBasicBlocks
      let def = GlobalDefinition functionDefaults
            { name        = mkName name
            , parameters  = ([Parameter (getType a) (mkName a) [] | a <- args], False) -- HINT: False - no var args
            , returnType  = i64 -- getType name -- TODO: get ret type
            , basicBlocks = blocks
            , callingConvention = CC.C
            }
      clearDefState
      modify' (\env@Env{..} -> env {envDefinitions = def : envDefinitions})
      O <$> unit

    ProgramF defs -> do
      -- register prim fun lib
      registerPrimFunLib
      sequence_ (map snd defs) >> O <$> unit

    SStoreF val -> do
      -- TODO: allocate memory; calculate types properly
      let opAddress = ConstantOperand $ Null $ PointerType i64 (AddrSpace 0)
      opVal <- codeGenVal val
      pure . I $ Store
        { volatile        = False
        , address         = opAddress
        , value           = opVal
        , maybeAtomicity  = Nothing
        , alignment       = 0
        , metadata        = []
        }
      {-
      tempName <- uniqueTempName
      emit [tempName := Store
        { volatile        = False
        , address         = opAddress
        , value           = opVal
        , maybeAtomicity  = Nothing
        , alignment       = 0
        , metadata        = []
        }]
      pure . O $ LocalReference i64 tempName
      -}
    SFetchIF name mIdx -> do
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
external retty label argtys = modify' (\env@Env{..} -> env {envDefinitions = def : envDefinitions}) where
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
  external i64 (mkName "_prim_intPrint") [(i64, mkName "x")]
