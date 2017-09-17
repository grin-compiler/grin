{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards, OverloadedStrings #-}

module CodeGenLLVM where

import Text.Printf
import Control.Monad as M
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Grin

import LLVM.AST
import LLVM.AST.Type
import LLVM.AST.Constant hiding (Add, ICmp)
import LLVM.AST.IntegerPredicate
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST as AST
import LLVM.AST.Global
--import LLVM.Context
--import LLVM.Module

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = []
  }

{-
toLLVM :: AST.Module -> IO ()
toLLVM mod = withContext $ \ctx -> do
  llvm <- withModuleFromAST ctx mod moduleLLVMAssembly
  BS.putStrLn llvm


main2 :: IO ()
main2 = toLLVM module_
-}
{-
main =
  n13 <- sum 0 1 10000
  intPrint n13

sum n29 n30 n31 =
  b2 <- intGT n30 n31
  case b2 of
    CTrue -> pure n29
    CFalse -> n18 <- intAdd n30 1
              n28 <- intAdd n29 n30
              sum n28 n18 n31
-}

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

tagMap :: Map Tag (Type, Constant)
tagMap = Map.fromList
  [ (Tag Grin.C "False" 0, (i1, Int 1 0))
  , (Tag Grin.C "True" 0,  (i1, Int 1 1))
  ]

typeMap :: Map Grin.Name Type
typeMap = Map.fromList
  [ ("b2" , i64)
  , ("n13", i64)
  , ("n18", i64)
  , ("n28", i64)
  , ("n29", i64)
  , ("n30", i64)
  , ("n31", i64)
  , ("sum", i64)
  ]

getType :: Grin.Name -> Type
getType name = case Map.lookup name typeMap of
  Nothing -> error $ "getType - unknown variable " ++ name
  Just ty -> ty

getTagId :: Tag -> Constant
getTagId tag = case Map.lookup tag tagMap of
  Nothing -> error $ "getTag - unknown tag " ++ show tag
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
  { envDefinitions  :: [Definition]
  , envBasicBlocks  :: [BasicBlock]
  , envInstructions :: [Named Instruction]
  , constantMap     :: Map Grin.Name Operand
  }

type CG = State Env

emit :: [Named Instruction] -> CG ()
emit instructions = modify' (\env@Env{..} -> env {envInstructions = instructions ++ envInstructions})

addConstant :: Grin.Name -> Operand -> CG ()
addConstant name operand = modify' (\env@Env{..} -> env {constantMap = Map.insert name operand constantMap})

unit :: CG Operand
unit = pure $ ConstantOperand $ Undef VoidType

codeGenVal :: Val -> CG Operand
codeGenVal = \case
  Unit          -> unit
  Var name      -> Map.lookup name <$> gets constantMap >>= \case
                      Nothing -> pure $ LocalReference (getType name) (mkName name) -- TODO: lookup in constant map
                      Just operand  -> pure operand
  Lit (LInt v)  -> pure $ ConstantOperand $ Int 64 (fromIntegral v)
  ValTag tag -> pure $ ConstantOperand $ getTagId tag
  -- TODO: support nodes

getCPatConstant :: CPat -> Constant
getCPatConstant = \case
  NodePat tag _     -> getTagId tag
  TagPat  tag       -> getTagId tag
  LitPat  (LInt v)  -> Int 64 (fromIntegral v)

getCPatName :: CPat -> String
getCPatName = \case
  NodePat tag _     -> tagName tag
  TagPat  tag       -> tagName tag
  LitPat  (LInt v)  -> "int_" ++ show v
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
closeBlock = undefined

getOperand :: Result -> CG Operand
getOperand = undefined

startNewBlock :: AST.Name -> CG ()
startNewBlock = undefined

addBlock :: AST.Name -> CG a -> CG a
addBlock = undefined

codeGen :: Exp -> CG ()
codeGen = M.void . para folder where
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
    SAppF "intPrint" [a] -> O <$> unit -- TODO
    SAppF "intGT" [a, b] -> do
      [opA, opB] <- mapM codeGenVal [a, b]
      pure . I $ ICmp
        { iPredicate  = SGT
        , operand0    = opA
        , operand1    = opB
        , metadata    = []
        }
    SAppF "intAdd" [a, b] -> do
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
        { tailCallKind        = Nothing
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
      closeBlock $ Switch
            { operand0'   = opVal
            , defaultDest = switchExit -- QUESTION: do we want to catch this error?
            , dests       = altDests
            , metadata'   = []
            }
      startNewBlock switchExit
      pure . I $ Phi
        { type'           = i64 -- TODO :: Type,
        , incomingValues  = altValues
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
            , returnType  = getType name
            , basicBlocks = blocks
            }
      clearDefState
      modify' (\env@Env{..} -> env {envDefinitions = def : envDefinitions})
      O <$> unit

    ProgramF defs -> sequence_ (map snd defs) >> O <$> unit

    SStoreF{}   -> fail "SStoreF is not supported yet"
    SFetchIF{}  -> fail "SFetchIF is not supported yet"
    SUpdateF{}  -> fail "SUpdateF is not supported yet"
