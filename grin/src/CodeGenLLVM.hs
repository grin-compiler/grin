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

defAdd :: Definition
defAdd = GlobalDefinition functionDefaults
  { name = Name "add"
  , parameters =
      ( [ Parameter i64 (Name "a") []
        , Parameter i64 (Name "b") [] ]
      , False )
  , returnType = i64
  , basicBlocks = [body]
  }
  where
    body = BasicBlock
        (Name "entry")
        [ Name "result" :=
            Add False  -- no signed wrap
                False  -- no unsigned wrap
                (LocalReference i64 (Name "a"))
                (LocalReference i64 (Name "b"))
                []]
        (Do $ Ret (Just (LocalReference i64 (Name "result"))) [])


module_ :: AST.Module
module_ = defaultModule
  { moduleName = "basic"
  , moduleDefinitions = [defAdd]
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

{-
  TODO:
    collect basic blocks
    collect global definitions
-}
data Env
  = Env
  { envInstructions :: [Named Instruction]
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
    SAppF "intGt" [a, b] -> do
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

    ECaseF val alts -> do
      {-
        TODO:
          collect alternatives ; create new basic block for each with unique alt name (i.e. based on the tag name)
          finish the current basic block with the switch terminator
      -}
      -- TODO: emit switch basic block terminator
      opVal <- codeGenVal val
      -- TODO: collect alt basic blocks
      let altBlocks = [(getCPatConstant cpat, mkName (getCPatName cpat)) | (Alt cpat _, m) <- alts]
          terminator = Switch
            { operand0'   = opVal
            , defaultDest = mkName "impossible" -- TODO :: Name,
            , dests       = altBlocks
            , metadata'   = []
            }
      O <$> unit

    DefF name args (_,body) -> do
      -- TODO collect function definition
      let def = GlobalDefinition functionDefaults
            { name = mkName name
            , parameters = ([Parameter (getType a) (mkName a) [] | a <- args], False) -- HINT: False - no var args
            , returnType = getType name
            , basicBlocks = [] -- TODO
            }
      O <$> unit

    _ -> O <$> unit

{-
  ProgramF  [a]
  DefF      Name [Name] a

  AltF CPat a

  SStoreF   Val
  SFetchIF  Name (Maybe Int)
  SUpdateF  Name Val
-}