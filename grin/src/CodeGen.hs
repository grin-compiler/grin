{-# LANGUAGE LambdaCase, TupleSections, DataKinds #-}

module CodeGen where

import Control.Monad
import Control.Monad.State
import Data.Functor.Foldable as Foldable

import Data.Map (Map)
import qualified Data.Map as Map

import Foreign hiding (void)
import CodeGen.X86
import CodeGen.X86.Examples

import Grin

{-
main =
  n13 <- sum 0.0 1.0 10000.0
  intPrint n13

sum n29 n30 n31 =
  b2 <- intGT n30 n31
  case b2 of
    CTrue -> pure n29
    CFalse -> n18 <- intAdd n30 1.0
              n28 <- intAdd n29 n30
              sum n28 n18 n31
-}

{-
  TODO
    - compile functions separately
    - Def
    * - function call
    done - bind operator
    * - case
    - memory operations
      - fetch
      - store
      - update
-}

regRel reg offset = MemOp $ Addr (Just reg) (Just offset) NoIndex
bpRel = regRel rbp

mainCode :: Code
mainCode = saveNonVolatile $ do
    mov result arg1
    ret
    mov rcx (addr64 (0 :: Address S64))
    mainFun <- label
    -- init
    push rbp
    mov rbp rsp
    sub rsp 32 -- space for local variables

    -- get args from stack
    mov rcx (bpRel 0)

    -- fetch nodes
    mov rax (regRel rcx 0) -- get tag   ; fetchI 0
    mov rbx (regRel rcx 8) -- get val1  ; fetchI 1

    -- app / function call
    push rbx
    call (ipRelValue mainFun)
    ret

-- QUESTION: from where will we know the labels when generating call op?

-- HINT: the most important part of codegen is EBind

-- IDEA: variable / register handling: use Var Name -> stack index mapping ; clear the map when leaving a Def

type StackIndex = Int32
type StackMap = Map Name StackIndex

type X64 = StateT StackMap CodeM

getStackIndex :: Name -> X64 StackIndex
getStackIndex name = do
  stackMap <- get
  case Map.lookup name stackMap of
    Just i -> pure (i * 8)
    Nothing -> modify' (Map.insert name i) >> pure (i * 8) where i = fromIntegral $ Map.size stackMap

resultVarName = "$result$" -- to store sexp values

-- TODO: create environment that contains the local stack index for variables
codeGen :: Exp -> Code
codeGen = void . flip runStateT mempty . cata folder where
  folder = \case
    ProgramF defs -> sequence defs >> pure 0
    DefF name args exp -> do
      -- pop the return values from the stack
      -- generate the entry code ; sub rsp LOCAL_VAR_SPACE_SIZE
      exp
      -- push the return value to the stack

    SReturnF val -> case val of
      Lit (LInt v)  -> do
        idx <- getStackIndex resultVarName
        lift $ do
          mov rax (fromIntegral v)
          mov (bpRel idx) rax
        pure idx
      Var name -> getStackIndex name

    EBindF sexp (Var name) exp -> do
      valStackIndex <- sexp
      varStackIndex <- getStackIndex name -- lookup or create local var
      lift $ do
        mov rax (bpRel valStackIndex)
        mov (bpRel varStackIndex) rax
      exp

    SFetchIF name (Just index) -> do
      resultStackIndex <- getStackIndex resultVarName
      nameStackIndex <- getStackIndex name
      lift $ do
        mov rsi (bpRel nameStackIndex)
        mov rax (regRel rsi (8 * fromIntegral index))
        mov (bpRel resultStackIndex) rax
      pure resultStackIndex

    SAppF name args -> do
      -- push arguments to stack
      forM_ args $ \val -> do
          let nameStackIndex = 0 -- TODO
          lift $ push (bpRel nameStackIndex)
      lift $ do
        nameLabel <- label-- TODO
        call (ipRelValue nameLabel)
      -- TODO: pop return values and copy them to binders
      pure 0 -- TODO: create local var for result

    e -> pure 0

{-
data Exp
  = Program     [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp LPat Exp
  | ECase       Val [Alt]
  -- Simple Exp
  | SApp        Name [SimpleVal]
  | SReturn     Val
  | SStore      Val
  | SFetchI     Name (Maybe Int) -- fetch a full node or a single node item
  | SUpdate     Name Val
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
-}
