{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo #-}

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

regRel reg offset = MemOp $ Addr (Just reg) (Just offset) NoIndex
bpRel = regRel rbp

-- QUESTION: from where will we know the labels when generating call op?

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
resultIndex = getStackIndex resultVarName

codeGenVal :: Val -> X64 StackIndex
codeGenVal = \case
  Unit -> pure 0 -- QUESTION: is this correct?

  Var name -> getStackIndex name

  Lit (LInt v)  -> do
    idx <- getStackIndex resultVarName
    lift $ do
      mov rax (fromIntegral v)
      mov (bpRel idx) rax
    pure idx

  ValTag (Tag Grin.C tagname _) -> do
    idx <- getStackIndex resultVarName
    lift $ mov (bpRel idx :: Operand RW S64) $ case tagname of
      "True"  -> 1 -- FIXME: hardcode it for now
      "False" -> 0
      _ -> 2 -- TODO
    pure idx

codeGenPat :: CPat -> X64 ()
codeGenPat = \case
  TagPat (Tag Grin.C tagname _) -> do
    lift $ mov rax $ case tagname of
      "True"  -> 1 -- FIXME: hardcode it for now
      "False" -> 0
      _ -> 2 -- TODO
  LitPat (LInt v) -> lift $ mov rax (fromIntegral v)

codeGenBinOp a b op = do
  resultStackIndex <- getStackIndex resultVarName
  -- load a value to register
  aIdx <- codeGenVal a
  lift $ mov rax (bpRel aIdx)

  -- load b value to register
  bIdx <- codeGenVal b
  lift $ do
    mov rbx (bpRel bIdx)

    -- do the work
    op -- a in rax, b in rbx, result should go to rax

    -- copy to result
    mov (bpRel resultStackIndex) rax
  pure resultStackIndex

-- TODO: create environment that contains the local stack index for variables
codeGen :: Exp -> Code
codeGen = void . flip runStateT mempty . para folder where
  folder :: ExpF (Exp, X64 StackIndex) -> X64 StackIndex
  folder = \case
    --ProgramF defs -> mapM id
    DefF name args (_,exp) -> do
      -- pop the return values from the stack
      -- generate the entry code ; sub rsp LOCAL_VAR_SPACE_SIZE
      rec
        lift $ do
          push rbp
          mov rbp rsp
          sub rsp (fromIntegral $ localVarCount * 8) -- create space for local variables
        result <- exp

        lift $ mov rax (bpRel result) -- load return value

        localVarCount <- gets Map.size
      lift $ do
        mov rsp rbp
        pop rbp
        ret
      put mempty -- clean up local variable map
      pure $ result

    SReturnF val -> codeGenVal val

    EBindF (_,sexp) lpat (_,exp) -> do
      valStackIndex <- sexp
      case lpat of
        Var name -> do
          varStackIndex <- getStackIndex name -- lookup or create local var
          -- copy val to var
          lift $ do
            mov rax (bpRel valStackIndex)
            mov (bpRel varStackIndex) rax
        _ -> pure ()
      exp

    SFetchIF name (Just index) -> do
      resultStackIndex <- getStackIndex resultVarName
      nameStackIndex <- getStackIndex name
      lift $ do
        -- node address
        mov rsi (bpRel nameStackIndex)
        -- fetch node item
        mov rax (regRel rsi (8 * fromIntegral index))
        -- copy to result
        mov (bpRel resultStackIndex) rax
      pure resultStackIndex

    ECaseF val alts -> do
      valIdx <- codeGenVal val
      lift $ mov rdx (bpRel valIdx) -- val

      rec
        _ <- forM_ alts $ \((Alt pat _), exp) -> do
          codeGenPat pat -- will load the tag to rax
          lift $ cmp rax rdx
          rec _ <- lift $ j NZ not_match
              result <- exp
              lift $ jmp exit
              not_match <- lift label
          pure result
        exit <- lift label
      resultIndex -- QUESTION: is this correct?

    SAppF "intAdd" [a, b] -> codeGenBinOp a b $ add rax rbx

    SAppF "intGT" [a, b] -> codeGenBinOp a b $ do
      mov rdx 1
      mov rcx rax
      xor_ rax rax
      sub rcx rbx
      cmov G rax rdx

        {-
                "intPrint" -> primIntPrint args
        -}
    SAppF name args -> do
      -- push arguments to stack
      forM_ args $ \val -> do
          valIdx <- codeGenVal val
          lift $ push (bpRel valIdx)

      resultIdx <- resultIndex
      lift $ do
        nameLabel <- label -- TODO

        call (ipRelValue nameLabel)
        -- remove arguments from stack
        add rsp (fromIntegral $ length args * 8)
        mov (bpRel resultIdx) rax -- save result

      pure resultIdx

    e -> sequence_ (snd <$> e) >> pure 0

{-
 * TODO
    Program     [Def]
      - call main

    Def         Name [Name] Exp
      - save function label
      - process arguments
      done - pass result back to caller ; now it's hardcoded to rax

    SApp        Name [SimpleVal]
      - lookup function label

 * Node support:
  | SStore      Val
  | SUpdate     Name Val
-}
