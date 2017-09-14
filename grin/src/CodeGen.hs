{-# LANGUAGE LambdaCase, TupleSections, DataKinds, RecursiveDo, RecordWildCards #-}

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

regRel reg offset = MemOp $ Addr (Just reg) (Just offset) NoIndex
bpRel = regRel rbp

-- QUESTION: from where will we know the labels when generating call op?

type StackIndex = Int32

data StackMap
  = StackMap
  { stackMap :: Map Name StackIndex
  , localCounter :: Int32
  , defMap :: Map Name Label
  }

emptyStackMap = StackMap mempty 0 mempty

type X64 = StateT StackMap CodeM

clearLocalVarMap :: X64 ()
clearLocalVarMap = modify' (\sm -> sm {stackMap = mempty, localCounter = 0})

addLocalVar :: Name -> StackIndex -> X64 ()
addLocalVar name index = modify' (\sm@StackMap{..} -> sm {stackMap = Map.insert name index stackMap})

getStackIndex :: Name -> X64 StackIndex
getStackIndex name = do
  StackMap{..} <- get
  case Map.lookup name stackMap of
    Just i -> pure (i * 8)
    Nothing -> fail $ "unknown vairable: " ++ name

newLocalVariable :: Name -> X64 StackIndex
newLocalVariable name = do
  StackMap{..} <- get
  modify' (\sm -> sm {stackMap = Map.insert name localCounter stackMap, localCounter = succ localCounter})
  pure (localCounter * 8)

codeGenVal :: Val -> X64 ()
codeGenVal = \case
  Unit -> pure () -- QUESTION: is this correct?

  Var name -> do
    varIdx <- getStackIndex name
    lift $ mov rax (bpRel varIdx)

  Lit (LInt v) -> lift $ mov rax (fromIntegral v)

  ValTag (Tag Grin.C tagname _) -> do
    lift $ mov rax $ case tagname of
      "True"  -> 1 -- FIXME: hardcode it for now
      "False" -> 0
      _ -> 2 -- TODO

codeGenPat :: CPat -> X64 ()
codeGenPat = \case
  TagPat (Tag Grin.C tagname _) -> do
    lift $ mov rax $ case tagname of
      "True"  -> 1 -- FIXME: hardcode it for now
      "False" -> 0
      _ -> 2 -- TODO
  LitPat (LInt v) -> lift $ mov rax (fromIntegral v)

codeGenBinOp a b op = do
  -- load b value to register
  codeGenVal b
  lift $ mov rbx rax

  -- load a value to register
  codeGenVal a

  lift op -- do the work ; a in rax, b in rbx, result should go to rax

codeGen :: Exp -> Code
codeGen = void . flip runStateT emptyStackMap . para folder where
  folder :: ExpF (Exp, X64 ()) -> X64 ()
  folder = \case
    --ProgramF defs -> mapM id
    DefF name args (_,exp) -> do
      funLabel <- lift label
      modify' (\sm@StackMap{..} -> sm {defMap = Map.insert name funLabel defMap})

      clearLocalVarMap -- clean up local variable map
      forM_ (zip (reverse args) [0..]) $ \(arg, idx) -> do
        addLocalVar arg (-(idx + 2 + 1))
      rec
        lift $ do
          push rbp
          mov rbp rsp
          sub rsp (fromIntegral $ localVarCount * 8) -- create space for local variables

        exp

        localVarCount <- gets localCounter
      lift $ do
        mov rsp rbp
        pop rbp
        ret
      clearLocalVarMap -- clean up local variable map

    SReturnF val -> codeGenVal val

    EBindF (_,sexp) lpat (_,exp) -> do
      sexp
      case lpat of
        Var name -> do
          varStackIndex <- newLocalVariable name -- create local var
          -- copy val to var
          lift $ mov (bpRel varStackIndex) rax
        _ -> pure ()
      exp

    SFetchIF name (Just index) -> do
      nameStackIndex <- getStackIndex name
      lift $ do
        -- node address
        mov rsi (bpRel nameStackIndex)
        -- fetch node item
        mov rax (regRel rsi (8 * fromIntegral index))

    ECaseF val alts -> do
      codeGenVal val
      lift $ mov rdx rax -- val
      rec
        _ <- forM_ alts $ \((Alt pat _), exp) -> do
          codeGenPat pat -- will load the tag to rax
          lift $ cmp rax rdx
          rec _ <- lift $ j NZ not_match
              exp
              lift $ jmp exit
              not_match <- lift label
          pure ()
        exit <- lift label
      pure ()

    SAppF "intPrint" [a] -> codeGenVal a -- TODO

    SAppF "intAdd" [a, b] -> codeGenBinOp a b $ add rax rbx

    SAppF "intGT" [a, b] -> codeGenBinOp a b $ do
      mov rdx 1
      mov rcx rax
      xor_ rax rax
      sub rcx rbx
      cmov G rax rdx

    SAppF name args -> do
      {-
      dm <- gets defMap
      nameLabel <- Map.lookup name <$> gets defMap >>= \case
        Nothing -> fail $ "unknown function: " ++ name ++ " labels: " ++ show dm
        Just l -> pure l
      -}
      -- push arguments to stack
      forM_ args $ \val -> do
          codeGenVal val
          lift $ push rax

      lift $ do
        nameLabel <- label -- HACK
        call (ipRelValue nameLabel)
        -- remove arguments from stack
        add rsp (fromIntegral $ length args * 8)

    e -> sequence_ (snd <$> e)
