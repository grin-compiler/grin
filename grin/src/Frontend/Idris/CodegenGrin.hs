{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, TypeApplications #-}
module Frontend.Idris.CodegenGrin(codegenGrin) where

import Control.Monad
import Text.Show.Pretty hiding (Name)
import Text.Printf

import IRTS.CodegenCommon
import IRTS.Simplified as Idris
import IRTS.Lang as Idris
import Grin as Grin
import Data.Functor.Foldable
import qualified Data.Text as Text
import qualified Idris.Core.TT as Idris
import Data.List
import Pretty
import Control.Exception
import Control.Monad
import Debug.Trace
import Transformations.SingleStaticAssignment
import Transformations.BindNormalisation
import Pipeline
import Text.PrettyPrint.ANSI.Leijen (ondullblack)


{-
TODO:
 * Implement appropiate primitive ops
 * Optimization transformation that removed empty defaults, like pure ()
 * Implement String primitives
-}

codegenGrin :: CodeGenerator
codegenGrin CodegenInfo{..} = do
  forM_ simpleDecls $ \(name, sdecl) -> do
    print name
    putStrLn $ ppShow sdecl
    putStr "\n"
  let idrisGrin = program simpleDecls
  pipeline pipelineOpts idrisGrin idrisPipeLine
  putStrLn ""

program :: [(Idris.Name, SDecl)] -> Exp
program defs =
  bindNormalisation $
  singleStaticAssignment $
  renameMain $
  Program $ map (function . snd) defs

renameMain :: Exp -> Exp
renameMain (Program defs) = Program $ map newMain defs where
  newMain (Def "idr_{runMain_0}" [] body) = Def "grinMain" [] body
  newMain rest = rest

function :: SDecl -> Exp
function (SFun fname params _int body) =
  Def
    (name fname)
    (map (\(p, i) -> (name fname) ++ show i) (params `zip` [0..]))
    (sexp (name fname) body)

loc :: Name -> LVar -> Name
loc fname (Idris.Loc i) = fname ++ show i

sexp :: Name -> SExp -> Exp
sexp fname = \case

  SLet loc0@(Idris.Loc i) v sc ->
    EBind (SBlock (sexp fname v)) (Var (loc fname loc0)) (sexp fname sc)

  SLet lvar0 sexp1 sexp2 -> error "sexp: SLet with non local should not happen."

  Idris.SApp bool nm lvars  -> Grin.SApp (name nm) (map (Var . lvar fname) lvars)
  Idris.SUpdate lvar0 sexp0 -> Grin.SUpdate (lvar fname lvar0) (val fname sexp0)

  SCase caseType lvar0 salts -> ECase (Var $ lvar fname lvar0) (map (alt fname) salts)
  SChkCase lvar0 salts       -> ECase (Var $ lvar fname lvar0) (map (alt fname) salts)

  --SProj lvar0 int -> SFetchI (lvar fname lvar0) (Just int)

  SOp f lvars -> primFn f (map (Var . lvar fname) lvars)

  scon@(SCon maybeLVar int name lvars) -> SReturn $ val fname scon
  sconst@(SConst cnst) -> SReturn $ val fname sconst

  SV lvar0@(Idris.Loc i)  -> SReturn (Var $ loc fname lvar0)
  SV lvar0@(Idris.Glob n) -> traceShow "Global call" $ Grin.SApp (lvar fname lvar0) []

  -- Keep DExps for describing foreign things, because they get
  -- translated differently
  --SForeign fdesc1 fdesc2 fdescLVars -> undefined
  SForeign _ (FStr "_prim_int_print") [(_,arg)] -> Grin.SApp "_prim_int_print" [Var . lvar fname $ arg]
  SNothing -> traceShow "Erased value" $ SReturn Unit
  --SError string -> traceShow ("Error with:" ++ string) $ Grin.SApp "prim_error" []
  e -> error $ printf "unsupported %s" (show e)

alt :: Name -> SAlt -> Exp
alt fname = \case
  SConCase startIdx t nm names sexp0 ->
    Alt (NodePat (Tag C (name nm)) (map (\(i,_n) -> fname ++ show i) ([startIdx ..] `zip` names)))
        (sexp fname sexp0)

  SConstCase cnst sexp0 -> Alt (LitPat (literal cnst)) (sexp fname sexp0)
  SDefaultCase sexp0    -> Alt DefaultPat (sexp fname sexp0)

primFn :: Idris.PrimFn -> [SimpleVal] -> Exp
primFn f ps = case f of
  {-
  LPlus arithTy -> undefined
  LMinus arithTy -> undefined
  LTimes arithTy -> undefined
  LUDiv intTy -> undefined
  LSDiv arithTy -> undefined
  LURem intTy -> undefined
  LSRem arithTy -> undefined
  LAnd intTy -> undefined
  LOr intTy -> undefined
  LXOr intTy -> undefined
  LCompl intTy -> undefined
  LSHL intTy -> undefined
  LLSHR intTy -> undefined
  LASHR intTy -> undefined
  -}
  LEq (Idris.ATInt intTy) -> Grin.SApp "_prim_int_eq" ps
  LEq Idris.ATFloat       -> Grin.SApp "_prim_float_eq" ps
  {-
  LLt intTy -> undefined
  LLe intTy -> undefined
  LGt intTy -> undefined
  LGe intTy -> undefined
  -}
  LSLt (Idris.ATInt intTy) -> Grin.SApp "_prim_int_lt" ps
  LSLt Idris.ATFloat       -> Grin.SApp "_prim_float_lt" ps
  {-
  LSLe arithTy -> undefined
  LSGt arithTy -> undefined
  LSGe arithTy -> undefined
  LSExt intTy1 intTy2 -> undefined
  LZExt intTy1 intTy2 -> undefined
  LTrunc intTy1 intTy2 -> undefined
  LStrConcat -> Grin.SApp "_prim_int_add" ps -- TODO: Fix String
  LStrLt -> undefined
  LStrEq -> Grin.SApp "_prim_int_eq" ps -- TODO: Fix String
  LStrLen -> undefined
  LIntFloat intTy -> undefined
  LFloatInt intTy -> undefined
  LIntStr intTy -> Grin.SApp "_prim_int_add" $ [Lit (LInt64 2)] ++ ps -- TODO: Fix String
  LStrInt intTy -> undefined
  LFloatStr -> undefined
  LStrFloat -> undefined
  LChInt intTy -> undefined
  LIntCh intTy -> undefined
  LBitCast arithTy1 arithTy2 -> undefined -- Only for values of equal width
  LFExp -> undefined
  LFLog -> undefined
  LFSin -> undefined
  LFCos -> undefined
  LFTan -> undefined
  LFASin -> undefined
  LFACos -> undefined
  LFATan -> undefined
  LFATan2 -> undefined
  LFSqrt -> undefined
  LFFloor -> undefined
  LFCeil -> undefined
  LFNegate -> undefined
  LStrHead -> Grin.SApp "_prim_int_add" $ [Lit (LInt64 2)] ++ ps -- TODO: Fix String
  LStrTail -> Grin.SApp "_prim_int_add" $ [Lit (LInt64 3)] ++ ps -- TODO: Fix String
  LStrCons -> Grin.SApp "_prim_int_add" ps -- TODO: Fix String
  LStrIndex -> undefined
  LStrRev -> undefined
  LStrSubstr -> undefined
  LReadStr -> Grin.SApp "_prim_int_add" $ [Lit (LInt64 4)] ++ ps -- TODO: Fix String

  LWriteStr -> Grin.SApp "_prim_write_str" ps -- TODO: Fix String
-}
  LExternal name -> Grin.SApp (show name) ps
  {-
  LSystemInfo -> undefined
  LFork -> undefined
  LPar -> undefined -- evaluate argument anywhere, possibly on another -- core or another machine. 'id' is a valid implementation
  LExternal nm -> case ps of
    []  -> SReturn Unit
    [p] -> Grin.SApp "_prim_int_add" $ [Lit (LInt64 5), p]
    _   -> Grin.SApp "_prim_int_add" $ (take 2 ps)  -- TODO: Fix String
  LCrash -> undefined
  LNoOp -> undefined
  -}
  x -> error $ printf "unsupported primitive operation %s" (show x)

val :: Name -> SExp -> Val
val fname = \case
  SV lvar0 -> Var $ lvar fname lvar0
  SConst c -> Lit $ literal c
  SCon _ int nm lvars -> ConstTagNode (Tag C (name nm)) (map (Var . lvar fname) lvars)
  rest -> error $ show rest

lvar :: Name -> LVar -> Name
lvar fname = \case
  Idris.Loc loc -> fname ++ show loc
  Glob nm       -> name nm

name :: Idris.Name -> Name
name n = "idr_" ++ (Idris.showCG n)

literal :: Idris.Const -> Lit
literal = \case
  Idris.I int -> LInt64 (fromIntegral int)
  Idris.BI integer -> LInt64 (fromIntegral integer)
  {-
  Idris.Fl double -> traceShow ("TODO: literal sould implement Double " ++ show double) $ LFloat (realToFrac double)
  Idris.Ch char -> traceShow ("TODO: literal should implement Char" ++ show char) $ LInt64 (fromIntegral $ fromEnum char)
  Idris.Str string -> traceShow ("TODO: literal should implement String " ++ string) $ LInt64 1234
  -}
{-
  Idris.B8 word8 -> undefined
  Idris.B16 word16 -> undefined
  Idris.B32 word32 -> undefined
  Idris.B64 word64 -> LWord64 word64
  Idris.AType arithTy -> undefined
  Idris.StrType -> undefined
  Idris.WorldType -> undefined
  Idris.TheWorld -> undefined
  Idris.VoidType -> undefined
  Idris.Forgot -> undefined
-}
  x -> error $ printf "unsupported literal %s" (show x)

pipelineOpts :: PipelineOpts
pipelineOpts = PipelineOpts
  { _poOutputDir = "./idris/"
  }

idrisPipeLine :: [Pipeline]
idrisPipeLine =
  [ SaveGrin "FromIdris"
  , T DeadProcedureElimination
  , PrintGrin ondullblack
  , HPT CompileHPT
  , HPT PrintHPT
  , HPT RunHPTPure
  , HPT PrintHPTResult
  , T CaseSimplification
  , T SplitFetch
  , T RightHoistFetch
  , T ConstantFolding
  , T EvaluatedCaseElimination
  , T TrivialCaseElimination
  , T UpdateElimination
  , T CopyPropagation
  , T ConstantPropagation
--  , T SparseCaseOptimisation: Illegal type {}
  , T EvaluatedCaseElimination
  , T ConstantPropagation
--  , T CommonSubExpressionElimination: Illegal type {}
  , T CaseCopyPropagation
--  , T GeneralizedUnboxing: Illegal type: {}
--  , T ArityRaising: Illegal type: {}
  , SaveGrin "After"
  , PrintGrin ondullblack
  , SaveLLVM "high-level-opt-code"
  , JITLLVM
  ]
