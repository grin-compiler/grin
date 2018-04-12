{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, TypeApplications #-}
module Frontend.Idris.CodegenGrin(codegenGrin) where

import Control.Monad
import Text.Show.Pretty hiding (Name)

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
  singleStaticAssignment $
  bindNormalisation $
  singleStaticAssignment $
  Program $ map (function . snd) defs

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

  SProj lvar0 int -> SFetchI (lvar fname lvar0) (Just int)

  SOp f lvars -> primFn f (map (Var . lvar fname) lvars)

  scon@(SCon maybeLVar int name lvars) -> SReturn $ val fname scon
  sconst@(SConst cnst) -> SReturn $ val fname sconst

  SV lvar0@(Idris.Loc i)  -> SReturn (Var $ loc fname lvar0)
  SV lvar0@(Idris.Glob n) -> traceShow "Global call" $ Grin.SApp (lvar fname lvar0) []

  -- Keep DExps for describing foreign things, because they get
  -- translated differently
  SForeign fdesc1 fdesc2 fdescLVars -> undefined
  SNothing -> traceShow "Erased value" $ SReturn Unit
  SError string -> traceShow ("Error with:" ++ string) $ Grin.SApp "prim_error" []

alt :: Name -> SAlt -> Exp
alt fname = \case
  SConCase int0 int1 nm names sexp0 -> Alt (NodePat (Tag C (name nm)) (map name names)) (sexp fname sexp0)
  SConstCase cnst sexp0 -> Alt (LitPat (literal cnst)) (sexp fname sexp0)
  SDefaultCase sexp0    -> Alt DefaultPat (sexp fname sexp0)

primFn :: Idris.PrimFn -> [SimpleVal] -> Exp
primFn f ps = case f of
  LPlus arityTy -> undefined
  LMinus arityTy -> undefined
  LTimes arityTy -> undefined
  LUDiv intTy -> undefined
  LSDiv arityTy -> undefined
  LURem intTy -> undefined
  LSRem arityTy -> undefined
  LAnd intTy -> undefined
  LOr intTy -> undefined
  LXOr intTy -> undefined
  LCompl intTy -> Grin.SApp "prim_comp" ps
  LSHL intTy -> undefined
  LLSHR intTy -> undefined
  LASHR intTy -> undefined
  LEq arityTy -> Grin.SApp "prim_leq" ps
  LLt intTy -> undefined
  LLe intTy -> undefined
  LGt intTy -> undefined
  LGe intTy -> undefined
  LSLt arityTy -> Grin.SApp "prim_slt" ps
  LSLe arityTy -> undefined
  LSGt arityTy -> undefined
  LSGe arityTy -> undefined
  LSExt intTy1 intTy2 -> undefined
  LZExt intTy1 intTy2 -> undefined
  LTrunc intTy1 intTy2 -> undefined
  LStrConcat -> Grin.SApp "prim_str_concat" ps
  LStrLt -> undefined
  LStrEq -> Grin.SApp "prim_str_eq" ps
  LStrLen -> undefined
  LIntFloat intTy -> undefined
  LFloatInt intTy -> undefined
  LIntStr intTy -> Grin.SApp "prim_int_str" ps
  LStrInt intTy -> undefined
  LFloatStr -> undefined
  LStrFloat -> undefined
  LChInt intTy -> undefined
  LIntCh intTy -> undefined
  LBitCast arityTy1 arityTy2 -> undefined -- Only for values of equal width
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
  LStrHead -> Grin.SApp "prin_str_head" ps
  LStrTail -> Grin.SApp "prim_str_tail" ps
  LStrCons -> Grin.SApp "prim_str_cons" ps
  LStrIndex -> undefined
  LStrRev -> undefined
  LStrSubstr -> undefined
  LReadStr -> Grin.SApp "prim_read_str" ps
  LWriteStr -> Grin.SApp "prim_write_str" ps
  LSystemInfo -> undefined
  LFork -> undefined
  LPar -> undefined -- evaluate argument anywhere, possibly on another -- core or another machine. 'id' is a valid implementation
  LExternal nm -> Grin.SApp ("prim_ext_" ++ name nm) ps
  LCrash -> undefined
  LNoOp -> undefined

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

-- | Names are hierarchies of strings, describing scope (so no danger of
-- duplicate names, but need to be careful on lookup).
name :: Idris.Name -> Name
name = \case
  Idris.UN text -> "un_" ++ Text.unpack text
  Idris.NS nm namespace -> "ns_" ++ name nm ++ (concat $ intersperse "_" $ map Text.unpack namespace)
  Idris.MN int text -> concat ["mn_", show int, "_", Text.unpack text]
  Idris.SN sn -> "sn_" ++ specialName sn
  Idris.SymRef int -> "symref_" ++ show int

specialName :: Idris.SpecialName -> Name
specialName = \case
  Idris.WhereN int nm1 nm2 -> concat ["where_", show int, "_", name nm1, "_", name nm2]
  Idris.WithN int nm -> concat ["with_", show int, "_", name nm]
  Idris.ImplementationN nm texts -> concat $ ["impl_", name nm, "_"] ++ (intersperse "_" $ map Text.unpack texts)
  Idris.ParentN nm text -> undefined
  Idris.MethodN nm -> concat ["method_", name nm]
  Idris.CaseN fc nm -> concat ["case_", name nm] -- fc is source location
  Idris.ImplementationCtorN nm -> undefined
  Idris.MetaN nm1 nm2 -> undefined
  voidElim -> traceShow ("specialName got a value for void eliminator: " ++ show voidElim) $ "void_eliminator"

literal :: Idris.Const -> Lit
literal = \case
  Idris.I int -> LInt64 (fromIntegral int)
  Idris.BI integer -> LInt64 (fromIntegral integer)
  Idris.Fl double -> traceShow ("TODO: literal sould implement Double " ++ show double) $ LFloat (realToFrac double)
  Idris.Ch char -> traceShow ("TODO: literal should implement Char" ++ show char) $ LInt64 (fromIntegral $ fromEnum char)
  Idris.Str string -> traceShow ("TODO: literal should implement String " ++ string) $ LInt64 0
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

pipelineOpts :: PipelineOpts
pipelineOpts = PipelineOpts
  { _poOutputDir = "./idris/"
  }

idrisPipeLine :: [Pipeline]
idrisPipeLine =
  [ SaveGrin "FromIdris"
  , HPT CompileHPT
--  , HPT PrintHPT
  , PrintGrin ondullblack
  , T CaseSimplification
  , T SplitFetch
  , T RightHoistFetch
  , T ConstantFolding
  , T EvaluatedCaseElimination
  , T TrivialCaseElimination
  , T UpdateElimination
  , T CopyPropagation
--  , T ConstantPropagation: cpatToLPat fails
--  , T DeadProcedureElimination: grinMain
  , SaveGrin "After"
  ]
