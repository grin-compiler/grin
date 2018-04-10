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


codegenGrin :: CodeGenerator
codegenGrin CodegenInfo{..} = do
  forM_ simpleDecls $ \(name, sdecl) -> do
    print name
    putStrLn $ ppShow sdecl
    putStr "\n"
  forM_ simpleDecls $ \x -> do
    print =<< (try @SomeException . printGrin . function $ snd x)
    putStrLn ""

program :: [(Idris.Name, SDecl)] -> Exp
program defs = Program $ map (function . snd) defs

function :: SDecl -> Exp
function (SFun fname params _int body) = Def (name fname) (map name params) (sexp (name fname) body)

sexp :: Name -> SExp -> Exp
sexp fname = \case
  SLet lvar0 sexp1 sexp2 ->
    EBind (sexp fname sexp2) (Var (lvar fname lvar0)) (sexp fname sexp1)

  Idris.SApp bool nm lvars  -> Grin.SApp (name nm) (map (Var . lvar fname) lvars)
  Idris.SUpdate lvar0 sexp0 -> Grin.SUpdate (lvar fname lvar0) (val fname sexp0)

  SCase caseType lvar0 salts -> ECase (Var $ lvar fname lvar0) (map (alt fname) salts)
  SChkCase lvar0 salts       -> ECase (Var $ lvar fname lvar0) (map (alt fname) salts)

  SProj lvar0 int -> SFetchI (lvar fname lvar0) (Just int)

  SOp primFn lvars -> undefined

  SCon maybeLVar int name lvars -> undefined
  SConst cnst -> undefined -- Value
  SV lvar0 -> undefined -- Value

  -- Keep DExps for describing foreign things, because they get
  -- translated differently
  SForeign fdesc1 fdesc2 fdescLVars -> undefined
  SNothing -> undefined -- erased value, will never be inspected  -> undefined
  SError string -> undefined

alt :: Name -> SAlt -> Exp
alt fname = \case
  SConCase int0 int1 nm names sexp0 -> Alt (NodePat (Tag C (name nm)) (map name names)) (sexp fname sexp0)
  SConstCase cnst sexp0 -> Alt (LitPat (literal cnst)) (sexp fname sexp0)
  SDefaultCase sexp0    -> Alt DefaultPat (sexp fname sexp0)

val :: Name -> SExp -> Val
val fname = \case
  SV lvar0 -> Var $ lvar fname lvar0
  SConst c -> Lit $ literal c
  SCon _ int nm lvars -> ConstTagNode (Tag C (name nm)) (map (Var . lvar fname) lvars)

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
  Idris.SN specialName -> undefined
  Idris.SymRef int -> "symref_" ++ show int

literal :: Idris.Const -> Lit
literal = \case
  Idris.I int -> LInt64 (fromIntegral int)
  Idris.BI integer -> LInt64 (fromIntegral integer)
  Idris.Fl double -> LFloat (realToFrac double) -- TODO
  Idris.Ch char -> undefined
  Idris.Str string -> undefined
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
