{-# LANGUAGE CPP #-}
module GhcDump_Convert where

import Data.Bifunctor
import qualified Data.ByteString.Char8 as BS8

import Literal (Literal(..))
import Var (Var)
import qualified Var
import Id (isFCallId)
import Module (ModuleName, moduleNameFS, moduleName)
import Unique (Unique, getUnique, unpkUnique)
import Name (getOccName, occNameFS, OccName, getName, nameModule_maybe)
import OccName
import qualified IdInfo
import qualified BasicTypes as OccInfo (OccInfo(..), isStrongLoopBreaker)
#if MIN_VERSION_ghc(8,0,0)
import qualified CoreStats
#else
import qualified CoreUtils as CoreStats
#endif
import qualified CoreSyn
import CoreSyn (Expr(..), CoreExpr, Bind(..), CoreAlt, CoreBind, AltCon(..))
import HscTypes (ModGuts(..), CgGuts(..))
import FastString (FastString, fastStringToByteString)
#if MIN_VERSION_ghc(8,0,0)
import TyCoRep as Type (Type(..), TyBinder(..))
#else
import TypeRep as Type (Type(..))
#endif
import Type (splitFunTy_maybe)
import TyCon (TyCon, tyConUnique)

import Outputable (ppr, showSDoc, SDoc)
import DynFlags (unsafeGlobalDynFlags)

import GhcDump_Ast as Ast

cvtSDoc :: SDoc -> T_Text
cvtSDoc = BS8.pack . showSDoc unsafeGlobalDynFlags

fastStringToText :: FastString -> T_Text
fastStringToText = fastStringToByteString

occNameToText :: OccName -> T_Text
occNameToText = fastStringToByteString . occNameFS

cvtUnique :: Unique.Unique -> Ast.Unique
cvtUnique u =
    let (a,b) = unpkUnique u
    in Ast.Unique a b

cvtVar :: Var -> BinderId
cvtVar = BinderId . cvtUnique . Var.varUnique

cvtBinder :: Var -> SBinder
cvtBinder v
  | Var.isId v =
    SBndr $ Binder { binderName   = occNameToText $ getOccName v
                   , binderId     = cvtVar v
                   , binderIdInfo = cvtIdInfo $ Var.idInfo v
                   , binderIdDetails = cvtIdDetails $ Var.idDetails v
                   , binderType   = cvtType $ Var.varType v
                   }
  | otherwise =
    SBndr $ TyBinder { binderName   = occNameToText $ getOccName v
                     , binderId     = cvtVar v
                     , binderKind   = cvtType $ Var.varType v
                     }

cvtIdInfo :: IdInfo.IdInfo -> Ast.IdInfo SBinder BinderId
cvtIdInfo i =
    IdInfo { idiArity         = IdInfo.arityInfo i
           , idiIsOneShot     = IdInfo.oneShotInfo i == IdInfo.OneShotLam
           , idiUnfolding     = cvtUnfolding $ IdInfo.unfoldingInfo i
           , idiInlinePragma  = cvtSDoc $ ppr $ IdInfo.inlinePragInfo i
           , idiOccInfo       = case IdInfo.occInfo i of
#if MIN_VERSION_ghc(8,2,0)
                                  OccInfo.ManyOccs{} -> OccManyOccs
#else
                                  OccInfo.NoOccInfo  -> OccManyOccs
#endif
                                  OccInfo.IAmDead    -> OccDead
                                  OccInfo.OneOcc{}   -> OccOneOcc
                                  oi@OccInfo.IAmALoopBreaker{} -> OccLoopBreaker (OccInfo.isStrongLoopBreaker oi)
           , idiStrictnessSig = cvtSDoc $ ppr $ IdInfo.strictnessInfo i
           , idiDemandSig     = cvtSDoc $ ppr $ IdInfo.demandInfo i
           , idiCallArity     = IdInfo.callArityInfo i
           }

cvtUnfolding :: CoreSyn.Unfolding -> Ast.Unfolding SBinder BinderId
cvtUnfolding CoreSyn.NoUnfolding = Ast.NoUnfolding
#if MIN_VERSION_ghc(8,2,0)
cvtUnfolding CoreSyn.BootUnfolding = Ast.BootUnfolding
#endif
cvtUnfolding (CoreSyn.OtherCon cons) = Ast.OtherCon (map cvtAltCon cons)
cvtUnfolding (CoreSyn.DFunUnfolding{}) = Ast.DFunUnfolding
cvtUnfolding u@(CoreSyn.CoreUnfolding{}) =
    Ast.CoreUnfolding { unfTemplate   = cvtExpr $ CoreSyn.uf_tmpl u
                      , unfIsValue    = CoreSyn.uf_is_value u
                      , unfIsConLike  = CoreSyn.uf_is_conlike u
                      , unfIsWorkFree = CoreSyn.uf_is_work_free u
                      , unfGuidance   = cvtSDoc $ ppr $ CoreSyn.uf_guidance u
                      }

cvtIdDetails :: IdInfo.IdDetails -> Ast.IdDetails
cvtIdDetails d =
    case d of
      IdInfo.VanillaId -> Ast.VanillaId
      IdInfo.RecSelId{} -> Ast.RecSelId
      IdInfo.DataConWorkId{} -> Ast.DataConWorkId
      IdInfo.DataConWrapId{} -> Ast.DataConWrapId
      IdInfo.ClassOpId{} -> Ast.ClassOpId
      IdInfo.PrimOpId{} -> Ast.PrimOpId
      IdInfo.FCallId{} -> error "This shouldn't happen"
      IdInfo.TickBoxOpId{} -> Ast.TickBoxOpId
      IdInfo.DFunId{} -> Ast.DFunId
#if MIN_VERSION_ghc(8,0,0)
      IdInfo.CoVarId{} -> Ast.CoVarId
#endif
#if MIN_VERSION_ghc(8,2,0)
      IdInfo.JoinId n -> Ast.JoinId n
#endif

cvtCoreStats :: CoreStats.CoreStats -> Ast.CoreStats
cvtCoreStats stats =
    Ast.CoreStats
      { csTerms     = CoreStats.cs_tm stats
      , csTypes     = CoreStats.cs_ty stats
      , csCoercions = CoreStats.cs_co stats
#if MIN_VERSION_ghc(8,2,0)
      , csValBinds  = CoreStats.cs_vb stats
      , csJoinBinds = CoreStats.cs_jb stats
#else
      , csValBinds  = 0
      , csJoinBinds = 0
#endif
      }

exprStats :: CoreExpr -> CoreStats.CoreStats
#if MIN_VERSION_ghc(8,0,0)
exprStats = CoreStats.exprStats
#else
-- exprStats wasn't exported in 7.10
exprStats _ = CoreStats.CS 0 0 0
#endif

cvtTopBind :: CoreBind -> STopBinding
cvtTopBind (NonRec b e) =
    NonRecTopBinding (cvtBinder b) (cvtCoreStats $ exprStats e) (cvtExpr e)
cvtTopBind (Rec bs) =
    RecTopBinding $ map to bs
  where to (b, e) = (cvtBinder b, cvtCoreStats $ exprStats e, cvtExpr e)

cvtExpr :: CoreExpr -> Ast.SExpr
cvtExpr expr =
  case expr of
    Var x
        -- foreign calls are local but have no binding site.
        -- TODO: use hasNoBinding here.
      | isFCallId x   -> EVarGlobal ForeignCall
      | Just m <- nameModule_maybe $ getName x
                      -> EVarGlobal $ ExternalName (cvtModuleName $ Module.moduleName m)
                                                   (occNameToText $ getOccName x)
                                                   (cvtUnique $ getUnique x)
                                                   (cvtIdDetails $ Var.idDetails x)
                                                   (IdInfo.arityInfo      $ Var.idInfo x)
                                                   (IdInfo.callArityInfo  $ Var.idInfo x)
                                                   (Var.isTyVar x)
      | otherwise     -> EVar (cvtVar x)
    Lit l             -> ELit (cvtLit l)
    App x y           -> EApp (cvtExpr x) (cvtExpr y)
    Lam x e
      | Var.isTyVar x -> ETyLam (cvtBinder x) (cvtExpr e)
      | otherwise     -> ELam (cvtBinder x) (cvtExpr e)
    Let (NonRec b e) body -> ELet [(cvtBinder b, cvtExpr e)] (cvtExpr body)
    Let (Rec bs) body -> ELet (map (bimap cvtBinder cvtExpr) bs) (cvtExpr body)
    Case e x _ as     -> ECase (cvtExpr e) (cvtBinder x) (map cvtAlt as)
    Cast x _          -> cvtExpr x
    Tick _ e          -> cvtExpr e
    Type t            -> EType $ cvtType t
    Coercion _        -> ECoercion

cvtAlt :: CoreAlt -> Ast.SAlt
cvtAlt (con, bs, e) = Alt (cvtAltCon con) (map cvtBinder bs) (cvtExpr e)

cvtAltCon :: CoreSyn.AltCon -> Ast.AltCon
cvtAltCon (DataAlt altcon) = Ast.AltDataCon (fastStringToText . moduleNameFS . Module.moduleName <$> (nameModule_maybe $ getName altcon)) $ occNameToText $ getOccName altcon
cvtAltCon (LitAlt l)       = Ast.AltLit $ cvtLit l
cvtAltCon DEFAULT          = Ast.AltDefault

cvtLit :: Literal -> Ast.Lit
cvtLit l =
    case l of
      Literal.MachChar x -> Ast.MachChar x
      Literal.MachStr x -> Ast.MachStr x
      Literal.MachNullAddr -> Ast.MachNullAddr
      Literal.MachInt x -> Ast.MachInt x
      Literal.MachInt64 x -> Ast.MachInt64 x
      Literal.MachWord x -> Ast.MachWord x
      Literal.MachWord64 x -> Ast.MachWord64 x
      Literal.MachFloat x -> Ast.MachFloat x
      Literal.MachDouble x -> Ast.MachDouble x
      Literal.MachLabel x _ _ -> Ast.MachLabel $ fastStringToText  x
      Literal.LitInteger x _ -> Ast.LitInteger x

cvtModule :: String -> ModGuts -> Ast.SModule
cvtModule phase guts =
    Ast.Module name (BS8.pack phase) (map cvtTopBind $ mg_binds guts)
  where name = cvtModuleName $ Module.moduleName $ mg_module guts

cvtModuleCg :: String -> CgGuts -> Ast.SModule
cvtModuleCg phase guts =
    Ast.Module name (BS8.pack phase) (map cvtTopBind $ cg_binds guts)
  where name = cvtModuleName $ Module.moduleName $ cg_module guts

cvtModuleName :: Module.ModuleName -> Ast.ModuleName
cvtModuleName = Ast.ModuleName . fastStringToText . moduleNameFS

cvtType :: Type.Type -> Ast.SType
cvtType t
  | Just (a,b) <- splitFunTy_maybe t = Ast.FunTy (cvtType a) (cvtType b)
cvtType (Type.TyVarTy v)       = Ast.VarTy (cvtVar v)
cvtType (Type.AppTy a b)       = Ast.AppTy (cvtType a) (cvtType b)
cvtType (Type.TyConApp tc tys) = Ast.TyConApp (cvtTyCon tc) (map cvtType tys)
#if MIN_VERSION_ghc(8,2,0)
cvtType (Type.ForAllTy (Var.TvBndr b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
#elif MIN_VERSION_ghc(8,0,0)
cvtType (Type.ForAllTy (Named b _) t) = Ast.ForAllTy (cvtBinder b) (cvtType t)
cvtType (Type.ForAllTy (Anon _) t)    = cvtType t
#else
cvtType (Type.ForAllTy b t)    = Ast.ForAllTy (cvtBinder b) (cvtType t)
#endif
cvtType (Type.LitTy _)         = Ast.LitTy
#if MIN_VERSION_ghc(8,0,0)
cvtType (Type.CastTy t _)      = cvtType t
cvtType (Type.CoercionTy _)    = Ast.CoercionTy
#endif

cvtTyCon :: TyCon.TyCon -> Ast.TyCon
cvtTyCon tc = TyCon (occNameToText $ getOccName tc) (cvtUnique $ tyConUnique tc)
