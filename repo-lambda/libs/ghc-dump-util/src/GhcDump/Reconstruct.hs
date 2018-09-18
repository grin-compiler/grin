{-# LANGUAGE RecordWildCards #-}
module GhcDump.Reconstruct (reconModule) where

import Data.Foldable
import Data.Bifunctor
import Prelude hiding (readFile)

import Data.Hashable
import qualified Data.HashMap.Lazy as HM

import GhcDump_Ast

newtype BinderMap = BinderMap (HM.HashMap BinderId Binder)

instance Hashable BinderId where
    hashWithSalt salt (BinderId (Unique c i)) = salt `hashWithSalt` c `hashWithSalt` i

emptyBinderMap :: BinderMap
emptyBinderMap = BinderMap mempty

insertBinder :: Binder -> BinderMap -> BinderMap
insertBinder (Bndr b) (BinderMap m) = BinderMap $ HM.insert (binderId b) (Bndr b) m

insertBinders :: [Binder] -> BinderMap -> BinderMap
insertBinders bs bm = foldl' (flip insertBinder) bm bs

getBinder :: BinderMap -> BinderId -> Binder
getBinder (BinderMap m) bid
  | Just b <- HM.lookup bid m = b
  | otherwise                 = error $ "unknown binder "++ show bid ++ ":\nin scope:\n"
                                        ++ unlines (map (\(bid',b) -> show bid' ++ "\t" ++ show b) (HM.toList m))

-- "recon" == "reconstruct"

reconModule :: SModule -> Module
reconModule m = Module (moduleName m) (modulePhase m) binds
  where
    binds = map reconTopBinding $ moduleTopBindings m
    bm = insertBinders (map (\(a,_,_) -> a) $ concatMap topBindings binds) emptyBinderMap

    reconTopBinding :: STopBinding -> TopBinding
    reconTopBinding (NonRecTopBinding b stats rhs) = NonRecTopBinding b' stats (reconExpr bm rhs)
      where b' = reconBinder bm b
    reconTopBinding (RecTopBinding bs) = RecTopBinding bs'
      where bs' = map (\(a,stats,rhs) -> (reconBinder bm a, stats, reconExpr bm rhs)) bs

reconExpr :: BinderMap -> SExpr -> Expr
reconExpr bm (EVar var)       = EVar $ getBinder bm var
reconExpr _  (EVarGlobal n)   = EVarGlobal n
reconExpr _  (ELit l)         = ELit l
reconExpr bm (EApp x y)       = EApp (reconExpr bm x) (reconExpr bm y)
reconExpr bm (ETyLam b x)     = let b' = reconBinder bm b
                                    bm' = insertBinder b' bm
                                in ETyLam b' (reconExpr bm' x)
reconExpr bm (ELam b x)       = let b' = reconBinder bm b
                                    bm' = insertBinder b' bm
                                in ELam b' (reconExpr bm' x)
reconExpr bm (ELet bs x)      = let bs' = map (bimap (reconBinder bm) (reconExpr bm')) bs
                                    bm' = insertBinders (map fst bs') bm
                                in ELet bs' (reconExpr bm' x)
reconExpr bm (ECase x b alts) = let b' = reconBinder bm b
                                    bm' = insertBinder b' bm
                                in ECase (reconExpr bm x) b' (map (reconAlt bm') alts)
reconExpr bm (EType t)        = EType (reconType bm t)
reconExpr _  ECoercion        = ECoercion

reconBinder :: BinderMap -> SBinder -> Binder
reconBinder bm (SBndr b@Binder{}) =
    Bndr $ b { binderType = reconType bm $ binderType b
             , binderIdInfo = reconIdInfo bm $ binderIdInfo b
             }
reconBinder bm (SBndr b@TyBinder{}) =
    Bndr $ b { binderKind = reconType bm $ binderKind b }

reconIdInfo :: BinderMap -> IdInfo SBinder BinderId -> IdInfo Binder Binder
reconIdInfo bm i =
    i { idiUnfolding = reconUnfolding bm $ idiUnfolding i }

reconUnfolding :: BinderMap -> Unfolding SBinder BinderId -> Unfolding Binder Binder
reconUnfolding _  NoUnfolding = NoUnfolding
reconUnfolding _  BootUnfolding = BootUnfolding
reconUnfolding _  (OtherCon alts) = OtherCon alts
reconUnfolding _  DFunUnfolding   = DFunUnfolding
reconUnfolding bm CoreUnfolding{..} = CoreUnfolding { unfTemplate = reconExpr bm unfTemplate
                                                    , .. }

reconAlt :: BinderMap -> SAlt -> Alt
reconAlt bm0 (Alt con bs rhs) =
    let (bm', bs') = doBinders bm0 [] bs
    in Alt con bs' (reconExpr bm' rhs)
  where
    doBinders bm acc []       = (bm, reverse acc)
    doBinders bm acc (b:rest) = doBinders bm' (b':acc) rest
      where
        b'  = reconBinder bm b
        bm' = insertBinder b' bm

reconType :: BinderMap -> SType -> Type
reconType bm (VarTy v) = VarTy $ getBinder bm v
reconType bm (FunTy x y) = FunTy (reconType bm x) (reconType bm y)
reconType bm (TyConApp tc tys) = TyConApp tc (map (reconType bm) tys)
reconType bm (AppTy x y) = AppTy (reconType bm x) (reconType bm y)
reconType bm (ForAllTy b x) = let b' = reconBinder bm b
                                  bm' = insertBinder b' bm
                              in ForAllTy b' (reconType bm' x)
reconType _  LitTy = LitTy
reconType _  CoercionTy = CoercionTy
