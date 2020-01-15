{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Transformations.ExtendedSyntax.Conversion where

import Data.String
import Data.Functor.Foldable as Foldable

import qualified Data.Map    as M
import qualified Data.Vector as V

import Control.Monad
import Control.Monad.Identity

import Lens.Micro.Extra
import Lens.Micro.Platform

import Grin.Grin
import Grin.Pretty
import Grin.Syntax
import Grin.SyntaxDefs
import Grin.TypeEnvDefs
import qualified Grin.ExtendedSyntax.Pretty as New
import qualified Grin.ExtendedSyntax.Grin as New
import qualified Grin.ExtendedSyntax.Syntax as New
import qualified Grin.ExtendedSyntax.SyntaxDefs as New
import qualified Grin.ExtendedSyntax.TypeEnvDefs as New

import Transformations.Util
import Transformations.Names
import Transformations.BindNormalisation
import Transformations.Optimising.CopyPropagation
import Transformations.Optimising.SimpleDeadVariableElimination
import Transformations.Simplifying.ProducerNameIntroduction
import Transformations.Simplifying.BindingPatternSimplification


class Convertible a b where
  convert :: a -> b

instance Convertible TagType New.TagType where
  convert = \case
    C   -> New.C
    F   -> New.F
    P n -> New.P n

instance Convertible Name New.Name where
  convert = \case
    NM name -> New.NM name
    NI n    -> New.NI n

instance Convertible Tag New.Tag where
  convert Tag{..} = New.Tag (convert tagType) (convert tagName)

instance Convertible Lit New.Lit where
  convert = \case
    LInt64 n  -> New.LInt64 n
    LWord64 n -> New.LWord64 n
    LFloat f  -> New.LFloat f
    LBool b   -> New.LBool b
    LString s -> New.LString s
    LChar c   -> New.LChar c

instance Convertible SimpleType New.SimpleType where
  convert = \case
    T_Int64               -> New.T_Int64
    T_Word64              -> New.T_Word64
    T_Float               -> New.T_Float
    T_Bool                -> New.T_Bool
    T_Unit                -> New.T_Unit
    T_Location locs       -> New.T_Location locs
    T_UnspecifiedLocation -> New.T_UnspecifiedLocation
    T_Dead                -> New.T_Dead
    T_String              -> New.T_String
    T_Char                -> New.T_Char

instance Convertible Type New.Type where
  convert = \case
    T_SimpleType st -> New.T_SimpleType (convert st)
    T_NodeSet ns    -> New.T_NodeSet
      $ M.mapKeysMonotonic convert
      . M.map (V.map convert)
      $ ns
    _ -> error "convert: Dependent type constructors are not supported in the new syntax."

instance Convertible Ty New.Ty where
  convert = \case
    TyCon name tys -> New.TyCon (convert name) (map convert tys)
    TyVar name     -> New.TyVar (convert name)
    TySimple st    -> New.TySimple (convert st)

instance Convertible ExternalKind New.ExternalKind where
  convert = \case
    PrimOp -> New.PrimOp
    FFI    -> New.FFI

instance Convertible External New.External where
  convert External{..} = New.External
    (convert eName)
    (convert eRetType)
    (map convert eArgsType)
    eEffectful
    (convert eKind)

instance Convertible CPat New.CPat where
  convert = \case
    NodePat t args -> New.NodePat (convert t) (map convert args)
    LitPat l       -> New.LitPat (convert l)
    DefaultPat     -> New.DefaultPat
    TagPat _ -> error "convert: Tag patterns are not supported in the new syntax."

oldNodeToNewNode :: Tag -> [Val] -> New.Val
oldNodeToNewNode tag vals
  | any (isn't _Var) vals = error $ "ConstTagNode " ++ show (PP $ ConstTagNode tag vals) ++ " has a non-variable argument."
  | otherwise             = New.ConstTagNode (convert tag) (map (convert . view _Var) vals)

oldNodePatToAsPat :: Tag -> [Val] -> Name -> NameM New.BPat
oldNodePatToAsPat tag args name = do
  args' <- forM args $ \case
    Var v -> pure $ convert v
    {- NOTE: Unit and Lit patterns can be "skipped". If the variable holds
       the same value as we are matching against, then it redundant. If it does
       not, then the semantics of the program is undefined (so we can do anything with it).

       Here we will just generate a variable pattern in place of literal and unit patterns.
    -}
    _ -> convert <$> deriveWildCard
  let tag'  = convert tag
      name' = convert name
  pure $ New.AsPat tag' args' name'

instance Convertible Val New.Val where
  -- NOTE: This should only be called for node values, but not for node patterns in LPats
  convert (ConstTagNode tag vals)
    | any (isn't _Var) vals = error $ "ConstTagNode " ++ show (PP $ ConstTagNode tag vals) ++ " has a non-variable argument."
    | otherwise             = New.ConstTagNode (convert tag) (map (convert . view _Var) vals)
  convert v@(VarTagNode _ _) = error $ "Cannot transform VarTagNode to new syntax: " ++ show (PP v)
  convert v@(ValTag _)       = error $ "Cannot transform ValTag to new syntax: " ++ show (PP v)
  convert Unit          = New.Unit
  convert (Lit l)       = New.Lit (convert l)
  convert (Var v)       = New.Var (convert v)
  convert (Undefined t) = New.Undefined (convert t)

instance Convertible Exp New.Exp where
  convert exp = fst $ evalNameM exp $ flip anaM exp $ \case
    (Program exts defs)  -> pure $ New.ProgramF (map convert exts) defs
    (Def name args body) -> pure $ New.DefF (convert name) (map convert args) body

    {- NOTE: We assume Binding Pattern Simplification has been run,
       and the value has been given the name v.0. This is a special
       case of the next program patterns. This one transforms the result
       of Binding Pattern Simplification to a more concise form.

      v.0 <- pure <value>
      <node pat> <- pure v.0
      <rhs2>

      <node pat> @ v.0 <- pure <value>
      <rhs2>
    -}
    (EBind lhs1 (Var var) rhs1)
      | EBind (SReturn (Var var')) (ConstTagNode tag args) rhs2 <- rhs1
      , var == var'
      -> do
         newNodePat <- oldNodePatToAsPat tag args var
         pure $ New.EBindF lhs1 newNodePat rhs2
    {- NOTE: The following transformation can be done, because
       unit and literal patterns are redundant. If the variable has
       the same value as the pattern, then we can safely remove the
       binding. If the variable holds some value different from the pattern,
       then the program's behaviour is undefined, so we can do anything
       with it.


      v.0 <- pure <value>
      <pat> <- pure v.0     -- pat is neither a node pat nor a var pat
      <rhs2>

      v.0 <- pure <value>
      <rhs2>
    -}
      | EBind (SReturn (Var var')) valPat rhs2 <- rhs1
      , isn't _Var valPat
      , var == var'
      -> pure $ New.EBindF lhs1 (New.VarPat $ convert var) rhs2
    {- NOTE: In this case, v.0 has been defined earlier in the program.
       This is a more general case that covers the one before as well.

      v.0 <- pure <value>
      <...>
      <node pat> <- pure v.0
      <rhs>

      v.0 <- pure <value>
      <...>
      <node pat> @ a.0 <- pure v.0    -- a.0 is a fresh variable
      <rhs>
    -}
    (EBind lhs (ConstTagNode tag args) rhs) -> do
      asPatName  <- deriveNewName "a"
      newNodePat <- oldNodePatToAsPat tag args asPatName
      pure $ New.EBindF lhs newNodePat rhs
    (EBind lhs (Var var) rhs)
      -> pure $ New.EBindF lhs (New.VarPat $ convert var) rhs
    (EBind lhs pat@Lit{} rhs) -> do
      patName <- deriveNewName "a"
      pure $ New.EBindF lhs (New.VarPat $ convert patName) rhs
    (EBind lhs pat@Unit rhs) -> do
      patName <- deriveWildCard
      pure $ New.EBindF lhs (New.VarPat $ convert patName) rhs
    (ECase scrut alts)
      | isn't _Var scrut   -> error $ "Non-variable pattern in case scrutinee: " ++ show (PP scrut)
      | (Var var) <- scrut -> pure $ New.ECaseF (convert var) alts
    e@(SApp f vals)
      | any (isn't _Var) vals -> error $ "Non-variable value in application: " ++ show (PP e)
      | otherwise             -> pure $ New.SAppF (convert f) $ map (convert . view _Var) vals
    e@(SStore val)
      | isn't _Var val   -> error $ "Non-variable value in store: " ++ show (PP e)
      | (Var var) <- val -> pure $ New.SStoreF (convert var)
    e@(SFetchI ptr mIx)
      | Nothing <- mIx -> pure $ New.SFetchF (convert ptr)
      | otherwise      -> error $ "Indexed fetch is no longer supported: " ++ show (PP e)
    e@(SUpdate ptr val)
      | isn't _Var val   -> error $ "Non-variable value in update: " ++ show (PP e)
      | (Var var) <- val -> pure $ New.SUpdateF (convert ptr) (convert var)
    (SReturn val)  -> pure $ New.SReturnF (convert val)
    (SBlock exp)   -> pure $ New.SBlockF exp
    (Alt cpat exp) -> do
      altName <- deriveNewName "alt"
      pure $ New.AltF (convert cpat) (convert altName) exp
    e -> error $ "Cannot convert to new: " ++ show (PP e)

instance Convertible New.TagType TagType where
  convert = \case
    New.C   -> C
    New.F   -> F
    New.P n -> P n

instance Convertible New.Name Name where
  convert = \case
    New.NM name -> NM name
    New.NI n    -> NI n

instance Convertible New.Tag Tag where
  convert New.Tag{..} = Tag (convert tagType) (convert tagName)

instance Convertible New.Lit Lit where
  convert = \case
    New.LInt64 n  -> LInt64 n
    New.LWord64 n -> LWord64 n
    New.LFloat f  -> LFloat f
    New.LBool b   -> LBool b
    New.LString s -> LString s
    New.LChar c   -> LChar c

instance Convertible New.SimpleType SimpleType where
  convert = \case
    New.T_Int64               -> T_Int64
    New.T_Word64              -> T_Word64
    New.T_Float               -> T_Float
    New.T_Bool                -> T_Bool
    New.T_Unit                -> T_Unit
    New.T_Location locs       -> T_Location locs
    New.T_UnspecifiedLocation -> T_UnspecifiedLocation
    New.T_Dead                -> T_Dead
    New.T_String              -> T_String
    New.T_Char                -> T_Char

instance Convertible New.Type Type where
  convert = \case
    New.T_SimpleType st -> T_SimpleType (convert st)
    New.T_NodeSet ns    -> T_NodeSet
      $ M.mapKeysMonotonic convert
      . M.map (V.map convert)
      $ ns

instance Convertible New.Ty Ty where
  convert = \case
    New.TyCon name tys -> TyCon (convert name) (map convert tys)
    New.TyVar name     -> TyVar (convert name)
    New.TySimple st    -> TySimple (convert st)

instance Convertible New.ExternalKind ExternalKind where
  convert = \case
    New.PrimOp -> PrimOp
    New.FFI    -> FFI

instance Convertible New.External External where
  convert New.External{..} = External
    (convert eName)
    (convert eRetType)
    (map convert eArgsType)
    eEffectful
    (convert eKind)

instance Convertible New.CPat CPat where
  convert = \case
    New.NodePat t args -> NodePat (convert t) (map convert args)
    New.LitPat l       -> LitPat (convert l)
    New.DefaultPat     -> DefaultPat

instance Convertible New.Val Val where
  convert (New.ConstTagNode t vars) = ConstTagNode (convert t) $ map (Var . convert) vars
  convert (New.Unit)  = Unit
  convert (New.Lit l) = Lit (convert l)
  convert (New.Var v) = Var (convert v)
  convert (New.Undefined t) = Undefined (convert t)

instance Convertible New.Exp Exp where
  convert (New.Program exts defs)  = Program (map convert exts) (map convert defs)
  convert (New.Def name args body) = Def (convert name) (map convert args) (convert body)
  convert e@(New.EBind lhs pat rhs)
    | (New.VarPat v)         <- pat = EBind (convert lhs) (Var $ convert v) (convert rhs)
    | (New.AsPat tag args v) <- pat -- condition
    , rhs' <- EBind (SReturn (Var $ convert v)) (ConstTagNode (convert tag) (map (Var . convert) args)) (convert rhs) -- helper
    = EBind (convert lhs) (Var $ convert v) rhs'
  convert e@(New.ECase scrut alts) = ECase (Var $ convert scrut) (map convert alts)
  convert (New.SApp f vars)        = SApp (convert f) $ map (Var . convert) vars
  convert (New.SStore var)         = SStore (Var $ convert var)
  convert (New.SFetch ptr)         = SFetchI (convert ptr) Nothing
  convert (New.SUpdate ptr var)    = SUpdate (convert ptr) (Var $ convert var)
  convert (New.SReturn val)        = SReturn (convert val)
  convert (New.SBlock exp)         = SBlock (convert exp)
  -- TODO: This transformation is not sound if the body contains a reference to the alt name.
  convert (New.Alt cpat _ exp)     = Alt (convert cpat) (convert exp)

convertToNew :: Exp -> New.Exp
convertToNew = convert . nameEverything

nameEverything :: Exp -> Exp
nameEverything
  = copyPropagation
  . bindNormalisation
  . nodeArgumentNaming
  . bindNormalisation
  . appArgumentNaming
  . bindNormalisation
  . fst . bindingPatternSimplification
  . bindNormalisation
  . fst . producerNameIntroduction
  . bindNormalisation

appArgumentNaming :: Exp -> Exp
appArgumentNaming e = fst . evalNameM e . cata alg $ e where
  alg :: ExpF (NameM Exp) -> NameM Exp
  alg e = case e of
    SAppF f args -> bindFunArgs f args
    expf -> fmap embed . sequence $ expf

  bindFunArgs :: Name -> [Val] -> NameM Exp
  bindFunArgs f args = do
    varArgs <- forM [1..length args] $ \_ ->
      Var <$> newArgName
    let g exp (arg, var) = EBind (SReturn arg) var exp
        boundApp = foldl g (SApp f varArgs) $ zip args varArgs
    pure $ SBlock boundApp

  newArgName :: NameM Name
  newArgName = deriveNewName "x"

-- NOTE: we can assume that Producer Name Introduction
-- & Binding Pattern Simplification has already been run
-- ConstTagNodes can only appear in SReturns
nodeArgumentNaming :: Exp -> Exp
nodeArgumentNaming e = fst . evalNameM e . cata alg $ e where
  alg :: ExpF (NameM Exp) -> NameM Exp
  alg e = case e of
    SReturnF (ConstTagNode tag args) -> bindNodeArgs tag args
    expf -> fmap embed . sequence $ expf

  bindNodeArgs :: Tag -> [Val] -> NameM Exp
  bindNodeArgs tag args = do
    varArgs <- forM [1..length args] $ \_ ->
      Var <$> newArgName
    let g exp (arg, var) = EBind (SReturn arg) var exp
        boundApp = foldl g (SReturn $ ConstTagNode tag varArgs) $ zip args varArgs
    pure $ SBlock boundApp

  newArgName :: NameM Name
  newArgName = deriveNewName "y"
