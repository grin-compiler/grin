{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
module Transformations.Rename where

import Data.Map (Map)
import Grin
import qualified Data.Map as Map
import Data.Functor.Foldable as Foldable


renameNames :: Map.Map Name Name -> Exp -> Exp
renameNames substituitons = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    Program  defs               -> ProgramF defs
    Def      name names exp     -> DefF (substName name) (substName <$> names) exp
    -- Exp
    EBind    simpleExp lpat exp -> EBindF simpleExp (subst lpat) exp
    ECase    val alts           -> ECaseF (subst val) alts
    -- Simple Exp
    SApp     name simpleVals    -> SAppF    (substName name) (subst <$> simpleVals)
    SReturn  val                -> SReturnF (subst val)
    SStore   val                -> SStoreF  (subst val)
    SFetchI  name index         -> SFetchIF  (substName name) index
    SUpdate  name val           -> SUpdateF (substName name) (subst val)
    SBlock   exp                -> SBlockF  exp
    -- Alt
    Alt pat exp                 -> AltF (substCPat pat) exp

  subst :: Val -> Val
  subst (Var name) = Var $ substName name
  subst (ConstTagNode tag simpleVals) = ConstTagNode (substTag tag) (subst <$> simpleVals)
  subst (ValTag tag)                  = ValTag (substTag tag)
  subst other                         = other

  substName :: Name -> Name
  substName old = case Map.lookup old substituitons of
    Nothing  -> old
    Just new -> new

  substTag :: Tag -> Tag
  substTag (Tag ttype name) = Tag ttype (substName name)

  substCPat :: CPat -> CPat
  substCPat = \case
    NodePat tag names -> NodePat (substTag tag) (substName <$> names)
    TagPat  tag       -> TagPat  (substTag tag)
    LitPat  lit       -> LitPat  lit
    DefaultPat        -> DefaultPat
