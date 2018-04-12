{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Playground where

import Data.Maybe
import Data.List (intercalate)
import Data.Set (Set, singleton, toList)
import qualified Data.Map as Map
import Data.Monoid hiding (Alt)
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Gen
import Control.Monad.Writer hiding (Alt)
import Data.Functor.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Foldable
import Control.Comonad.Cofree

import Grin

countStores :: Exp -> Int
countStores = cata folder where
  folder = \case
    SStoreF {} -> 1
    e -> Data.Foldable.sum e


collectTagInfo2 :: Exp -> Set Tag
collectTagInfo2 = execWriter . cata folder where
  folder = \case
    -- Exp
    ECaseF val alts -> add val >> sequence_ alts
    -- Simple Exp
    SReturnF  val   -> add val
    SStoreF   val   -> add val
    SUpdateF  _ val -> add val
    e -> sequence_ e

  add :: Val -> Writer (Set Tag) ()
  add = \case
    ConstTagNode (Tag tagtype name) args -> tell $ singleton (Tag tagtype name)
    ValTag tag            -> tell $ singleton tag
    _ -> pure ()


collectTagInfoPure :: Exp -> Set Tag
collectTagInfoPure = cata folder where
  folder = \case
    ProgramF a      -> mconcat a
    DefF _ _ a      -> a
    -- Exp
    EBindF    a _ b -> a <> b
    ECaseF val alts -> mconcat $ add val : alts
    -- Simple Exp
    SAppF     name vals -> mconcat $ map add vals
    SReturnF  val   -> add val
    SStoreF   val   -> add val
    SUpdateF  _ val -> add val
    SFetchIF  _ _   -> mempty
    SBlockF   a     -> a
    -- Alt
    AltF _ a        -> a

  add = \case
    ConstTagNode (Tag tagtype name) args -> singleton (Tag tagtype name)
    ValTag tag          -> singleton tag
    _                   -> mempty


renameVaribales :: Map.Map Name Name -> Exp -> Exp
renameVaribales substituitons = ana builder where
  builder :: Exp -> ExpF Exp
  builder = \case
    Program  defs               -> ProgramF defs
    Def      name names exp     -> DefF name (substName <$> names) exp
    -- Exp
    EBind    simpleExp lpat exp -> EBindF simpleExp (subst lpat) exp
    ECase    val alts           -> ECaseF (subst val) alts
    -- Simple Exp
    SApp     name simpleVals    -> SAppF    (substName name) (subst <$> simpleVals)
    SReturn  val                -> SReturnF (subst val)
    SStore   val                -> SStoreF  (subst val)
    SFetchI  name index         -> SFetchIF  name index
    SUpdate  name val           -> SUpdateF name (subst val)
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


idAna :: Exp -> Exp
idAna e = ana builder ([], e) where
  builder :: (String, Exp) -> ExpF (String, Exp)
  builder (path, exp) =
    case exp of
      SStore val -> SStoreF val
      e -> ([],) <$> project e
