{-# LANGUAGE LambdaCase #-}
module Transformations where

import Data.Set (Set, singleton)
import Control.Monad
import Control.Monad.Gen
import Control.Monad.Writer
import Data.Functor.Foldable as Foldable

import Grin

testCata :: Exp -> Int
testCata = cata folder where
  folder = \case
    ProgramF a      -> sum a
    DefF _ _ a      -> a
    -- Exp
    EBindF    a _ b -> a + b
    ECaseF    _ a   -> sum a
    -- Simple Expr
    SAppF     {}    -> 0
    SReturnF  {}    -> 0
    SStoreF   {}    -> 1
    SFetchF   {}    -> 0
    SUpdateF  {}    -> 0
    SBlockF   a     -> a
    -- Alt
    AltF _ a        -> a

type GenM = Gen Integer

vectorisation :: Exp -> Exp
vectorisation = runGen . cata folder where
  folder :: ExpF (GenM Exp) -> GenM Exp
  folder = \case
    EBindF simpleexp (Var name) exp -> do
      let newName = ('_' :) . show <$> gen
      tag <- newName
      args <- map Var <$> replicateM 3 newName
      EBind <$> simpleexp <*> pure (VarTagNode tag args) <*> exp
    e -> embed <$> sequence e

{-
  TODO:
    write a monoid version instead of writer monad
    write ana version of if possible at all
-}
collectTagInfo :: Exp -> Set Tag
collectTagInfo = execWriter . cata folder where
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
    ConstTagNode (Tag tagtype name _) args -> tell $ singleton (Tag tagtype name (length args))
    ValTag tag            -> tell $ singleton tag
    _ -> pure ()
