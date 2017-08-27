{-# LANGUAGE LambdaCase #-}
module Transformations where

import Control.Monad
import Control.Monad.Gen
import Data.Functor.Foldable as Foldable

import Grin

testCata :: Exp -> Int
testCata = cata folder where
  folder = \case
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
