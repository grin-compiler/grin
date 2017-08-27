{-# LANGUAGE LambdaCase #-}
module Transformations where

import Data.Monoid hiding (Alt)
import Data.Maybe
import Data.Set (Set, singleton, toList)
import Control.Monad
import Control.Monad.Gen
import Control.Monad.Writer hiding (Alt)
import Data.Functor.Foldable as Foldable

import Grin

countStores :: Exp -> Int
countStores = cata folder where
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
    SFetchF   _     -> mempty
    SBlockF   a     -> a
    -- Alt
    AltF _ a        -> a

  add = \case
    ConstTagNode (Tag tagtype name _) args -> singleton (Tag tagtype name (length args))
    ValTag tag          -> singleton tag
    _                   -> mempty

generateEval :: Program -> Program
generateEval program@(Program defs) = Program $ defs ++ [evalDef (collectTagInfoPure program)]
generateEval _ = error "generateEval - Program required"

evalDef :: Set Tag -> Def
evalDef tagInfo = Def "generated_eval" ["p"] $
  EBind (SFetch "p") (Var "v") $
  ECase (Var "v") $ mapMaybe tagAlt $ toList tagInfo where
    {- e.g.
        case v of
          (CInt x')     -> return v
          (Fupto a b)   -> w <- upto a b
                           update p w
                           return w
    -}
    -- TODO: create GRIN AST builder EDSL
    tagAlt tag@(Tag C name arity) = Just $ Alt (NodePat tag (newNames arity)) $ SReturn (Var "v")
    tagAlt tag@(Tag F name arity) = Just $ Alt (NodePat tag names) $ 
                                      EBind (SApp name $ map Var names) (Var "w") $
                                      EBind (SUpdate "p" $ Var "w")      Unit $
                                      SReturn $ Var "w"
                                    where names = newNames arity

    tagAlt (Tag P _ _) = Nothing

    newNames n = ['_' : show i | i <- [1..n]]
