{-# LANGUAGE LambdaCase #-}
module Transformations.GenerateEval where

import Data.Set (Set, singleton, toList)
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Foldable

import Data.Functor.Foldable as Foldable

import Grin


generateEval :: Program -> Program
generateEval program@(Program defs) = Program $ defs ++ maybe [] pure (evalDef (collectTagInfo program))
generateEval _ = error "generateEval - Program required"

evalDef :: Set Tag -> Maybe Def
evalDef tagInfo | not (Set.null tagInfo) = Just $ Def "generated_eval" ["p"] $
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
    tagAlt tag@(Tag C name) = Just $ Alt (NodePat tag (newNames 0 {-TODO-})) $ SReturn (Var "v")
    tagAlt tag@(Tag F name) = Just $ Alt (NodePat tag names) $
                                      EBind (SApp name $ map Var names) (Var "w") $
                                      EBind (SUpdate "p" $ Var "w")      Unit $
                                      SReturn $ Var "w"
                                    where names = newNames 0 {-TODO-}

    tagAlt (Tag P _) = Nothing

    newNames n = ['_' : show i | i <- [1..n]]
evalDef _ = Nothing

collectTagInfo :: Exp -> Set Tag
collectTagInfo = cata folder where
  folder = \case
    -- Exp
    ECaseF val alts -> mconcat $ add val : alts
    -- Simple Exp
    SReturnF  val   -> add val
    SStoreF   val   -> add val
    SUpdateF  _ val -> add val
    e -> Data.Foldable.fold e

  add = \case
    ConstTagNode (Tag tagtype name) args -> singleton (Tag tagtype name)
    ValTag tag          -> singleton tag
    _                   -> mempty
