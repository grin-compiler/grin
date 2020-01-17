{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Transformations.ExtendedSyntax.GenerateEval where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

import Grin.ExtendedSyntax.Grin
import Transformations.ExtendedSyntax.Names

generateEval :: Program -> (Program, ExpChanges)
generateEval prog@(Program exts defs) =
  let exclude = Set.fromList ["int_print", "grinMain"]
      ((evalFun, applyFun), changed) = evalNameM prog $ do
        (,) <$> genEval exclude "eval" defs <*> genApply exclude "apply" defs
  in (Program exts $ evalFun : applyFun : defs, changed)
generateEval _ = error "program expected"

genEval :: Set Name -> Name -> [Def] -> NameM Def
genEval exclude evalName defs = do
  ptrName <- deriveNewName "p"
  valueName <- deriveNewName "v"
  defaultAltName <- deriveNewName "alt"
  let defaultAlt = Alt DefaultPat defaultAltName . SReturn . Var $ valueName
      funAlt name args = do
        argNames <- replicateM (length args) $ deriveNewName "a"
        whnf <- deriveNewName "res"
        wildcard <- deriveWildCard
        altName <- deriveNewName "alt"
        pure $
          Alt (NodePat (Tag F name) argNames) altName $
            EBind (SApp name $ argNames) (VarPat whnf) $
            EBind (SUpdate ptrName whnf) (VarPat wildcard) $
            SReturn $ Var whnf

  alts <- sequence [funAlt name args | Def name args _ <- defs, Set.notMember name exclude]
  pure $
    Def evalName [ptrName] $
      EBind (SFetch ptrName) (VarPat valueName) $
      ECase valueName (defaultAlt : alts)

genApply :: Set Name -> Name -> [Def] -> NameM Def
genApply exclude applyName defs = do
  partialName <- deriveNewName "p"
  argName <- deriveNewName "x"

  let funAlt name arity missing = do
        altName <- deriveNewName "alt"
        argNames <- replicateM (arity - missing) $ deriveNewName "a"
        pure $ Alt (NodePat (Tag (P missing) name) argNames) altName $ if missing < 1 then error "genApply: internal error" else
          if missing == 1
            then SApp name $ argNames ++ [argName]
            else SReturn $ ConstTagNode (Tag (P $ pred missing) name) $ argNames ++ [argName]

  alts <- sequence
    [ funAlt name arity missing
    | Def name args _ <- defs
    , Set.notMember name exclude
    , let arity = length args
    , missing <- [1..arity]
    ]
  pure $ Def applyName [partialName, argName] $ ECase partialName alts
