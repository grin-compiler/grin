{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Transformations.GenerateEval where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

import Grin.Grin
import Transformations.Names

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
  let defaultAlt = Alt DefaultPat . SReturn . Var $ valueName
      funAlt name args = do
        argNames <- replicateM (length args) $ deriveNewName "a"
        whnf <- deriveNewName "res"
        pure $
          Alt (NodePat (Tag F name) argNames) $
            EBind (SApp name $ map Var argNames) (Var whnf) $
            EBind (SUpdate ptrName $ Var whnf) Unit $
            SReturn $ Var whnf

  alts <- sequence [funAlt name args | Def name args _ <- defs, Set.notMember name exclude]
  pure $
    Def evalName [ptrName] $
      EBind (SFetch ptrName) (Var valueName) $
      ECase (Var valueName) (defaultAlt : alts)

genApply :: Set Name -> Name -> [Def] -> NameM Def
genApply exclude applyName defs = do
  partialName <- deriveNewName "p"
  argName <- deriveNewName "x"

  let funAlt name arity missing = do
        argNames <- replicateM (arity - missing) $ deriveNewName "a"
        pure $ Alt (NodePat (Tag (P missing) name) argNames) $ if missing < 1 then error "genApply: internal error" else
          if missing == 1
            then SApp name $ map Var $ argNames ++ [argName]
            else SReturn $ ConstTagNode (Tag (P $ pred missing) name) $ map Var $ argNames ++ [argName]

  alts <- sequence
    [ funAlt name arity missing
    | Def name args _ <- defs
    , Set.notMember name exclude
    , let arity = length args
    , missing <- [1..arity]
    ]
  pure $ Def applyName [partialName, argName] $ ECase (Var partialName) alts
