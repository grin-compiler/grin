{-# LANGUAGE LambdaCase #-}
module Transformations.GenerateEval where

import Text.Printf
import Grin

{-
  done - collect all functions + arity
  done - generate eval
  generate apply
-}
generateEval :: Program -> Program
generateEval = \case
  Program defs -> Program $ eval defs : {-apply defs : -}defs where
  _ -> error "program expected"

eval :: [Def] -> Def
eval defs = Def "eval" ["p"] $
  EBind (SFetch "p") (Var "v") $
  ECase (Var "v") $ defaultAlt : map funAlt defs where
    defaultAlt                = Alt DefaultPat (SReturn $ Var "v")
    funAlt (Def name args _)  = Alt (NodePat (Tag F name) argNames) $
                                      EBind (SApp name $ map Var argNames) (Var whnf) $
                                      EBind (SUpdate loc $ Var whnf) Unit $
                                      SReturn $ Var whnf
                                    where
                                      whnf      = printf "%s.whnf" name
                                      loc       = printf "%s.loc" name
                                      argNames  = map (printf "%s.%d" name) [1..length args]

apply :: [Def] -> Def
apply defs = undefined
