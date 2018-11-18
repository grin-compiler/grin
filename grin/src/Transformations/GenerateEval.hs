{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Transformations.GenerateEval where

import Text.Printf
import Grin.Grin

{-
  done - collect all functions + arity
  done - generate eval
  done - generate apply
-}
generateEval :: Program -> Program
generateEval = \case
  Program defs -> Program $ eval defs : apply defs : defs where
  _ -> error "program expected"

eval :: [Def] -> Def
eval defs = Def "eval" ["p"] $
  EBind (SFetch "p") (Var "v") $
  ECase (Var "v") $ defaultAlt : (map funAlt . filter f $ defs) where
    defaultAlt                = Alt DefaultPat (SReturn $ Var "v")
    funAlt (Def name args _) = Alt (NodePat (Tag F name) argNames) $
                                      EBind (SApp name $ map Var argNames) (Var whnf) $
                                      EBind (SUpdate "p" $ Var whnf) Unit $
                                      SReturn $ Var whnf
                                    where
                                      whnf      = name <> ".whnf"
                                      argNames  = map (\i -> name <> "." <> showTS i) [1..length args]
    f (Def name _ _) = name `notElem` ["int_print", "grinMain"] -- TODO: proper filtering

apply :: [Def] -> Def
apply defs = Def "apply" ["f", "x"] $
  ECase (Var "f") $ concatMap funAlts $ filter f defs where
    f (Def name args _) = name `notElem` ["int_print", "grinMain"] && length args > 0 -- TODO: proper filtering
    funAlts def@(Def _ args _) = map (funAlt def) [0..length args-1]
    funAlt (Def name args _) i
      | i == n-1  = Alt (NodePat (Tag (P missingCount) name) argNames) $
                          SApp name $ map Var argNames ++ [Var "x"]
      | otherwise = Alt (NodePat (Tag (P missingCount) name) argNames) $
                          SReturn $ ConstTagNode (Tag (P $ pred missingCount) name) $ map Var argNames ++ [Var "x"]
      where
        argNames      = map (\i -> name <> showTS missingCount <> "." <> showTS i) [1..i]
        n             = length args
        missingCount  = length args - i
