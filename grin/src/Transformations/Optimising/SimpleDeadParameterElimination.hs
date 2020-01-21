{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations.Optimising.SimpleDeadParameterElimination where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

import Control.Monad (msum)
import Data.Maybe
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin.Grin
import Transformations.Util

{-
Check if the parameter name is used in value position.
 * pure val1
 * store val, fetch name, update name val
 * otherfun val1 ... valn
 * recfun val1 ... valn -- ignore if the name is used in its original pos
 * case val of
-}

-- Checks if the given name are present in the value and and return
-- Just name if is there, otherwise Nothing.
nameInVal :: Val -> Name -> Maybe Name
nameInVal val name = case val of
  Var name0 | name == name0 -> Just name
            | otherwise     -> Nothing
  ConstTagNode  tag  vals   -> msum $ map (`nameInVal` name) vals
  VarTagNode    _    vals   -> msum $ map (`nameInVal` name) vals
  ValTag        tag         -> Nothing
  Unit                      -> Nothing
  Lit           lit         -> Nothing
  Undefined     typ         -> Nothing

collectUsedArguments :: Name -> [(Int, Name)] -> Exp -> Set Name
collectUsedArguments fun args = cata collect where

  -- Collect all the arguments that are referred in the given val
  argsInVal :: Val -> Set Name
  argsInVal val = Set.fromList $ mapMaybe (nameInVal val . snd) args

  -- Collect the name if is an argument
  isArg :: Name -> Set Name
  isArg n = if n `elem` (snd <$> args) then Set.singleton n else Set.empty

  -- Collect the name when the recursive argument is out of its calling index.
  recursiveArg :: (Int, Val) -> Set Name
  recursiveArg (i, Var n)
    = if (i,n) `elem` args
        then Set.empty -- recursively used
        else Set.singleton n
  recursiveArg (_, val)
    = argsInVal val

  -- Collect all the args that are not recursively (in their original place) used
  collect :: ExpF (Set Name) -> Set Name
  collect = \case
    SReturnF val      -> argsInVal val
    SStoreF val       -> argsInVal val
    SFetchF name      -> isArg name
    SUpdateF name val -> isArg name <> argsInVal val
    SAppF name vals
      | name == fun -> mconcat $ map recursiveArg $ zip [0..] vals
      | otherwise   -> mconcat $ map argsInVal vals
    ECaseF val names -> argsInVal val <> mconcat names
    exp -> Data.Foldable.fold exp

type DeadArgMap
  = Map
    Name      -- Name  of the function
    (Set Int) -- Index of dead argument

simpleDeadParameterElimination :: Program -> Program
simpleDeadParameterElimination prog@(Program exts defs) = ana builder prog where
  deadArgMap :: DeadArgMap
  deadArgMap = mconcat $ mapMaybe deadArgsInDef defs

  deadArgsInDef :: Def -> Maybe DeadArgMap
  deadArgsInDef def@(Def name args body)
    | usedNames       <- collectUsedArguments name (zip [0..] args) body
    , deadArgIndices  <- Set.fromList . map fst . filter (flip Set.notMember usedNames . snd) $ zip [0..] args
    = if null deadArgIndices
        then Nothing
        else Just $ Map.singleton name deadArgIndices

  removeDead :: Set Int -> [a] -> [a]
  removeDead dead args = [arg | (idx, arg) <- zip [0..] args, Set.notMember idx dead]

  builder :: Exp -> ExpF Exp
  builder e = case mapValsExp pruneVal e of
    Def name args body
      | Just dead <- Map.lookup name deadArgMap
      -> DefF name (removeDead dead args) body

    SApp name args
      | Just dead <- Map.lookup name deadArgMap
      -> SAppF name (removeDead dead args)

    EBind leftExp lpat@ConstTagNode{} rightExp
      -> EBindF leftExp (pruneVal lpat) rightExp

    Alt cpat@NodePat{} body
      -> AltF (pruneCPat cpat) body

    exp -> project exp

  pruneVal :: Val -> Val
  pruneVal = \case
    ConstTagNode tag@(Tag kind name) vals
      | isPFtag kind
      , Just dead <- Map.lookup name deadArgMap
      -> ConstTagNode tag (removeDead dead vals)
    val -> val

  pruneCPat :: CPat -> CPat
  pruneCPat = \case
    NodePat tag@(Tag kind name) vars
      | isPFtag kind
      , Just dead <- Map.lookup name deadArgMap
      -> NodePat tag (removeDead dead vars)
    cpat -> cpat

  isPFtag :: TagType -> Bool
  isPFtag = \case
    F{} -> True
    P{} -> True
    _   -> False
