{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
module Transformations where

import Text.Printf
import Text.Pretty.Simple (pShow)
import Text.PrettyPrint.ANSI.Leijen (pretty)
import Data.Maybe
import Data.List (intercalate, foldl')
import Data.Set (Set, singleton, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid hiding (Alt)
import Control.Arrow ((***), second)
import Control.Monad
import Control.Monad.Gen
import Control.Monad.Writer hiding (Alt)
import Data.Functor.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Text.Lazy as Text

import qualified Data.Foldable
import Control.Comonad.Cofree
import Control.Monad.State

import Grin
import VarGen
import Pretty
import AbstractRunGrin hiding (Step)


type GenM = Gen Integer

type VectorisationAccumulator = (Map.Map Name Val, Exp)

getVarNodeArity :: HPTResult -> Name -> Maybe Int
getVarNodeArity Computer{..} name = case Map.lookup name envMap of
  Nothing -> error $ printf "getVarNodeArity - unknown variable '%s'" name
  Just varSet -> case [length args | N (RTNode tag args) <- Set.toList varSet] of
    [] -> Nothing
    maxArityWithoutTag -> Just $ maximum maxArityWithoutTag

vectorisation :: HPTResult -> Exp -> Exp
vectorisation hptResult expression = apo folder (Map.empty, expression)
  where
    folder :: VectorisationAccumulator -> ExpF (Either Exp VectorisationAccumulator)
    folder (nameStore, expression) =
      case expression of
        EBind simpleexp var@(Var name) exp -> case getVarNodeArity hptResult name of
          Nothing           -> EBindF (Right (nameStore, simpleexp)) var (Right (nameStore, exp))
          Just maximumArity -> EBindF (Right (nameStore, simpleexp)) nodeContents (Right (newNameStore, exp))
           where
            nodeContents = VarTagNode (name <> show 0) (map (\i -> Var (name <> show i)) [1 .. maximumArity])
            newNameStore = Map.insert name nodeContents nameStore
        ECase (Var name) alts | Just nodeContents <- Map.lookup name nameStore -> ECaseF nodeContents (map (\subExpression -> Right (nameStore, subExpression)) alts)
        SApp name vals -> SAppF name (map replaceVar vals)
        SReturn (Var name) | Just nodeContents <- Map.lookup name nameStore -> SReturnF nodeContents
        SStore (Var name) | Just nodeContents <- Map.lookup name nameStore -> SStoreF nodeContents
        SUpdate updateName (Var name) | Just nodeContents <- Map.lookup name nameStore -> SUpdateF updateName nodeContents
        e -> forwardRecursion
      where
        replaceVar (Var name) | Just val <- Map.lookup name nameStore = val
        replaceVar var = var
        forwardRecursion = fmap (\subExpression -> Right (nameStore, subExpression)) (project expression)

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
    ConstTagNode (Tag tagtype name _) args -> singleton (Tag tagtype name (length args))
    ValTag tag          -> singleton tag
    _                   -> mempty

generateEval :: Program -> Program
generateEval program@(Program defs) = Program $ defs ++ [evalDef (collectTagInfo program)]
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


splitFetch :: Exp -> Exp
splitFetch = cata folder where
  folder = \case
    EBindF (SFetch name) (ConstTagNode _ args) exp -> EBind (SBlock $ newBinds name $ zip [1..] args) Unit exp
    EBindF (SFetch name) (VarTagNode tagvar args) exp -> EBind (SBlock $ newBinds name $ zip [0..] $ Var tagvar : args) Unit exp
    e -> embed e

  newBinds name [] = SReturn Unit
  newBinds name ((i, var) : vars) = EBind (SFetchI name (Just i)) var $ newBinds name vars

-- Assign Store IDs
assignStoreIDs :: Exp -> Cofree ExpF Int
assignStoreIDs = runGen . cata folder where
  folder = \case
    SStoreF v -> (:< SStoreF v) <$> gen
    e -> (0 :<) <$> sequence e

-- Bind normalisation (EXTREME UGLY first version)
bindNormalisation :: Exp -> Exp
bindNormalisation = ($ id) . snd . cata folder where
  folder :: ExpF (Bool, (Exp -> Exp) -> Exp) -> (Bool, (Exp -> Exp) -> Exp)
  folder = \case

    EBindF (hasSBlock, sexpf) pat (_, expf) -> case hasSBlock of
      True  -> (False, \f -> sexpf $ \sexp -> EBind sexp pat (expf f))
      False -> (False, \f -> EBind (sexpf id) pat (expf f))

    SBlockF (_, f) -> (True, f)
    -- SimpleExp: return, app, case, store, fetch, update
    SAppF name vals -> (False, \f -> f (SApp name vals))
    SReturnF val -> (False, \f -> f (SReturn val))
    SStoreF val -> (False, \f -> f (SStore val))
    SFetchIF name index -> (False, \f -> f (SFetchI name index))
    SUpdateF name val -> (False, \f -> f (SUpdate name val))
    AltF cpat (_, expf) -> (False, \f -> f (Alt cpat (expf id)))
    ECaseF val altsf -> (False, \f -> f (ECase val (map (($ id) . snd) altsf)))
    DefF name args (_, expf) -> (False, \f -> f (Def name args (expf id)))
    ProgramF defs -> (False, \f -> f (Program (map (($ id) . snd) defs)))

-- Case Simplification
type SubstMap = Map SimpleVal SimpleVal

mapVals :: (Val -> Val) -> Exp -> Exp
mapVals f = \case
  ECase val alts -> ECase (f val) alts
  SApp name vals -> SApp name (map f vals)
  SReturn val -> SReturn $ f val
  SStore val -> SStore $ f val
  SUpdate name val -> SUpdate name $ f val
  exp -> exp

substExpVals :: SubstMap -> Exp -> Exp
substExpVals env = mapVals (subst env)

subst env x = Map.findWithDefault x x env

caseSimplification :: Exp -> Exp
caseSimplification e = ana builder (mempty, e) where
  builder :: (SubstMap, Exp) -> ExpF (SubstMap, Exp)
  builder (env, exp) =
    case exp of
      ECase (VarTagNode tagVar vals) alts -> ECaseF (subst env $ Var tagVar) (map (substAlt env vals) alts)
      e -> (env,) <$> project (substExpVals env e)

  substAlt env vals = \case
      Alt (NodePat tag vars) e -> (altEnv, Alt (TagPat tag) e)
                             where altEnv = foldl' (\m (name,val) -> Map.insert (Var name) (subst env val) m) env (zip vars vals)
      alt -> (env, alt)

data PatternF f a
  = PVar String
  | PNext String (f a)
  | PVal (f a)
  deriving (Eq, Show)

type Pattern = Fix (PatternF ExpF)

example1 :: Pattern
example1 = Fix (PVal (EBindF (Fix (PVar "m0")) (Var "v") (Fix (PVar "m1"))))

example2 :: Pattern
example2 = Fix (PNext "m1" (ECaseF Unit
  [ Fix (PVal (AltF (LitPat (LInt 3)) (Fix (PVar "m1"))))
  , Fix (PVal (AltF (LitPat (LInt 3)) (Fix (PVar "m2"))))
  ]))

{-
m0; \v ->
m1;

EBindF (Fix (PVar "m0")) (Var v) (Fix (PVar m1))

m0;
case (t a1 a2) of
  CNil -> m1
  CCons x xs -> m2

(PBind "m1" (ECaseF val
  [ Fix (PVal (Alt cpat) (Fix (PVar "m1")))
  , Fix (PVal (Alt cpat) (Fix (PVar "m2")))
  ])
-}


