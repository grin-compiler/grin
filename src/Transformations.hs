{-# LANGUAGE LambdaCase, TupleSections #-}
module Transformations where

import Data.Maybe
import Data.List (intercalate)
import Data.Set (Set, singleton, toList)
import qualified Data.Map as Map
import Data.Monoid hiding (Alt)
import Control.Arrow (second)
import Control.Monad
import Control.Monad.Gen
import Control.Monad.Writer hiding (Alt)
import Data.Functor.Foldable as Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Foldable
import Control.Comonad.Cofree

import Grin

type GenM = Gen Integer

type VectorisationAccumulator = (Map.Map Name Val, Exp)

vectorisation :: Exp -> Exp
vectorisation expression = apo folder (Map.empty, expression)
  where
    maximumArity = maximum (0 : map tagArity (Set.toList (collectTagInfo expression)))

    folder :: VectorisationAccumulator -> ExpF (Either Exp VectorisationAccumulator)
    folder (nameStore, expression) =
      case expression of
        EBind simpleexp (Var name) exp ->
          EBindF (Right (nameStore, simpleexp)) nodeContents (Right (newNameStore, exp))
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
  newBinds name ((i, var) : vars) = EBind (fetchItem name i) var $ newBinds name vars

  fetchItem name i = SFetchI name (Just i)


newVarName :: Exp -> Maybe [String]
newVarName = \case
  Program     defs               -> Nothing
  Def         name names exp     -> Just [name]
  EBind       simpleExp lpat exp -> Just ["b1", "b2"]
  ECase       val alts           -> Just . map (\(i,_) -> "c" ++ show i) $ [0..] `zip` alts
  SApp        name simpleVals    -> Just ["a"]
  SReturn     val                -> Just ["r"]
  SStore      val                -> Just ["s"]
  SFetchI     name index         -> Nothing
  SUpdate     name val           -> Just [name]
  SBlock      exp                -> Nothing
  Alt         cpat exp           -> Nothing


type VariablePath = [String]

registerIntroduction :: Exp -> Exp
registerIntroduction e = ana builder ([], e) where
  builder :: (VariablePath, Exp) -> ExpF (VariablePath, Exp)
  builder (path, exp) =
    case exp of
      EBind (SStore (VarTagNode name vals))        lpat exp -> varTagNode   (\val -> EBind (SStore val) lpat exp)       name vals
      EBind (SStore (ConstTagNode tag vals))       lpat exp -> constTagNode (\val -> EBind (SStore val) lpat exp)       tag vals
      EBind (SStore (Lit lit))                     lpat exp -> literal      (\val -> EBind (SStore val) lpat exp)       lit
      EBind (SUpdate name (VarTagNode tname vals)) lpat exp -> varTagNode   (\val -> EBind (SUpdate name val) lpat exp) tname vals
      EBind (SUpdate name (ConstTagNode tag vals)) lpat exp -> constTagNode (\val -> EBind (SUpdate name val) lpat exp) tag vals
      EBind (SUpdate name (Lit lit))               lpat exp -> literal      (\val -> EBind (SUpdate name val) lpat exp) lit
      EBind (SReturn (VarTagNode name vals))       lpat exp -> varTagNode   (\val -> EBind (SReturn val) lpat exp)      name vals
      EBind (SReturn (ConstTagNode tag vals))      lpat exp -> constTagNode (\val -> EBind (SReturn val) lpat exp)      tag vals

      EBind (SApp name vals)                       lpat exp -> appExp       (\val -> EBind val lpat exp)                name vals

      SStore (VarTagNode name vals)         -> varTagNode   SStore          name vals
      SStore (ConstTagNode tag vals)        -> constTagNode SStore          tag vals
      SStore (Lit lit)                      -> literal      SStore          lit
      SReturn (VarTagNode name vals)        -> varTagNode   SReturn         name vals
      SReturn (ConstTagNode tag vals)       -> constTagNode SReturn         tag vals
      SUpdate uname (VarTagNode tname vals) -> varTagNode   (SUpdate uname) tname vals
      SUpdate uname (ConstTagNode tag vals) -> constTagNode (SUpdate uname) tag vals
      SUpdate uname (Lit lit)               -> literal      (SUpdate uname) lit

      SApp name vals                        -> appExp id                    name vals

      e -> fmap (withPath' exp) $ project e

    where
      withPath' exp = maybe withPath (withNewPath . head) (newVarName exp)
      withPath e = (path, e)
      withNewPath p e = (p:path, e)
      vars = map (intercalate ".") $ zipWith (++) (map (pure . show) [1..]) (repeat path)

      changeSimpleVals :: [Name] -> [SimpleVal] -> ([SimpleVal], [(Name, Val)])
      changeSimpleVals newVars svals = second catMaybes . unzip $ zipWith changeVal svals newVars
        where
          changeVal (Lit lit)  v = (Var v, Just (v, Lit lit))
          changeVal (Var v)    _ = (Var v, Nothing)
          changeVal (ValTag g) v = (Var v, Just (v, ValTag g)) -- constTagNode only
          changeVal bad        _ = error $ unwords ["registerIntroduction changeSimpleVals: invalid simple literal:", show bad]

      literal context lit =
        fmap (withPath' exp) . project $ EBind (SReturn (Lit lit)) (Var (vars !! 0)) (context (Var $ vars !! 0))

      introduction context vals =
        let (vals', newVars) = changeSimpleVals vars vals
        in fmap (withPath' exp) . project $ foldr
            (\(name, lit) -> EBind (SReturn lit) (Var name))
            (context (fst <$> listToMaybe newVars) vals') -- Tag is always first and stand for constTagNode only
            newVars

      varTagNode   context name = introduction (const $ context . VarTagNode name)
      appExp       context name = introduction (const $ context . SApp name)
      constTagNode context tag vals =
        introduction
          (\(Just t) vs -> context $ VarTagNode t (tail vs))
          ((ValTag tag):vals)


-- Work In Progress
{-
  TODO:
    - mapping over Vals
    - convenient fresh name API
    - AST builder monad. e.g.:
        registerIntroduction :: Val -> AST Val
        buildSExp $ SStore <$> registerIntroduction val
  IDEA:
    - shape functor over Val
-}
registerIntroduction2 :: Exp -> Exp
registerIntroduction2 e = ana builder ([], e) where
  builder :: (VariablePath, Exp) -> ExpF (VariablePath, Exp)
  builder (path, exp) =
    case exp of
      SStore val        -> ([],) <$> project (build SStore val)

      SUpdate uname val -> ([],) <$> project (build (SUpdate uname) val)
      SReturn val       -> ([],) <$> project (build SReturn val) -- TODO: prevent generate non node values

      --SApp name vals    | isNodeVal val -> appExp id                    name vals

      e -> ([],) <$> project e

build :: (Val -> SimpleExp) -> Val -> SimpleExp
build expfun val = bindVals (expfun valVar) patvars where (valVar, patvars) = breakVal val

bindVals :: SimpleExp -> [(LPat, Val)] -> SimpleExp
bindVals exp [] = exp
bindVals exp vals = SBlock $ foldr (\(lpat, val) e -> EBind (SReturn val) lpat e) exp vals

breakVal :: Val -> (Val, [(LPat, Val)])
breakVal = \case -- return new valueless Val + name-val list

  VarTagNode tname vals -> (VarTagNode tname valVars, varvals) where
    splitted_vals = map splitSVal vals
    (valVars, _) = unzip splitted_vals
    varvals = [(var, val) | (var, Just val) <- splitted_vals]

  ConstTagNode tag vals -> (VarTagNode tname valVars, varvals) where
    splitted_vals = map splitSVal (ValTag tag : vals)
    (Var tname : valVars, _) = unzip splitted_vals
    varvals = [(var, val) | (var, Just val) <- splitted_vals]

  val -> case splitSVal val of
    (var, Nothing)  -> (var, [])
    (var, Just v)   -> (var, [(var, v)])

splitSVal :: Val -> (Val, Maybe Val)
splitSVal val = case val of
  Var{} -> (val, Nothing)
  _ -> (Var "newName", Just val)


-- Assign Store IDs
assignStoreIDs :: Exp -> Cofree ExpF Int
assignStoreIDs = runGen . cata folder where
  folder = \case
    SStoreF v -> (:< SStoreF v) <$> gen
    e -> (0 :<) <$> sequence e
