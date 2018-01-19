{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
module Transformations.Simplifying.RegisterIntroduction where

import Control.Arrow ((***), second)
import Data.Function
import Data.Map (Map)
import Data.Maybe
import Grin
import Transformations.Rename
import Data.List (intercalate, foldl')
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Data.Functor.Infix
import Control.Monad.State
import VarGen
import Test.Hspec



type FreshM a = State Int a

registerIntroductionM :: Int -> Exp -> Exp
registerIntroductionM nth exp = flip evalState 0 $ cata folder exp where
  folder :: ExpF (FreshM Exp) -> FreshM Exp
  folder = \case
    SStoreF (VarTagNode name vals)         -> SBlock <$> varTagNode SStore name vals
    SStoreF (ConstTagNode tag vals)        -> SBlock <$> constTagNode SStore tag vals
    SStoreF (Lit lit)                      -> SBlock <$> literal SStore lit
    SReturnF (VarTagNode name vals)        -> SBlock <$> varTagNode SReturn name vals
    SReturnF ctag@(ConstTagNode tag [])    -> pure $ SReturn ctag
    SReturnF (ConstTagNode tag vals)       -> SBlock <$> constTagNode SReturn tag vals
    SUpdateF uname (VarTagNode tname vals) -> SBlock <$> varTagNode (SUpdate uname) tname vals
    SUpdateF uname (ConstTagNode tag vals) -> SBlock <$> constTagNode (SUpdate uname) tag vals
    SUpdateF uname (Lit lit)               -> SBlock <$> literal SStore lit
    SAppF name vals | any isLit vals       -> SBlock <$> appExp name vals
    e                                      -> embed  <$> sequence e

    where
      freshName :: FreshM String
      freshName = do
        n <- gets show
        modify' (+1)
        pure $ intercalate "." ["v", show nth, n]

      changeSimpleVals :: [SimpleVal] -> FreshM ([SimpleVal], [(Name, Val)])
      changeSimpleVals svals = (second catMaybes . unzip) <$> mapM changeVal svals
        where
          changeVal (Lit lit)  = do { v <- freshName; pure (Var v, Just (v, Lit lit)) }
          changeVal (Var v)    = pure (Var v, Nothing)
          changeVal (ValTag g) = do { v <- freshName; pure (Var v, Just (v, ValTag g)) }
          changeVal bad        = error $ unwords ["registerIntroduction changeSimpleVals: invalid simple literal:", show bad]

      literal context lit = do
        v <- Var <$> freshName
        pure $ EBind (SReturn (Lit lit)) v (context v)

      varTagNode   context name = introduction (const $ context . VarTagNode name)
      appExp               name = introduction (const $ SApp name)
      constTagNode context tag vals =
        introduction
          (\(Just t) vs -> context $ VarTagNode t (tail vs))
          ((ValTag tag):vals)

      introduction context vals = do
        (vals', newVars) <- changeSimpleVals vals
        pure $ foldr
            (\(name, lit) -> EBind (SReturn lit) (Var name))
            (context (fst <$> listToMaybe newVars) vals') -- Tag is always first and stand for constTagNode only
            newVars

registerIntroduction :: Int -> Exp -> Exp
registerIntroduction nth e = apo builder (branchVar nth newVarGen, e) where
  builder :: (VPM Mod10, Exp) -> ExpF (Either Exp (VPM Mod10, Exp))
  builder (path, exp) =
    case exp of
      SStore (VarTagNode name vals)         -> varTagNode   SStore          name vals
      SStore (ConstTagNode tag vals)        -> constTagNode SStore          tag vals
      SStore (Lit lit)                      -> literal      SStore          lit
      SReturn (VarTagNode name vals)        -> varTagNode   SReturn         name vals
      SReturn (ConstTagNode tag vals)       -> constTagNode SReturn         tag vals
      SUpdate uname (VarTagNode tname vals) -> varTagNode   (SUpdate uname) tname vals
      SUpdate uname (ConstTagNode tag vals) -> constTagNode (SUpdate uname) tag vals
      SUpdate uname (Lit lit)               -> literal      (SUpdate uname) lit
      SApp name vals                        -> appExp (if any isLit vals then SBlock else id) name vals

      Def name names exp                    -> DefF name names (Right (branch name path, exp))
      EBind sexp lpat exp                   -> EBindF (Right (branchVar 1 path, sexp)) lpat (Right (branchVar 2 path, exp))
      ECase val alts                        -> ECaseF val $ zipWith (\i a -> Right (branchVar i path, a)) [0..] alts

      e -> fmap (Right . withPath' exp) $ project e

    where
      withPath' exp = maybe withPath withNewPath (newVarName path exp)
      withPath e = (path, e)
      withNewPath p e = (extendVarGen p path, e)
      evars = vars path

      changeSimpleVals :: [Name] -> [SimpleVal] -> ([SimpleVal], [(Name, Val)])
      changeSimpleVals newVars svals = second catMaybes . unzip $ zipWith changeVal svals newVars
        where
          changeVal (Lit lit)  v = (Var v, Just (v, Lit lit))
          changeVal (Var v)    _ = (Var v, Nothing)
          changeVal (ValTag g) v = (Var v, Just (v, ValTag g)) -- constTagNode only
          changeVal bad        _ = error $ unwords ["registerIntroduction changeSimpleVals: invalid simple literal:", show bad]

      literal context lit =
        fmap Left . project $ EBind (SReturn (Lit lit)) (Var (evars !! 0)) (context (Var $ evars !! 0))

      introduction block context vals =
        let (vals', newVars) = changeSimpleVals evars vals
        in fmap Left . project . block $ foldr
            (\(name, lit) -> EBind (SReturn lit) (Var name))
            (context (fst <$> listToMaybe newVars) vals') -- Tag is always first and stand for constTagNode only
            newVars

      appExp       block name = introduction block (const $ SApp name)
      varTagNode   context name = introduction id (const $ context . VarTagNode name)
      constTagNode context tag vals =
        introduction SBlock
          (\(Just t) vs -> context $ VarTagNode t (tail vs))
          ((ValTag tag):vals)

nth :: Int -> Int -> [a] -> [a]
nth s n = go 1 . drop s where
  go 1 (x:xs) = x:go n   xs
  go n (_:xs) = go (n-1) xs

nthSpec :: Spec
nthSpec = describe "nth" $ do
  it "works for 0 2" $ do
    (take 5 $ nth 0 2 [1..]) `shouldBe` [1,3,5,7,9]
  it "works for 1 2" $ do
    (take 5 $ nth 1 2 [1..]) `shouldBe` [2,4,6,8,10]


type Ids = [Int]

newIds :: Exp -> Exp
newIds e = ana (dCoAlg (show . take 5 . fst) (newIdsCA project)) ([1..], e)

newIdsCA :: (a -> ExpF a) -> ((Ids,a) -> ExpF (Ids,a))
newIdsCA coAlg (ids,x) = case coAlg x of
  ProgramF  as           ->
    let n = length as
    in ProgramF $ zipWith (\x y -> (nth x n ids, y)) [0..] as
  DefF      name names a -> DefF name names (ids, a)
  -- Exp
  EBindF    a0 lpat a1 -> EBindF ((take 1 ids), a0) lpat ((tail ids), a1)
  ECaseF    val as ->
    let n = length as
    in ECaseF val $ zipWith (\x y -> (nth x n ids, y)) [0..] as
  -- Simple Expr
  SAppF     name simpleVals -> SAppF name simpleVals
  SReturnF  val -> SReturnF val
  SStoreF   val -> SStoreF val
  SFetchIF  name pos -> SFetchIF name pos
  SUpdateF  name val -> SUpdateF name val
  SBlockF   a -> SBlockF (ids, a)
  -- Alt
  AltF cpat a -> AltF cpat (ids, a)


registerIntroductionI :: Int -> Exp -> Exp
registerIntroductionI _ e = apo builder ([1..], e) where
  builder :: ([Int], Exp) -> ExpF (Either Exp ([Int], Exp))
  builder (path, exp) =
    case exp of
      SStore (VarTagNode name vals)         -> varTagNode   SStore          name vals
      SStore (ConstTagNode tag vals)        -> constTagNode SStore          tag vals
      SStore (Lit lit)                      -> literal      SStore          lit
      SReturn (VarTagNode name vals)        -> varTagNode   SReturn         name vals
      SReturn (ConstTagNode tag vals)       -> constTagNode SReturn         tag vals
      SUpdate uname (VarTagNode tname vals) -> varTagNode   (SUpdate uname) tname vals
      SUpdate uname (ConstTagNode tag vals) -> constTagNode (SUpdate uname) tag vals
      SUpdate uname (Lit lit)               -> literal      (SUpdate uname) lit
      SApp name vals                        -> appExp (if any isLit vals then SBlock else id) name vals

      Program defs -> let n = length defs
                      in ProgramF $ zipWith (\i d -> Right (nth i n path', d)) [1..] defs
      EBind sexp lpat exp                   -> EBindF (Right (nth 0 2 path', sexp)) lpat (Right (nth 1 2 path', exp))
      ECase val alts                        -> let n = length alts
                                               in ECaseF val $ zipWith (\i a -> Right (nth i n path', a)) [0..] alts

      e -> fmap (\e' -> Right (path', e')) $ project e -- (Right . (,) (tail path)) $ project e

    where
      path' = tail path
      evars = map (\i -> concat ["v.", show (head path), ".", show i]) [1..]

      changeSimpleVals :: [Name] -> [SimpleVal] -> ([SimpleVal], [(Name, Val)])
      changeSimpleVals newVars svals = second catMaybes . unzip $ zipWith changeVal svals newVars
        where
          changeVal (Lit lit)  v = (Var v, Just (v, Lit lit))
          changeVal (Var v)    _ = (Var v, Nothing)
          changeVal (ValTag g) v = (Var v, Just (v, ValTag g)) -- constTagNode only
          changeVal bad        _ = error $ unwords ["registerIntroduction changeSimpleVals: invalid simple literal:", show bad]

      literal context lit =
        fmap Left . project $ EBind (SReturn (Lit lit)) (Var (evars !! 0)) (context (Var $ evars !! 0))

      introduction block context vals =
        let (vals', newVars) = changeSimpleVals evars vals
        in fmap Left . project . block $ foldr
            (\(name, lit) -> EBind (SReturn lit) (Var name))
            (context (fst <$> listToMaybe newVars) vals') -- Tag is always first and stand for constTagNode only
            newVars

      appExp       block name = introduction block (const $ SApp name)
      varTagNode   context name = introduction id (const $ context . VarTagNode name)
      constTagNode context tag vals =
        introduction SBlock
          (\(Just t) vs -> context $ VarTagNode t (tail vs))
          ((ValTag tag):vals)


-- Work In Progress
type VariablePath = [String]
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

tests :: Spec
tests = do
  nthSpec
