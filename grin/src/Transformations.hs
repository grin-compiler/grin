{-# LANGUAGE LambdaCase, TupleSections, TypeApplications, RecordWildCards, DeriveFunctor #-}
module Transformations where

import Text.Printf
import Text.Pretty.Simple (pShow)
import Data.Functor.Infix
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
import AbstractRunGrin hiding (Step)

import qualified Debug.Trace as Debug

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

data VarOccurance a
  = Defined a
  | Used    a
  deriving (Eq, Functor, Show, Ord)

coPoint :: VarOccurance a -> a
coPoint = \case
  Defined a -> a
  Used    a -> a

isDefined :: VarOccurance a -> Bool
isDefined = \case
  Defined _ -> True
  Used    _ -> False

isUsed :: VarOccurance a -> Bool
isUsed = \case
  Defined _ -> False
  Used    _ -> True

data Step
  = SName Name
  | Case Int
  | CAlt Int
  deriving (Eq, Ord, Show)

type Path = [Step]
type RHIData = Map Name [VarOccurance Path]

-- | Returns the map of the location where the name is used
-- ordered by the location.
firstUsed :: RHIData -> RHIData
firstUsed = Map.map (List.sort . filter isUsed)

nubRHIData :: RHIData -> RHIData
nubRHIData = Map.map List.nub

-- | Returns the variables that are not used where they are defined.
usedInDifferentBlock :: RHIData -> RHIData
usedInDifferentBlock = Map.filter nonLocalyUsed where
  nonLocalyUsed vs = case (List.filter isDefined vs) of
    []    -> False -- True -- error "Undefined variable."
    [Defined path] -> maybe True (const False) $ List.find (path==)
                      $ map coPoint
                      $ List.filter isUsed vs
    (_:_) -> error "Mupliple defined variable."

rightHoistFetch :: Exp -> Exp
rightHoistFetch e =
  Debug.traceShow ("RIGHT HOIST FETCH", "VARS", vars0, "NONLOCAL", nonLocal)
  $ apo builder (Map.empty, [], e)
  where
    vars0 = rightHoistFetchVars e
    vars = firstUsed vars0
    nonLocal = usedInDifferentBlock vars0

    builder :: (Map Name SimpleExp, [Step], Exp) -> ExpF (Either Exp (Map Name SimpleExp, [Step], Exp))
    builder (moves, path, e) = Debug.traceShow ("MOVES", moves) $ case e of

--      e@(Def _ _ _) -> Debug.trace (Text.unpack $ pShow e) $
--        (Right . (,) path) <$> project e

      EBind fetch@(SFetchI n p) pval@(Var name) rest ->
        let names0 = foldr (++) [] (NamesInExpF (project fetch) list)
            names1 = foldNames list pval
            names = names0 ++ names1
            shouldMove = not . Map.null $ Map.filterWithKey (\k _ -> k `elem` names1) nonLocal
--            moves' = if shouldMove then foldr Map.delete moves names else moves
            moves' = if shouldMove then (Map.insert name fetch moves) else moves
--            shouldInsert = intersection' names0 moves'
--            moves'' = Map.difference moves' shouldInsert
        in Debug.traceShow (
            ( "EBind fetch var"
            , "SHOULD MOVE", shouldMove
--            , shouldInsert
--            , Map.filterWithKey (\k _ -> k `elem` names) nonLocal
--            , names
            ))
          $
            EBindF (Left fetch) pval (Right (moves', path, rest))

{-
      EBind se pval rest | isSimpleExp rest ->
        Debug.traceShow (
          "EBind simple",
          let names0 = foldr (++) [] (NamesInExpF (project se) list)
              names1 = foldr (++) [] (NamesInExpF (project rest) list)
              names2 = foldNames list pval
              names = names0 ++ names1 ++ names2
              shouldMove = not . Map.null $ Map.filterWithKey (\k _ -> k `elem` names) nonLocal
          in (shouldMove, Map.filterWithKey (\k _ -> k `elem` names) nonLocal, names))
          $
        EBindF (Left se) pval (Left rest)
-}

      EBind se pval rest ->
        let names0 = foldr (++) [] (NamesInExpF (project se) list)
            names1 = foldNames list pval
            names = names0 ++ names1
--            shouldMove = not . Map.null $ Map.filterWithKey (\k _ -> k `elem` names) nonLocal
--            insertS = if shouldMove then (\n -> (n, n `Map.lookup` moves)) <$> names else []
--            moves' = if shouldMove then foldr Map.delete moves names else moves
            shouldInsert = intersection' names0 moves
            moves' = Map.difference moves shouldInsert
        in Debug.traceShow
            ( "EBind complex"
--            , shouldMove
            , "SHOULD INSERT",shouldInsert
--            , Map.filterWithKey (\k _ -> k `elem` names) nonLocal
--            , names
            )
          $
            EBindF (Left se) pval
              (if isSimpleExp rest
                then (Left rest)
                else (Right (moves', path, rest)))

      ECase val alts ->
        ECaseF val $ zipWith
          (\i (Alt pat e) -> (Right (moves, [CAlt i, Case (length alts)] ++ path, Alt pat e)))
          [1..] alts
      Def def args body -> DefF def args (Right (moves, SName def:path, body))

      e | isSimpleExp e ->
          let names = foldr (++) [] (NamesInExpF (project e) list)
              shouldMove = not . Map.null $ Map.filterWithKey (\k _ -> k `elem` names) nonLocal

          in Debug.traceShow (
              "Simple expr",
              (shouldMove, Map.filterWithKey (\k _ -> k `elem` names) nonLocal))
          $ (Right . (,,) moves path) <$> project e

      e -> (Right . (,,) moves path) <$> project e
      where
        list x = [x]
        namesInExp = foldr (:) [] (NamesInExpF (project e) list)
        containsKeys keys = not . Map.null . Map.filterWithKey (\k _ -> k `elem` keys)
        intersection' keys m = Map.intersection m (Map.fromList $ zip keys [1..])

--  Debug.trace (Text.unpack . pShow $ rightHoistFetchVars e) e

-- For local blocks...
rightHoistFetchVars :: Exp -> RHIData
rightHoistFetchVars = cata collect where
  collect :: ExpF RHIData -> RHIData
  collect = \case
    EBindF se pval rest ->
      Map.unionsWith (++)
        [ -- uncurry Map.singleton $ defName name
          Map.fromList . map defName $ collectNames pval
        , se
        , rest
        ]
    ECaseF val alts ->
      Map.unionsWith (++)
        [ (Map.unionsWith (++)
            $ zipWith (\i a -> (([Case (length alts), CAlt i]++) <$$$> a)) [1..] alts)
        , useNameValMap val
        ]
    DefF def args rest ->
      nubRHIData $ Map.unionsWith (++)
        [ uncurry Map.singleton $ defName def
        , (SName def:) <$$$$> Map.fromList $ defName <$> args
        , (SName def:) <$$$> rest
        ]

    -- Does not collect function names.
    SAppF name args ->
      Map.fromList $ map useName $ (concat $ collectNames <$> args)

    SReturnF val -> useNameValMap val
    SStoreF val  -> useNameValMap val
    SFetchIF name pos -> uncurry Map.singleton $ useName name
    SUpdateF name val ->
      Map.unionsWith (++)
        [ uncurry Map.singleton $ useName name
        , Map.fromList $ map useName $ collectNames val
        ]

    e -> Data.Foldable.fold e

  useNameValMap = Map.fromList . map useName . collectNames
  collectNames = foldNames list
  list x = [x]
  useName n = (n, [Used []])
  defName n = (n, [Defined []])


withoutKeys :: (Ord k) => Map k a -> Set k -> Map k a
withoutKeys m s = Map.filterWithKey (\k _ -> k `Set.notMember` s) m

varsInVal :: Val -> Set Name
varsInVal = cata $ \case
  VarTagNodeF n ns -> Set.unions $ (Set.singleton n):ns
  VarF n           -> Set.singleton n
  _ -> Set.empty

varsInExp :: ExpF a -> Set Name
varsInExp = \case
  ECaseF    val alts        -> varsInVal val
  SAppF     name simpleVals -> Set.unions $ map varsInVal simpleVals
  SReturnF  val             -> varsInVal val
  SStoreF   val             -> varsInVal val
  SFetchIF  name pos        -> Set.singleton name
  SUpdateF  name val        -> Set.singleton name <> varsInVal val
  _ -> Set.empty

-- linter for low level grin
lintLowLevelGrin :: Exp -> All -- TODO: collect errors
lintLowLevelGrin = cata folder where
  folder = \case
    SReturnF  val   -> mempty
    e -> Data.Foldable.fold e
