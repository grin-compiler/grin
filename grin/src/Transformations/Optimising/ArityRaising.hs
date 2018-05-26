{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.Optimising.ArityRaising where

import Control.Arrow
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Grin
import TypeEnv
import Debug.Trace
import Lens.Micro.Platform
import qualified Data.Vector as Vector
import Control.Monad
import Data.Monoid hiding (Alt)
import Data.Functor.Infix
import Data.List as List
import Control.Monad.State
import Transformations.Util (apoM)


type VarM a = State TypeEnv a

runVarM :: TypeEnv -> VarM a -> (TypeEnv, a)
runVarM te = (\(f,s) -> (s,f)) . flip runState te

changeParams :: [(Name, [SimpleType])] -> [(Name, Type)] -> [(Name, [(Name, Type)])]
changeParams tagParams names = map
  (\(name, t) -> maybe (name, [(name, t)]) (((,) name) . newParams name) $ List.lookup name tagParams)
  names

newParams :: Name -> [SimpleType] -> [(Name, Type)]
newParams name [ty] = [(name, T_SimpleType ty)]
newParams name types  = flip map (types `zip` [1 ..]) $ \(t, i) -> (name <> show i, T_SimpleType t)

-- TODO: Create unique names
arityRaising :: (TypeEnv, Exp) -> (TypeEnv, Exp)
arityRaising (te, exp) = runVarM te (apoM builder ([], exp))
  where
    candidates :: Map Name [(Name, Int, (Tag, Vector SimpleType))]
    candidates =
      flip examineCallees (te,exp) $
      flip examineCallers exp $
      examineTheParameters (te, exp)

    nodeParamMap :: Map Name (Tag, Vector SimpleType)
    nodeParamMap = Map.fromList $ map (\(n, _i, ts) -> (n, ts)) $ concat $ Map.elems candidates

    -- Set of stores in the function body.
    collectStores :: Exp -> [(Name, Val)]
    collectStores = para $ \case
      SBlockF (_, body)  -> body
      AltF _ (_, body)   -> body
      ECaseF _ alts -> mconcat $ map snd alts
      EBindF (SStore node, _) (Var v) (_, rhs) -> [(v,node)] <> rhs
      EBindF (_, lhs) _ (_, rhs) -> lhs <> rhs
      _ -> mempty

    -- The substituition that contains a Node or a list of new invariant parameters
    builder :: ([(Name, Either Val [Name])], Exp) -> VarM (ExpF (Either Exp ([(Name, Either Val [Name])], Exp)))
    builder (substs0, exp0) = case exp0 of
      Def name params0 body ->
        let substs1 = map (second Left) $ collectStores body
        in case Map.lookup name candidates of
            Nothing        -> pure $ DefF name params0 (Right (substs1, body))
            Just tagParams -> do
              (Just (t, paramsTypes0)) <- use (function . at name)
              let oldToNewParams = changeParams
                    (map (\(n, i, (t, ts)) -> (n, Vector.toList ts)) tagParams)
                    (params0 `zip` (Vector.toList paramsTypes0))
              forM oldToNewParams $ \(old, news) -> do
                variable . at old .= Nothing
                forM news $ \(newName, newType) -> (variable . at newName) .= Just newType
              let params1 = concatMap snd oldToNewParams
                  (paramNames, paramTypes) = unzip params1
              let substs2 = substs1 ++ map (second (Right . map fst)) oldToNewParams
              function . at name %= fmap (second (const (Vector.fromList paramTypes)))
              pure $ DefF name paramNames (Right (substs2, body))

      SApp name params -> pure $ case (Map.lookup name candidates) of
        Nothing -> SAppF name params
        Just parametersToChange -> SAppF name $ flip concatMap ([1..] `zip` params) $ \case
          (_, Lit l) -> [Lit l]
          (i, Var v) -> case (List.find (\(_, i0, _) -> i == i0) parametersToChange) of
            Nothing -> case List.lookup v substs0 of
              Just (Right [name]) -> [Var name]
              _                   -> [Var v]
            Just _  -> case List.lookup v substs0 of
              Nothing -> [Var v]
              Just (Left (ConstTagNode tag vals)) -> vals -- The tag node should have the arity as in the candidates
              Just (Left (Var v)) -> [Var v]
              Just (Right names) -> map Var names

      SFetchI name pos -> case (Map.lookup name nodeParamMap) of
        Nothing        -> pure $ SFetchIF name pos
        Just (tag, ps) -> do
          let params = newParams name (Vector.toList ps)
          forM_ params $ \(newName, newType) ->
            variable . at newName .= Just newType
          pure $ SReturnF $ ConstTagNode tag $ ((Var . fst) <$> params)

      rest -> pure $ fmap (Right . (,) substs0) $ project rest

-- Return the functions that has node set parameters with unique names
examineTheParameters :: (TypeEnv, Exp) -> Map Name [(Name, Int, (Tag, Vector SimpleType))]
examineTheParameters (te, e) = Map.filter (not . null) $ Map.map candidate funs
  where
    funs :: Map Name [(Name, Type)]
    funs = Map.intersectionWith combine (_function te) funParamNames
    combine (_tr, pt) params = params `zip` (Vector.toList pt)

    funParamNames = flip cata e $ \case
      ProgramF defs      -> mconcat defs
      DefF name params _ -> Map.singleton name params
      _                  -> mempty

    candidate :: [(Name, Type)] -> [(Name, Int, (Tag, Vector SimpleType))]
    candidate ns = flip mapMaybe (ns `zip` [1..]) $ \((name, typ), idx) -> (,,) name idx <$>
      typ ^? _T_SimpleType
           . _T_Location
           . to (sameNodeOnLocations te)
           . _Just

-- MonoidMap
newtype MMap k m = MMap { unMMap :: Map k m }
  deriving Show

instance (Ord k, Monoid m) => Monoid (MMap k m) where
  mempty = MMap mempty
  mappend (MMap m1) (MMap m2) = MMap (Map.unionWith mappend m1 m2)

-- | Examine the function calls in the body.
examineCallers :: Map Name [(Name, Int, (Tag, Vector SimpleType))] -> Exp -> Map Name [(Name, Int, (Tag, Vector SimpleType))]
examineCallers candidates e =
    Map.difference candidates $
    (\(_,_,exclude,_) -> Map.fromSet (const ()) exclude) $
    para collect e
  where
    inCandidates :: Name -> (Name, Int) -> Bool
    inCandidates fn (funName, nth)
        -- Non-recursive case
      | fn /= funName = Map.member funName candidates
        -- Recursive case
      | otherwise = isJust $ do -- Maybe
          params <- Map.lookup funName candidates
          List.find ((nth ==) . view _2) params

    -- Function calls in body: VarName -> [(FunName, Nth param)]
    -- Name of parameters to be checked
    -- Name of functions to be excluded
    -- Name of parameters not bound to a store
    collect :: ExpF (Exp, (MMap Name [(Name, Int)], Set Name, Set Name, Set Name)) -> (MMap Name [(Name, Int)], Set Name, Set Name, Set Name)
    collect = \case
      ProgramF defs -> mconcat $ map snd defs

      DefF name params (_, (MMap calls, callsParam, _, nonStored)) ->
        let recFunParams = fromMaybe [] (view _1 <$$> Map.lookup name candidates)
            params0 = Set.fromList $ params \\ recFunParams
        in ( mempty
           , mempty
           , Set.map fst $ Set.fromList $ filter (inCandidates name) $ -- Only interested in candidates
             concatMap (\p -> fromMaybe [] $ Map.lookup p calls) $ -- Every function that uses the parameters
             (nonStored `Set.union` params0) `Set.intersection` callsParam -- Call parameters should be stored
           , mempty
           )

      SBlockF (_, body) -> body
      ECaseF _ alts     -> mconcat $ map snd alts
      EBindF (SStore (ConstTagNode _ _), lhs) (Var _) (_, rhs) -> rhs
      EBindF (_, lhs) pat (_, rhs)      -> mconcat [lhs, rhs, (mempty, mempty, mempty, vars pat)]

      AltF (NodePat _ names) (_, body) -> body <> (mempty, mempty, mempty, Set.fromList names)
      AltF _                 (_, body) -> body
      SAppF name params ->
        ( mconcat $
          map (\(p, i) -> MMap (Map.singleton p [(name, i)])) $
          concatMap (\(p,i) -> (map (flip (,) i) (Set.toList $ vars p))) (params `zip` [1..])
        , Set.unions (vars <$> params)
        , mempty
        , mempty
        )

      _ -> mempty

-- Keep the parameters that are part of an invariant calls only,
-- or an argument to a fetch.
examineCallees :: Map Name [(Name, Int, (Tag, Vector SimpleType))] -> (TypeEnv, Exp) -> Map Name [(Name, Int, (Tag, Vector SimpleType))]
examineCallees funParams (te, exp) =
    Map.mapMaybe (nonEmpty . (filter ((`Set.notMember` others) . view _1))) funParams
  where
    others = fst $ cata collect exp

    collect :: ExpF (Set Name, [(Name, Name)]) -> (Set Name, [(Name, Name)])
    collect = \case
      ProgramF  defs             -> mconcat defs

      DefF name params body@(others, funCalls)
        | Map.member name funParams ->
            -- non recursive function call parameters mut be included in other parameters.
            let otherCallParams = Set.fromList $ map snd $ filter ((name /=) . view _1) funCalls
            in (others `Set.union` otherCallParams, mempty)
        | otherwise                 -> mempty

      SBlockF   body             -> body
      EBindF    lhs _ rhs        -> lhs <> rhs
      ECaseF    val as           -> (vars val, mempty) <> mconcat as
      AltF cpat body             -> body

      SReturnF  val      -> (vars val, mempty)
      SStoreF   val      -> (vars val, mempty)
      SUpdateF  name val -> (Set.insert name (vars val), mempty)
      SAppF name params  -> (mempty, (,) name <$> concatMap (Set.toList . vars) params)

      _ -> mempty -- Update and SApp parameters don't need to be crossed out

vars :: Val -> Set Name
vars = Set.fromList . \case
  Var n             -> [n]
  ConstTagNode _ vs -> vs ^.. each . _Var
  VarTagNode n vs   -> n : (vs ^.. each . _Var)
  _                 -> []

sameNodeOnLocations :: TypeEnv -> [Int] -> Maybe (Tag, Vector SimpleType)
sameNodeOnLocations te is =
  fmap (second (Vector.map unLEST)) $
  join $
  allSame $
  map (fmap (second (Vector.map LEST))) $
  map (oneNodeOnLocation te) is

-- Ignore the location differences.
newtype LocEqSimpleType = LEST { unLEST :: SimpleType }
  deriving Show

instance Eq LocEqSimpleType where
  (LEST (T_Location _)) == (LEST (T_Location _)) = True
  (LEST a)              == (LEST b)              = a == b

-- | NonEmpty tags
oneNodeOnLocation :: TypeEnv -> Int -> Maybe (Tag, Vector SimpleType)
oneNodeOnLocation te idx = case (Map.size ns) of
  1 -> listToMaybe $ mapMaybe checkNonEmptyTag $ Map.toList ns
  _ -> Nothing
  where
    ns = (_location te) Vector.! idx
    checkNonEmptyTag :: (Tag, Vector SimpleType) -> Maybe (Tag, Vector SimpleType)
    checkNonEmptyTag tv@(t, vs)
      | Vector.null vs = Nothing
      | otherwise      = Just tv

allSame :: (Eq a) => [a] -> Maybe a
allSame []     = Nothing
allSame [a]    = Just a
allSame (a:as) = if all (a==) as then Just a else Nothing

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs
