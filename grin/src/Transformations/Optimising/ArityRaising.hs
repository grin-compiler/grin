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

changeParams :: [(Name, [SimpleType])] -> [Name] -> VarM [(Name, [Name])]
changeParams tagParams names = mapM
  (\name -> maybe (pure (name, [name])) (fmap ((,) name) . newParams name) $ List.lookup name tagParams)
  names

newParams :: Name -> [SimpleType] -> VarM [Name]
newParams name types = forM (types `zip` [1 ..]) $ \(t, i) -> do
  let name' = name <> show i
  variable %= Map.insert name' (T_SimpleType t)
  pure name'

infixl 4 <@>
(<@>) :: (Applicative f) => f (a -> b) -> a -> f b
f <@> x = f <*> (pure x)

-- TODO: Change TypeEnv
-- - Remove transformed parameters, from TypeEnv
-- TODO: Create unique names
-- TODO: Improve: Also check the caller sites of the selected funcions: It should be a store on the parmeters, used
-- by the candidates.
arityRaising :: (TypeEnv, Exp) -> (TypeEnv, Exp)
arityRaising (te, exp) = runVarM te (apoM builder ([], exp))
  where
    candidates :: Map Name [(Name, (Tag, Vector SimpleType))]
    candidates = flip examineCallees (te,exp) $ flip examineCallers exp $ examineTheParameters (te, exp)

    canditateOriginalParams :: Map Name [Name]
    canditateOriginalParams = Map.intersectionWith (\ps _ -> ps) (definedFunctions exp) candidates

    nodeParamMap :: Map Name (Tag, Vector SimpleType)
    nodeParamMap = Map.fromList $ concat $ Map.elems candidates

    -- Set of stores in the function body.
    collectStores :: Exp -> [(Name, Val)]
    collectStores = para $ \case
      SBlockF (_, body)  -> body
      AltF _ (_, body)   -> body
      ECaseF _ alts -> mconcat $ map snd alts
      EBindF (SStore node, _) (Var v) (_, rhs) -> [(v,node)] <> rhs
      EBindF (_, lhs) _ (_, rhs) -> lhs <> rhs
      _ -> mempty

    -- The name in the current scope: TODO: Remove
    -- The substituition that contains a Node or a list of new invariant parameters
    builder :: ([(Name, Either Val [Name])], Exp) -> VarM (ExpF (Either Exp ([(Name, Either Val [Name])], Exp)))
    builder (substs0, exp0) = case exp0 of
      Def name params0 body ->
        let substs1 = map (second Left) $ collectStores body
        in case Map.lookup name candidates of
            Nothing        -> pure $ DefF name params0 (Right (substs1, body))
            Just tagParams -> do
              oldToNewParams <- changeParams (map (second (Vector.toList . snd)) tagParams) params0
              let params1 = concatMap snd oldToNewParams
              let substs2 = substs1 ++ map (second Right) oldToNewParams
              pure $ DefF name params1 (Right (substs2, body))

      SApp name params -> pure $ case (Map.lookup name candidates) of
        Nothing -> SAppF name params
        Just _  -> SAppF name $ flip concatMap params $ \case
          Lit l -> [Lit l]
          Var v -> case List.lookup v substs0 of
            Nothing -> [Var v]
            Just (Left (ConstTagNode tag vals)) -> vals -- The tag node should have the arity as in the candidates
            Just (Right names) -> map Var names

      SFetchI name pos -> case (Map.lookup name nodeParamMap) of
        Nothing        -> pure $ SFetchIF name pos
        Just (tag, ps) -> (SReturnF . ConstTagNode tag) <$> (Var <$$> newParams name (Vector.toList ps))

      rest -> pure $ fmap (Right . (,) substs0) $ project rest

definedFunctions :: Exp -> Map Name [Name]
definedFunctions = cata $ \case
  ProgramF defs      -> mconcat defs
  DefF name params _ -> Map.singleton name params
  _                  -> mempty

-- Return the functions that has node set parameters with unique names
examineTheParameters :: (TypeEnv, Exp) -> Map Name [(Name, (Tag, Vector SimpleType))]
examineTheParameters (te, e) = Map.filter (not . null) $ Map.map candidate funs
  where
    funs :: Map Name [(Name, Type)]
    funs = Map.intersectionWith combine (_function te) funParamNames
    combine (_tr, pt) params = params `zip` (Vector.toList pt)

    funParamNames = flip cata e $ \case
      ProgramF defs      -> mconcat defs
      DefF name params _ -> Map.singleton name params
      _                  -> mempty

    candidate :: [(Name, Type)] -> [(Name, (Tag, Vector SimpleType))]
    candidate = mapMaybe $ \(name, typ) -> (,) name <$>
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

-- | Restrict candidate set, where parameters of a function call are fetched.
examineCallers :: Map Name [(Name, (Tag, Vector SimpleType))] -> Exp -> Map Name [(Name, (Tag, Vector SimpleType))]
examineCallers candidates e = Map.difference candidates $ (\(_,_,exclude,_) -> Map.fromSet (const ()) exclude) $ para collect e where
  -- Function calls in body: ParamName -> [FunName]
  -- Name of parameters to be checked
  -- Name of functions to be excluded
  -- Name of parameters not bound to a fetch
  collect :: ExpF (Exp, (MMap Name [Name], Set Name, Set Name, Set Name)) -> (MMap Name [Name], Set Name, Set Name, Set Name)
  collect = \case
    ProgramF defs -> mconcat $ map snd defs

    DefF name params (_, (MMap calls, callsParam, _, nonFetched)) ->
      ( mempty
      , mempty
      , Set.fromList $ concatMap (\p -> fromMaybe [] $ Map.lookup p calls) $ nonFetched `Set.difference` callsParam
      , mempty
      )

    SBlockF (_, body) -> body
    ECaseF _ alts     -> mconcat $ map snd alts
    EBindF (SFetchI _ _, lhs) _ (_, rhs) -> rhs
    EBindF (_, lhs) pat (_, rhs)      -> mconcat [lhs, rhs, (mempty, mempty, mempty, vars pat)]

    AltF cpat (_, body) -> body <> (mempty, mempty, mempty, cpatVars cpat)
    SAppF name params ->
      ( MMap $ Map.fromSet (const [name]) $ Set.unions $ (vars <$> params)
      , Set.unions (vars <$> params)
      , mempty
      , mempty
      )

    SReturnF  val -> (mempty, mempty, mempty, vars val)
    SStoreF   val -> (mempty, mempty, mempty, vars val)
    SUpdateF  name val -> (mempty, mempty, mempty, vars val)

    _ -> mempty

-- Keep the parameters that are part of an invariant calls,
-- or an argument to a fetch.
examineCallees :: Map Name [(Name, (Tag, Vector SimpleType))] -> (TypeEnv, Exp) -> Map Name [(Name, (Tag, Vector SimpleType))]
examineCallees funParams (te, exp) =
    Map.mapMaybe (nonEmpty . (filter ((`Set.notMember` others) . fst))) funParams
  where
    others = cata collect exp

    collect :: ExpF (Set Name) -> Set Name
    collect = \case
      ProgramF  defs             -> mconcat defs

      DefF name params body
        | Map.member name funParams -> body
        | otherwise                 -> mempty

      SBlockF   body             -> body
      EBindF    lhs _ rhs        -> lhs <> rhs
      ECaseF    val as           -> vars val <> mconcat as
      AltF cpat body             -> body

      SReturnF  val      -> vars val
      SStoreF   val      -> vars val
      SUpdateF  name val -> Set.insert name (vars val)

      _ -> mempty -- Update and SApp parameters don't need to be crossed out

vars :: Val -> Set Name
vars = Set.fromList . \case
  Var n             -> [n]
  ConstTagNode _ vs -> vs ^.. each . _Var
  VarTagNode n vs   -> n : (vs ^.. each . _Var)
  _                 -> []

cpatVars :: CPat -> Set Name
cpatVars = Set.fromList . \case
  NodePat _ names -> names
  _               -> mempty

sameNodeOnLocations :: TypeEnv -> [Int] -> Maybe (Tag, Vector SimpleType)
sameNodeOnLocations te is = join $ allSame $ map (oneNodeOnLocation te) is

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
