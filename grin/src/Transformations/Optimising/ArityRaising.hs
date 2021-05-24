{-# LANGUAGE LambdaCase #-}
module Transformations.Optimising.ArityRaising where

import Grin.Grin (packName, unpackName)
import Grin.Syntax
import Grin.TypeEnv
import Data.Functor.Foldable

import Data.List (nub)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Monoid
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.Map.Strict as Map; import Data.Map (Map)
import qualified Data.Vector as Vector; import Data.Vector (Vector)
import Control.Monad.State.Strict
import Transformations.Names (ExpChanges(..))

{-
1. Select one function which has a parameter of a pointer to one constructor only.
2. If the parameter is linear and fetched in the function body then this is a good function for
   arity raising

How to raise arity?
1. Change the function parameters: replace the parameter with the parameters in the constructor
2. Change the function body: remove the fectch and use the variables as parameters
3. Change the caller sides: instead of passing the pointer fetch the pointer and pass the values are parameters

How to handle self recursion?
1. If a function is self recursive, the paramter that is fetched originaly in the function body
   must be passed as normal parameters in the same function call.

Phase 1: Select a function and a parameter to transform.
Phase 2: Transform the parameter and the function body.
Phase 3: Transform the callers.

This way the fetches propagates slowly to the caller side to the creational point.

Parameters:
 - Used only in fetch or in recursive calls for the same function.
 - Its value points to a location, which location has only one Node with at least one parameter
-}

-- TODO: True is reported even if exp stayed the same. Investigate why exp stay the same
-- for non-null arity data.
arityRaising :: Int -> TypeEnv -> Exp -> (Exp, ExpChanges)
arityRaising n te exp = if Map.null arityData then (exp, NoChange) else (phase2 n arityData exp, NewNames)
  where
    arityData = phase1 te exp

-- | ArityData maps a function name to its arguments that can be arity raised.
-- 1st: Name of the argument
-- 2nd: The index of the argument
-- 3rd: The tag and one possible locaition where the parameter can point to.
type ArityData = Map Name [(Name, Int, (Tag, Int))]

type ParameterInfo = Map Name (Int, (Tag, Int))

data Phase1Data
  = ProgramData { pdArityData :: ArityData }
  | FunData { fdArityData :: ArityData }
  | BodyData { bdFunCall :: [(Name, Name)]
             , bdFetch   :: Map Name Int
             , bdOther   :: [Name]
             }
  deriving (Show)

instance Semigroup Phase1Data where
  (ProgramData ad0) <> (ProgramData ad1) = ProgramData (Map.unionWith mappend ad0 ad1)
  (FunData fd0) <> (FunData fd1) = FunData (mappend fd0 fd1)
  (BodyData c0 f0 o0) <> (BodyData c1 f1 o1) = BodyData (c0 ++ c1) (Map.unionWith (+) f0 f1) (o0 ++ o1)

instance Monoid Phase1Data where
  mempty = BodyData mempty mempty mempty

variableInVar   = \case { Var n -> [n]; _ -> [] }
variableInNode  = \case { ConstTagNode _ vs -> concatMap variableInVar vs; _ -> [] }
variableInNodes = concatMap variableInNode

phase1 :: TypeEnv -> Exp -> ArityData
phase1 te = pdArityData . cata collect where
  collect :: ExpF Phase1Data -> Phase1Data
  collect = \case
    SAppF fn ps       -> mempty { bdFunCall = [ (fn, v) | Var v <- ps], bdOther = variableInNodes ps }
    SFetchIF var _    -> mempty { bdFetch = Map.singleton var 1 }
    SUpdateF var val  -> mempty { bdOther = [var] ++ variableInNode val ++ variableInVar val }
    SReturnF val -> mempty { bdOther = variableInNode val ++ variableInVar val }
    SStoreF v  -> mempty { bdOther = variableInNode v ++ variableInVar v }
    SBlockF ad -> ad
    AltF _ ad  -> ad
    ECaseF v alts    -> mconcat alts <> mempty { bdOther = variableInNode v ++ variableInVar v }
    EBindF lhs _ rhs -> lhs <> rhs

    -- Keep the parameters that are locations and points to a single node with at least one parameters
    -- - that are not appear in others
    -- - that are not appear in other function calls
    -- - that are fetched at least once
    DefF fn ps body ->
      let funData =
            [ (p,i,(fromJust mtag))
            | (p,i) <- ps `zip` [1..]
            , Map.member p (bdFetch body)
            , let mtag = pointsToOneNode te p
            , isJust mtag
            , p `notElem` (bdOther body)
            , p `notElem` (snd <$> (filter ((/=fn) . fst) (bdFunCall body)))
            , fn /= "grinMain"
            ]
      in FunData $ case funData of
          [] -> Map.empty
          _  -> Map.singleton fn funData

    ProgramF exts defs -> ProgramData $ Map.unionsWith mappend (fdArityData <$> defs)

pointsToOneNode :: TypeEnv -> Name -> Maybe (Tag, Int)
pointsToOneNode te var = case Map.lookup var (_variable te) of
  (Just (T_SimpleType (T_Location locs))) -> case nub $ concatMap Map.keys $ ((_location te) Vector.!) <$> locs of
    [tag] -> Just (tag, Vector.length $ head $ Map.elems $ (_location te) Vector.! (head locs))
    _ -> Nothing
  _ -> Nothing

type VarM a = State Int a

{-
Phase2 and Phase3 can be implemented in one go.

Change only the functions which are in the ArityData map, left the others out.
 * Change fetches to pure, using the tag information provided
 * Change funcall parameters
 * Change fundef parameters

Use the original parameter name with new indices, thus we dont need a name generator.
-}
phase2 :: Int -> ArityData -> Exp -> Exp
phase2 n arityData = flip evalState 0 . cata change where
  fetchParNames :: Name -> Int -> Int -> [Name]
  fetchParNames nm idx i = (\j -> packName $ concat [unpackName nm,".",show n,".",show idx,".arity.",show j]) <$> [1..i]

  newParNames :: Name -> Int -> [Name]
  newParNames nm i = (\j -> packName $ concat [unpackName nm,".",show n,".arity.",show j]) <$> [1..i]

  parameterInfo :: ParameterInfo
  parameterInfo = Map.fromList $ map (\(n,ith,tag) -> (n, (ith, tag))) $ concat $ Map.elems arityData

  replace_parameters_with_new_ones = concatMap $ \case
    p | Just (nth, (tag, ps)) <- Map.lookup p parameterInfo ->
        newParNames p ps
      | otherwise -> [p]

  change :: ExpF (VarM Exp) -> (VarM Exp)
  change = \case
    {- Change only function bodies that are in the ArityData
        from: (CNode c1 cn) <- fetch pi
          to: (CNode c1 cn) <- pure (CNode pi1 pin)

        from: funcall p1 pi pn
          to: rec-funcall p1 pi1 pin pn
          to: do (CNode c1 cn) <- fetch pi
                 non-rec-funcall p1 c1 cn pn

        from: fundef p1 pi pn
          to: fundef p1 pi1 pin pn
    -}
    SFetchIF var idx
      | Just (nth, (tag, ps)) <- Map.lookup var parameterInfo ->
        pure $ SReturn (ConstTagNode tag (Var <$> newParNames var ps))
      | otherwise ->
        pure $ SFetchI var idx

    SAppF f fps
      | Just aritedParams <- Map.lookup f arityData -> do
        idx <- get
        let qsi = Map.fromList $ map (\(_,i,t) -> (i,t)) aritedParams
            nsi = Map.fromList $ map (\(n,i,t) -> (n,t)) aritedParams
            psi = [1..] `zip` fps
            newPs = flip concatMap psi $ \case
              (_, Var n) | Just (t, jth) <- Map.lookup n nsi -> Var <$> newParNames n jth
              (i, Var n) | Just (t, jth) <- Map.lookup i qsi -> Var <$> fetchParNames n idx jth
              (i, Undefined{}) | Just (_, jth) <- Map.lookup i qsi -> replicate jth (Undefined dead_t)
              (_, other) -> [other]
            fetches = flip mapMaybe psi $ \case
              (_, Var n) | Just _ <- Map.lookup n nsi -> Nothing
              (i, Var n) | Just (t, jth) <- Map.lookup i qsi ->
                Just ((ConstTagNode t (Var <$> fetchParNames n idx jth)), SFetchI n Nothing)
              _ -> Nothing
        put (idx + 1)
        pure $ case fetches of
            [] -> SApp f newPs
            _  -> SBlock $ foldr (\(pat, fetch) rest -> EBind fetch pat rest) (SApp f newPs) fetches
      | otherwise ->
        pure $ SApp f fps

    DefF f ps new
      | Map.member f arityData -> Def f (replace_parameters_with_new_ones ps) <$> new
      | otherwise              -> Def f ps <$> new

    rest -> embed <$> sequence rest
