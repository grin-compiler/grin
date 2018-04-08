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


changeParams :: [(Name, Int)] -> [Name] -> [Name]
changeParams tagParams = concatMap $ \name ->
  maybe [name] (newParams name) $ List.lookup name tagParams

newParams :: Name -> Int -> [Name]
newParams name n = map ((name <>) . show) [1 .. n]

-- TODO: Change TypeEnv
-- - Remove transformed parameters
-- - Add new parameters
-- TODO: Create unique names
arityRaising :: (TypeEnv, Exp) -> (TypeEnv, Exp)
arityRaising (te, exp) = (te, apo build (Nothing, exp))
  where
    candidates :: Map Name [(Name, (Tag, Vector SimpleType))]
    candidates = flip examineCallees (te,exp) $ examineTheParameters (te, exp)

    canditateOriginalParams :: Map Name [Name]
    canditateOriginalParams = Map.intersectionWith (\ps _ -> ps) (definedFunctions exp) candidates

    nodeParamMap :: Map Name (Tag, Vector SimpleType)
    nodeParamMap = Map.fromList $ concat $ Map.elems candidates

    build :: (Maybe Name, Exp) -> ExpF (Either Exp (Maybe Name, Exp))
    build (fun, exp0) = case exp0 of
      Def name params body -> case (fmap (second (Vector.length . snd))) <$> Map.lookup name candidates of
        Nothing        -> DefF name params (Right (Just name, body))
        Just tagParams -> DefF name (changeParams tagParams params) (Right (Just name, body))

      SApp name params -> case (Map.lookup name candidates) of
        Nothing -> SAppF name params
        Just ps ->
          let params' = (fromJust $ Map.lookup name canditateOriginalParams) `zip` params
          in SBlockF $ Left $
              foldr (\(pname, (tag, ptypes)) rest ->
                -- pname is in the list, and node values can be passed to a function only in form of variables.
                let localPName = (\(Var name) -> name) $ fromJust $ List.lookup pname params'
                in EBind
                    (SFetch localPName)
                    (ConstTagNode tag (Var <$> newParams localPName (Vector.length ptypes))) rest)
                (SApp name (concatMap
                  (\(oname, actVal) -> case actVal of
                      Lit l -> [Lit l]
                      Var v -> case Map.lookup oname nodeParamMap of
                        Nothing       -> [Var v]
                        Just (_t, ts) -> (Var <$> newParams v (Vector.length ts)))
                  params'))
                ps

      SFetchI name pos -> case (Map.lookup name nodeParamMap) of
        Nothing        -> SFetchIF name pos
        Just (tag, ps) -> SReturnF $ ConstTagNode tag (Var <$> newParams name (Vector.length ps))

      rest -> fmap (Right . (,) fun) $ project rest

definedFunctions :: Exp -> Map Name [Name]
definedFunctions = cata $ \case
  ProgramF defs      -> mconcat defs
  DefF name params _ -> Map.singleton name params
  _                  -> mempty

{-
Step 1: Examine the function parameter types
Step 2: Examine the callees
Step 3: Transform
-}

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

-- Keep the parameters that are part of an invariant calls,
-- or an argument to a fetch.
examineCallees :: Map Name [(Name, (Tag, Vector SimpleType))] -> (TypeEnv, Exp) -> Map Name [(Name, (Tag, Vector SimpleType))]
examineCallees funParams (te, exp) =
    Map.mapMaybe (nonEmpty . (filter ((`Set.notMember` others) . fst))) funParams
  where
    others = cata collect exp

    vars :: Val -> Set Name
    vars = Set.fromList . \case
      Var n             -> [n]
      ConstTagNode _ vs -> vs ^.. each . _Var
      VarTagNode n vs   -> n : (vs ^.. each . _Var)
      _                 -> []

    collect :: ExpF (Set Name) -> Set Name
    collect = \case
      ProgramF  defs             -> mconcat defs
      DefF      name params body -> body
      SBlockF   body             -> body
      EBindF    lhs _ rhs        -> lhs <> rhs
      ECaseF    val as           -> mconcat as
      AltF cpat body             -> body

      SReturnF  val      -> vars val
      SStoreF   val      -> vars val
      SUpdateF  name val -> Set.insert name (vars val)
      _ -> mempty

sameNodeOnLocations :: TypeEnv -> [Int] -> Maybe (Tag, Vector SimpleType)
sameNodeOnLocations te is = join $ allSame $ map (oneNodeOnLocation te) is

oneNodeOnLocation :: TypeEnv -> Int -> Maybe (Tag, Vector SimpleType)
oneNodeOnLocation te idx = case (Map.size ns) of
  1 -> Just $ head $ Map.toList ns
  _ -> Nothing
  where
    ns = (_location te) Vector.! idx

allSame :: (Eq a) => [a] -> Maybe a
allSame []     = Nothing
allSame [a]    = Just a
allSame (a:as) = if all (a==) as then Just a else Nothing

nonEmpty :: [a] -> Maybe [a]
nonEmpty [] = Nothing
nonEmpty xs = Just xs
