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

changeParams :: [(Name, [SimpleType])] -> [Name] -> VarM [Name]
changeParams tagParams names = concat <$> mapM
  (\name -> maybe (pure [name]) (newParams name) $ List.lookup name tagParams)
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
arityRaising :: (TypeEnv, Exp) -> (TypeEnv, Exp)
arityRaising (te, exp) = runVarM te (apoM builder (Nothing, exp))
  where
    candidates :: Map Name [(Name, (Tag, Vector SimpleType))]
    candidates = flip examineCallees (te,exp) $ examineTheParameters (te, exp)

    canditateOriginalParams :: Map Name [Name]
    canditateOriginalParams = Map.intersectionWith (\ps _ -> ps) (definedFunctions exp) candidates

    nodeParamMap :: Map Name (Tag, Vector SimpleType)
    nodeParamMap = Map.fromList $ concat $ Map.elems candidates

    builder :: (Maybe Name, Exp) -> VarM (ExpF (Either Exp (Maybe Name, Exp)))
    builder (fun, exp0) = case exp0 of
      Def name params body -> case Map.lookup name candidates of
        Nothing        -> pure $ DefF name params (Right (Just name, body))
        Just tagParams -> DefF name <$> (changeParams (map (second (Vector.toList . snd)) tagParams) params) <@> (Right (Just name, body))

      SApp name params -> case (Map.lookup name candidates) of
        Nothing -> pure $ SAppF name params
        Just ps -> do
          let params' = (fromJust $ Map.lookup name canditateOriginalParams) `zip` params

          appNode <- (SApp name . concat) <$> mapM
                  (\(oname, actVal) -> case actVal of
                      Lit l -> pure [Lit l]
                      Var v -> case Map.lookup oname nodeParamMap of
                        Nothing       -> pure [Var v]
                        Just (_t, ts) -> (Var <$$> newParams v (Vector.toList ts)))
                  params'

          (SBlockF . Left) <$>
              foldM (\rest (pname, (tag, ptypes)) -> do
                -- pname is in the list, and node values can be passed to a function only in form of variables.
                let localPName = (\(Var name) -> name) $ fromJust $ List.lookup pname params'
                EBind
                  (SFetch localPName) <$>
                  (ConstTagNode tag <$> (Var <$$> newParams localPName (Vector.toList ptypes))) <@>
                  rest)
                appNode
                (reverse ps)


      SFetchI name pos -> case (Map.lookup name nodeParamMap) of
        Nothing        -> pure $ SFetchIF name pos
        Just (tag, ps) -> (SReturnF . ConstTagNode tag) <$> (Var <$$> newParams name (Vector.toList ps))

      rest -> pure $ fmap (Right . (,) fun) $ project rest

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
