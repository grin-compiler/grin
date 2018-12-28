{-# LANGUAGE TemplateHaskell, LambdaCase, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, QuasiQuotes #-}
{-# LANGUAGE DeriveFunctor, RankNTypes, TypeApplications, OverloadedStrings #-}
module AbstractInterpretation.Model where

import Grin.Grin hiding (SimpleType(..), Loc)
import Grin.TH hiding (text)
import AbstractInterpretation.HeapPointsTo.Pretty
import AbstractInterpretation.HeapPointsTo.Result (HPTResult(..), SimpleType(..), Loc)
import qualified AbstractInterpretation.HeapPointsTo.Result as HPT
import AbstractInterpretation.Sharing.Result
import AbstractInterpretation.Sharing.Pretty

import Data.Functor.Foldable
import Lens.Micro.Platform
import Data.Map as Map
import Data.Monoid ((<>))
import Control.Monad.Identity
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Data.Maybe as Maybe
import Data.List as List (foldl', nub)
import Data.Bifunctor
import Debug.Trace
import Data.Foldable

{-
TODO:
[ ] Handle VarTagNodes
[ ] Handle updates
    [ ] Create two phases, the first phase should run before eval inlining, the second after
    [ ] First phase ignores updates
    [ ] Second phase handles update
[x] Sharing analysis
    [x] Collect non-linear variables
    [x] Add sharing heap
    [x] An abstract location is set to be shared
        [x] Pointed by a non-linear variable:
        [x] From another shared location
    [x] In the abstract heap the return value part of the location is only unioned if the same abstract location becomes shared
[x] Implement warnings
[ ] Check emptyness of nodeset and simpletypes when converting from TypeSet
[ ] Handle default alternatives
[ ] Create test case
-}

data Ind = IN Name | IF Name | IFP FParam deriving (Show, Eq, Ord)

data Ref i a = Ind i | Dir a deriving (Show, Eq, Ord, Functor) -- TODO: Improve indirection

newtype FName = FName { unFName :: Name } deriving (Show, Eq, Ord)

data FParam = FParam Name Int deriving (Show, Eq, Ord)

type PrimitiveTypeSet = Set.Set (Ref Ind SimpleType)

data TypeSet = TypeSet { _simpleTypes :: PrimitiveTypeSet , _nodes :: NodeSet } deriving (Show, Eq, Ord)

newtype NodeSet = NodeSet { _nodeSet :: Map.Map Tag [PrimitiveTypeSet] } deriving (Show, Eq, Ord)

makeLenses ''NodeSet

data FetchTo
  = FetchName Name
  | FetchNode Name Tag Int
  | FetchFunc Name
  deriving (Show, Eq, Ord)

data Equation
  = VVar   Name   (Ref Ind TypeSet)
  | Param  FParam (Ref Ind TypeSet)
  | Fun    Name   (Ref Ind TypeSet)
  | Access Name   (Ref Ind TypeSet, Tag, Int)
  -- Heap
  | Store  Int  (Ref Ind NodeSet)
  | Fetch  FetchTo (Ref Ind TypeSet) -- Fetches from pointer variables
  | Update Name (Ref Ind NodeSet)
  deriving (Show, Eq, Ord)

type Warning = String

data BuildState = BuildState
  { _maxLoc :: Int
  , _equations :: [Equation]
  , _functions :: Map.Map Name Int -- Arity of the function
  , _nonLinearVars :: Set.Set Name
  , _warnings :: [Warning]
  }
  deriving (Show)

makeLenses ''BuildState

heapPointsTo :: Exp -> (SharingResult, [Warning])
heapPointsTo exp =
  computeResult
  $ flip execState (BuildState 0 mempty mempty mempty mempty)
  $ para buildEquations exp

computeResult :: BuildState -> (SharingResult, [Warning])
computeResult (BuildState maxLoc equations functions nonLinearVars warnings) =
    ( snd $ until (uncurry (==)) (onPair smallStep) (zeroResult, startResult)
    , warnings
    )
  where
    registers = Set.fromList $ flip Maybe.mapMaybe equations $ \case
      VVar   n _ -> Just n
      Access n _ -> Just n
      Fetch  (FetchName n) _ -> Just n
      Fetch  (FetchNode n  _ _) _ -> Just n
      Update n _ -> Just n
      _          -> Nothing
    primitiveFunctions (n, (ps, r)) = Fun n (Dir r) : fmap (\(i, p) -> Param (FParam n i) (Dir p)) ([1..] `zip` ps)
    primitiveFunctionEquations =
      concatMap primitiveFunctions
      $ catMaybes
      $ Map.elems
      $ Map.mapWithKey (\k v -> (,) k <$> primitive k) functions
    zeroResult = SharingResult (HPTResult mempty mempty mempty) mempty
    startResult =
      SharingResult
        (HPTResult
          (V.replicate maxLoc mempty)
          (Map.fromSet (const mempty) registers)
          (Map.map (\n -> (mempty, V.replicate n mempty)) functions)
        )
        mempty
    onPair f (_, b) = (b, f b)
    smallStep res =
      let result1 = List.foldl' oneStepResult res (equations <> primitiveFunctionEquations)
          result2 = oneStepSharing result1
      in result2

    oneStepSharing :: SharingResult -> SharingResult
    oneStepSharing  result@(SharingResult resultHPT sharing) = result
      { _sharedLocs =
          sharing `Set.union`
          joinSets (Set.map locationsForRegister nonLinearVars `Set.union` Set.map locationsForHeap sharing)
      }
      where
        locationsForRegister :: Name -> Set.Set Loc
        locationsForRegister n = maybe mempty (locationsPrimitiveTypeSet . HPT._simpleType) $ Map.lookup n (HPT._register resultHPT)

        locationsForHeap :: Loc -> Set.Set Loc
        locationsForHeap l = maybe mempty locationsNodeSet $ (HPT._memory resultHPT) V.!? l

    oneStepResult :: SharingResult -> Equation -> SharingResult
    oneStepResult result@(SharingResult resultHPT sharing) = \case
      VVar var ts ->
        result & hptResult . HPT.register . at var . _Just %~ mappend (calcTypeSet resultHPT ts)

      Param (FParam n i) ts ->
        result & hptResult . HPT.function . at n . _Just . _2 . at (i - 1) . _Just %~ mappend (calcTypeSet resultHPT ts)

      Fun n ts ->
        result & hptResult . HPT.function . at n . _Just . _1 %~ mappend (calcTypeSet resultHPT ts)

      Access var (ref, tag, i) ->
        let (HPT.TypeSet _ (HPT.NodeSet nodeSet)) = calcTypeSet resultHPT ref -- TODO: Check if simpletypeset is empty
            node = Map.lookup tag nodeSet
            mith  = node >>= (V.!? (i - 1))
            res1 = result & (maybe id (\simpleTypeSet -> hptResult . HPT.register . at var . _Just %~ mappend (HPT.TypeSet simpleTypeSet mempty)) mith)
        in res1

      Store l ns@(Ind (IF fname)) | l `Set.notMember` sharing -> result

      Store l ns ->
        result & hptResult . HPT.memory . at l . _Just %~ mappend (HPT._nodeSet (calcNodeSet resultHPT ns)) -- TODO: check if typeset is not empty

      Fetch val ts0 ->
        let HPT.TypeSet ts1 _ = calcTypeSet resultHPT ts0
            setVar = case val of
              FetchName n     -> hptResult . HPT.register . at n . _Just
              FetchNode n _ _ -> hptResult . HPT.register . at n . _Just
              FetchFunc n     -> hptResult . HPT.function . at n . _Just . _1
            locations = Set.filter (match _T_Location) ts1
            fetch r l = fromMaybe mempty $ r ^. hptResult . HPT.memory . at l
            fetchTypeName r l = HPT.TypeSet mempty (fetch r l)
            fetchTypeNode tag i r l = (HPT.TypeSet (fromMaybe mempty $ Map.lookup tag (HPT._nodeTagMap (fetch r l)) >>= (V.!? (i - 1))) mempty)
            fetchTypeFunc r l = HPT.TypeSet mempty (fetch r l)
            fetchTypeSet = case val of
              FetchName n -> fetchTypeName
              FetchNode _ tag i -> fetchTypeNode tag i
              FetchFunc n -> fetchTypeFunc
        in Set.foldl' (\r1 (T_Location l) -> r1 & setVar %~ mappend (fetchTypeSet r1 l))
                      result
                      locations

      -- TODO: Sharing analysis is required to not to bloat the heap with unrelated nodes.
      -- TODO: Sharing analysis is not enough
      Update n ns0 -> result
{--
        let (HPT.TypeSet ts1 _) = calcTypeSet result (Ind (IN n))
            locations = (Set.map (\(T_Location l) -> l) $ Set.filter (match _T_Location) ts1) `Set.intersection` sharing
            (HPT.TypeSet _ ns1) = calcNodeSet result ns0
        in Set.foldl' (\r1 l -> r1 & HPT.memory . at l . _Just %~ mappend ns1) result locations
--}
    isFunction :: Name -> SharingResult -> Bool
    isFunction n (SharingResult (HPTResult _ _ fs) _) = Map.member n fs

    locationsPrimitiveTypeSet :: Set.Set SimpleType -> Set.Set Loc
    locationsPrimitiveTypeSet = Set.map (\(T_Location l) -> l) . Set.filter (match _T_Location)

    locationsNodeSet :: HPT.NodeSet -> Set.Set Loc
    locationsNodeSet (HPT.NodeSet ns) = Set.unions $ fmap locationsPrimitiveTypeSet $ concatMap V.toList $ Map.elems ns


calcNodeSet :: HPTResult -> Ref Ind NodeSet -> HPT.TypeSet
calcTypeSet :: HPTResult -> Ref Ind TypeSet -> HPT.TypeSet
(calcNodeSet, calcTypeSet) = (valuesOfNodeSet [], valuesOfTypeSet [])
  where
    valuesOfSimpleTypeSet :: [Ref Ind ()] -> HPTResult -> Ref Ind SimpleType -> HPT.TypeSet
    valuesOfSimpleTypeSet ps r p | void p `elem` ps = traceShow ("Circle in simple type set: " ++ show ps) mempty
    valuesOfSimpleTypeSet ps r p = case p of
      Dir t -> HPT.TypeSet (Set.singleton t) mempty
      rest  -> valuesOfTypeSet ps r (const mempty <$> rest)

    valuesOfNodeSet :: [Ref Ind ()] -> HPTResult -> Ref Ind NodeSet -> HPT.TypeSet
    valuesOfNodeSet ps r p | void p `elem` ps = traceShow ("Circle in node set: " ++ show ps) mempty
    valuesOfNodeSet ps r p = case p of
      Dir (NodeSet nodeSet1) ->
        let nodeSet3 = Map.map (fmap (Set.map (valuesOfSimpleTypeSet ps r))) nodeSet1
            nodeSet4 = Map.map (V.fromList . fmap (joinSets . Set.map HPT._simpleType)) nodeSet3
        in HPT.TypeSet mempty (HPT.NodeSet nodeSet4)
      Ind (IN n) -> fromMaybe mempty $ Map.lookup n $ _register r
      Ind (IF n) -> maybe mempty fst $ Map.lookup n $ _function r
      Ind (IFP (FParam n i)) -> fromMaybe mempty $ do -- simpletype should be empty
        (_, params) <- Map.lookup n $ _function r
        pure (params V.! (i - 1))

    valuesOfTypeSet :: [Ref Ind ()] -> HPTResult -> Ref Ind TypeSet -> HPT.TypeSet
    valuesOfTypeSet ps r p | void p `elem` ps = traceShow ("Circle in type set: " ++ show ps) mempty
    valuesOfTypeSet ps r p = case p of
      Dir (TypeSet simpleTypeSet nodeSet1) ->
        let (simpleTypeSetDir, simpleTypeSetInd1) = first (Set.map (\(Dir d) -> d)) $ Set.partition isDir simpleTypeSet
            -- NodeSet2 should be empty
            (HPT.TypeSet simpleTypeSetInd2 _nodeSet) = mconcat $ Set.toList $ Set.map (valuesOfSimpleTypeSet ps r) simpleTypeSetInd1
            nodeSet2 = valuesOfNodeSet ps r (Dir nodeSet1)
        in HPT.TypeSet (simpleTypeSetDir <> simpleTypeSetInd2) mempty <> nodeSet2
      Ind (IN n) -> fromMaybe mempty $ Map.lookup n $ _register r
      Ind (IF n) -> maybe mempty fst $ Map.lookup n $ _function r
      Ind (IFP (FParam n i)) -> fromMaybe mempty $ do
        (_, params) <- Map.lookup n $ _function r
        pure (params V.! (i - 1))

class SetEquation lhs rhs where
  setEq :: lhs -> rhs -> State BuildState ()

infixr 5 ~>
(~>) :: (SetEquation lhs rhs) => lhs -> rhs -> State BuildState ()
n ~> p = setEq n p

(||:) :: Ind -> Tag -> (Int -> (Ind, Tag, Int))
n ||: t = \i -> (n,t,i)

-- | Bind function parameters stored in FNodes as they are also parameters for functions
bindFNodeParams :: Val -> State BuildState ()
bindFNodeParams (ConstTagNode (Tag F name) params) = forM_ ([1..] `zip` params) $ \(i, p) -> case p of
  Var var -> FParam name i ~> var
  rest    -> FParam name i ~> typeSet rest
bindFNodeParams _ = pure ()


deriveFunctionType :: FName -> Exp -> State BuildState ()
deriveFunctionType name = cata folder where

  folder :: ExpF (State BuildState ()) -> State BuildState ()
  folder = \case
    EBindF _ _ rhs -> rhs
    SUpdateF _ _ -> name ~> T_Unit
    SAppF n _    -> name ~> FName n
    SFetchIF n _ -> equations %= (:) (Fetch (FetchFunc (unFName name)) $ Ind (IN n))
    SStoreF _    -> name ~> T_Unit
    SReturnF v -> case v of
      Var w -> name ~> w
      _     -> name ~> typeSet v
    rest         -> sequence_ rest

-- TODO: This is O(n^2) now, on binds for blocks and cases,
-- Move this into the algebra, same for the function type
deriveBlockType :: LPat -> Exp -> State BuildState ()
deriveBlockType pat = cata folder where
  folder :: ExpF (State BuildState ()) -> State BuildState ()
  folder = \case
    EBindF _ _ rhs -> rhs

    ECaseF _  alts -> sequence_ alts

    SUpdateF _ _   ->
      case pat of
        Var pVar -> pVar ~> (Dir @Ind @TypeSet (typeSet T_Unit))
        rest     -> warning $ "Update on invalid pattern: " ++ show rest

    SAppF n _      ->
      case pat of
        Var pVar -> pVar ~> (Ind @Ind @TypeSet (IF n))
        ConstTagNode tag ps -> forM_ ([1..] `zip` ps) $ \case
          (i, Var v) -> equations %= (:) (Access v (Ind (IF n), tag, i))
          _          -> warning $ "Application to literal pattern: " ++ show pat

    SFetchIF n _   ->
      case pat of
        Var pVar -> equations %= (:) (Fetch (FetchName pVar) $ (Ind (IN n)))
        ConstTagNode tag ps -> forM_ ([1..] `zip` ps) $ \case
          (i, Var v) -> equations %= (:) (Fetch (FetchNode v tag i) $ (Ind (IN n)))
          _          -> warning $ "Fetching to literal in pattern: " ++ show pat

    SStoreF _      ->
      case pat of
        Var pVar -> pVar ~> (Dir @Ind @TypeSet (typeSet T_Unit))
        rest     -> warning $ "Store on invalid pattern: " ++ show rest

    SReturnF v -> case (v, pat) of
      (Var w, Var pVar) -> pVar ~> (Ind @Ind @TypeSet (IN w))
      (Var w, ConstTagNode t ps) -> forM_ ([1..] `zip` ps) $ \case
        (i, Var pVar) -> equations %= (:) (Access pVar ((Ind @Ind @TypeSet (IN w), t, i)))
        _             -> warning $ "Return bind to literal in pattern: " ++ show pat
      (node@(ConstTagNode t ps), Var pVar) -> pVar ~> (Dir @Ind @TypeSet (typeSet node))
      (Lit l, Var pVar) -> pVar ~> (Dir @Ind @TypeSet (typeSet (Lit l)))
      (ConstTagNode t0 ps0, ConstTagNode t1 ps1)
        | t0 == t1 && (length ps0) == length ps1 -> forM_ (ps0 `zip` ps1) $ \case
            (Var x, Var y) -> y ~> (Ind @Ind @TypeSet (IN x))
            (Lit x, Var y) -> y ~> (Dir @Ind @TypeSet (typeSet (Lit x)))
            rest           -> warning $ "Return bind to literal in patter: " ++ show (v, pat)
        | otherwise -> warning $ "Return bind to literal in patter: " ++ show (v, pat)
      rest -> error $ show rest

    rest -> sequence_ rest


calcNonLinearVariables :: Exp -> State BuildState ()
calcNonLinearVariables exp = nonLinearVars %= (Set.fromList (Map.keys $ Map.filter (>1) $ cata collect exp) `Set.union`)
  where
    unions = Map.unionsWith (+)
    collect = \case
      ECaseF val alts -> unions (seen val : alts)
      SStoreF val -> seen val
      SFetchIF var _ -> seen (Var var)
      SUpdateF _var _val -> mempty -- We ignore update, as was not part of the code.
      SReturnF val -> seen val
      SAppF _ ps -> unions $ fmap seen ps
      rest -> Data.Foldable.foldr (Map.unionWith (+)) mempty rest

    seen = \case
      Var v -> Map.singleton v 1
      ConstTagNode _ ps -> unions $ fmap seen ps
      VarTagNode v ps -> unions $ fmap seen (Var v : ps)
      _ -> Map.empty


buildEquations :: ExpF (Exp, State BuildState ()) -> State BuildState ()
buildEquations = \case

    ProgramF exts defs -> do
      mapM_ snd defs
      equations %= (Set.toList . Set.fromList)

    DefF name params (body, cbody) -> do
      forM_ ([1..] `zip` params) $ \(i,p) -> p ~> FParam name i
      functions %= Map.insert name (length params)
      cbody
      deriveFunctionType (FName name) body
      calcNonLinearVariables body

    EBindF (SStore val, clhs) (Var storeVar) (rhs, crhs)
      | match _CNode val || match _Var val -> do
          loc <- maxLoc <<%= succ
          case val of
            Var v               -> loc ~> v
            (ConstTagNode (Tag C _) ps)     -> loc ~> val
            (ConstTagNode (Tag F fname) ps) -> do
              loc ~> val
              loc ~> (FName fname)
          storeVar ~> loc
          clhs >> crhs
      | otherwise -> do
          clhs >> crhs
          warning $ "Invalid store value: " ++ show val

    EBindF (SFetchI name Nothing, clhs) pat (rhs, crhs)
      | match _CNode pat || match _Var pat -> do
          clhs >> crhs
          case pat of
            Var var -> equations %= (:) (Fetch (FetchName var) $ Ind (IN name))
            (ConstTagNode tag params) ->
              forM_ ([1..] `zip` params) $ \case
                (i, Var pv) -> equations %= (:) (Fetch (FetchNode pv tag i) (Ind (IN name)))
                rest        -> warning $ "Invalid parameter in fetch:" ++ show rest
            (VarTagNode tagVar params) ->
              warning $ "Variable tag node is not supported in HPT:" ++ show pat
      | otherwise -> do
          clhs >> crhs
          warning $ "Invalid fetch pattern: " ++ show pat

    EBindF (SApp name params, clhs) (Var primRetVar) (rhs, crhs) | Just (paramTypes, retType) <- primitive name -> do
      primRetVar ~> retType
      forM_ (zip3 [1..] params paramTypes) $ \(i,p,t) -> FParam name i ~> t
      clhs >> crhs

    EBindF (SApp name params, clhs) pat (rhs, crhs) -> do
      case pat of
        Var appVar -> appVar ~> FName name
        val@(ConstTagNode tag params) -> do
          forM_ ([1..] `zip` params) $ \(i,p) -> case p of
            Var np -> np ~> ((IF name) ||: tag $ i)
            _ -> warning $ "Invalid pattern for app const tag binding: " ++ show (name, params, pat)
          bindFNodeParams val
        (VarTagNode tagVar params) -> warning $ "Var tag node is not supported yet: " ++ show pat
        _ -> warning $ "Invalid pattern for app: " ++ show (name, params, pat)
      clhs >> crhs

    EBindF (SReturn val, clhs) pat (rhs, crhs) -> do
      case (pat, val) of
        (Var appVar, Var valVar)
          -> appVar ~> valVar
        (Var appVar, val) | match _CNode val || match _Lit val
          -> appVar ~> typeSet val
        (Var appVar, rest)
          -> warning $ "Invalid pattern for return: " ++ show pat

        (ConstTagNode tag params, Var valVar) -> forM_ ([1..] `zip` params) $ \case
          (i, Var pv) -> equations %= (:) (Access pv (Ind (IN valVar), tag, i))
          rest -> warning $ "Invalid pattern for return: " ++ show rest
        (ConstTagNode tag params, val) | match _CNode val || match _Lit val
          -> forM_ ([1..] `zip` params) $ \case
            (i, Var pv) -> equations %= (:) (Access pv (Dir $ typeSet val, tag, i))
            rest -> warning $ "Invalid pattern for return: " ++ show rest
        (ConstTagNode tag params, rest)
          -> warning $ "Invalid pattern for return: " ++ show pat

        (VarTagNode tagVar params, _) -> warning $ "Variable tag node is not supported by HPT" ++ show pat

      clhs >> crhs

    EBindF (SBlock lhs, clhs) pat (rhs, crhs) -> do
      deriveBlockType pat lhs
      clhs >> crhs

    EBindF (lhs@(ECase _ _), clhs) pat (rhs, crhs) -> do
      deriveBlockType pat lhs
      clhs >> crhs

    SStoreF val -> bindFNodeParams val

    SUpdateF var val ->
      case val of
        Var ref -> equations %= (:) (Update var $ Ind (IN ref))
        (ConstTagNode t ps) -> equations  %= (:) (Update var $ Dir $ valToNodeSet val)
        _ -> warning $ "Invalid value for an update: " ++ show val

    SAppF name params -> do
      forM_ (primitive name) $ \(params, ret) ->
        functions %= Map.insert name (length params)

      forM_ ([1 ..] `zip` params) $ \(i, p) -> case p of
        Lit l -> FParam name i ~> typeSet l
        Var n -> FParam name i ~> n
        _ -> warning $ "Invalid parameter function application: " ++ show (name, i, p)

    ECaseF (Var v) alts -> forM_ alts $ \case
      (Alt (NodePat tag ns) _, calt) -> do
        forM_ ([1..] `zip` ns) $ \(i,n) -> n ~> ((IN v) ||: tag $ i)
        calt
      (Alt _ _, calt) -> calt

    ECaseF (ConstTagNode t ps) alts -> forM_ alts $ \case
      (Alt (NodePat tag ns) _, calt) | tag == t -> do
        forM_ (ns `zip` ps) $ \(n,p) -> case p of
          Lit l  -> n ~> typeSet l
          Var vp -> n ~> vp
        calt
      (Alt _ _, calt) -> calt

    rest -> mapM_ snd rest

-- * Helpers

isValueNode :: Tag -> Bool
isValueNode (Tag t _) = t == C

isDir :: Ref i a -> Bool
isDir = \case
  Dir _ -> True
  _     -> False

joinSets :: (Ord a) => Set.Set (Set.Set a) -> Set.Set a
joinSets = Set.unions . Set.toList

instance Semigroup  NodeSet where NodeSet a <> NodeSet b = NodeSet $ Map.unionWith (zipWith Set.union) a b
instance Monoid     NodeSet where mempty = NodeSet mempty

instance Semigroup  TypeSet where TypeSet p1 n1 <> TypeSet p2 n2 = TypeSet (p1 `mappend` p2) (n1 `mappend` n2)
instance Monoid     TypeSet where mempty = TypeSet mempty mempty

instance SetEquation Name   Loc               where setEq n l = setEq n (T_Location l)
instance SetEquation Name   SimpleType        where setEq n t = setEq n (typeSet t)
instance SetEquation Name   TypeSet           where setEq n ts = equations %= (:) (VVar n $ Dir ts)
instance SetEquation Name   (Ref Ind TypeSet) where setEq n ts = equations %= (:) (VVar n ts)
instance SetEquation Name   Name              where setEq n nr = equations %= (:) (VVar n $ Ind (IN nr))
instance SetEquation Name   (Ind, Tag, Int)   where setEq n (tn, t, i) = equations%= (:) (Access n (Ind tn, t, i))
instance SetEquation Name   FParam            where setEq n fp = equations %= (:) (VVar n $ Ind (IFP fp))
instance SetEquation Name   FName             where setEq n (FName fn) = equations %= (:) (VVar n $ Ind (IF fn))
instance SetEquation FName  SimpleType        where setEq (FName fn) t = equations %= (:) (Fun fn (Dir (typeSet t)))
instance SetEquation FName  TypeSet           where setEq (FName fn) ts = equations %= (:) (Fun fn (Dir ts))
instance SetEquation FName  Name              where setEq (FName fn) v = equations %= (:) (Fun fn (Ind (IN v)))
instance SetEquation FName  FName             where setEq (FName fnt) (FName fnf) = equations %= (:) (Fun fnt (Ind (IF fnf)))
instance SetEquation FParam Name              where setEq fp nr = equations %= (:) (Param fp $ Ind (IN nr))
instance SetEquation FParam TypeSet           where setEq fp ts = equations %= (:) (Param fp $ Dir ts)
instance SetEquation Loc    Name              where setEq l n  = equations %= (:) (Store l $ Ind (IN n))
instance SetEquation Loc    Val               where setEq l val = equations %= (:) (Store l $ Dir $ valToNodeSet val)
instance SetEquation Loc    FName             where setEq l (FName fn) = equations %= (:) (Store l $ Ind (IF fn))

warning :: Warning -> State BuildState ()
warning msg = warnings %= (:) msg

class ToTypeSet t where typeSet :: t -> TypeSet

instance ToTypeSet SimpleType where typeSet t = mempty { _simpleTypes = Set.singleton (Dir t) }
instance ToTypeSet Lit where typeSet = typeSet . typeOfLiteral
instance ToTypeSet Val where typeSet = litOrConstTagNodeToTypeSet

typeOfLiteral :: Lit -> SimpleType
typeOfLiteral = \case
  LInt64  _ -> T_Int64
  LWord64 _ -> T_Word64
  LFloat  _ -> T_Float
  LBool   _ -> T_Bool

litOrConstTagNodeToTypeSet :: Val -> TypeSet
litOrConstTagNodeToTypeSet = \case
  c@(ConstTagNode t ps) -> mempty { _nodes = valToNodeSet c }
  VarTagNode _ _ -> mempty
  ValTag _ -> mempty
  Unit  -> mempty
  Lit l -> typeSet l
  Var _ -> mempty

valToNodeSet :: Val -> NodeSet
valToNodeSet = \case
  ConstTagNode t ps -> NodeSet $ Map.singleton t (fmap valToSimpleTypeSet ps)
  _ -> mempty

valToSimpleTypeSet :: Val -> Set.Set (Ref Ind SimpleType)
valToSimpleTypeSet = \case
  Lit l -> Set.singleton $ Dir $ typeOfLiteral l
  Var v -> Set.singleton $ Ind (IN v)
  _     -> Set.empty

-- * Primitive operations

primitive :: Name -> Maybe ([TypeSet], TypeSet)
primitive name = case name of
  "_prim_int_print" -> op [int] unit
  -- Int
  "_prim_int_add"   -> op [int, int] int
  "_prim_int_sub"   -> op [int, int] int
  "_prim_int_mul"   -> op [int, int] int
  "_prim_int_div"   -> op [int, int] int
  "_prim_int_eq"    -> op [int, int] bool
  "_prim_int_ne"    -> op [int, int] bool
  "_prim_int_gt"    -> op [int, int] bool
  "_prim_int_ge"    -> op [int, int] bool
  "_prim_int_lt"    -> op [int, int] bool
  "_prim_int_le"    -> op [int, int] bool
  -- Word
  "_prim_word_add"  -> op [word, word] word
  "_prim_word_sub"  -> op [word, word] word
  "_prim_word_mul"  -> op [word, word] word
  "_prim_word_div"  -> op [word, word] word
  "_prim_word_eq"   -> op [word, word] bool
  "_prim_word_ne"   -> op [word, word] bool
  "_prim_word_gt"   -> op [word, word] bool
  "_prim_word_ge"   -> op [word, word] bool
  "_prim_word_lt"   -> op [word, word] bool
  "_prim_word_le"   -> op [word, word] bool
  -- Float
  "_prim_float_add" -> op [float, float] float
  "_prim_float_sub" -> op [float, float] float
  "_prim_float_mul" -> op [float, float] float
  "_prim_float_div" -> op [float, float] float
  "_prim_float_eq"  -> op [float, float] bool
  "_prim_float_ne"  -> op [float, float] bool
  "_prim_float_gt"  -> op [float, float] bool
  "_prim_float_ge"  -> op [float, float] bool
  "_prim_float_lt"  -> op [float, float] bool
  "_prim_float_le"  -> op [float, float] bool
  -- Bool
  "_prim_bool_eq"   -> op [bool, bool] bool
  "_prim_bool_ne"   -> op [bool, bool] bool
  _ -> Nothing
  where
    int = T_Int64
    bool = T_Bool
    word = T_Word64
    unit = T_Unit
    float = T_Float

    op ::[SimpleType] -> SimpleType -> Maybe ([TypeSet], TypeSet)
    op ps r = Just (typeSet <$> ps, typeSet r)

-- * Pretty

instance (Pretty a) => Pretty (Ref Ind a) where
  pretty = \case
    Ind (IN n)  -> pretty n
    Ind (IF n)  -> pretty n
    Ind (IFP p) -> pretty p
    Dir t       -> pretty t

instance Pretty FName where
  pretty (FName n) = pretty n

instance Pretty FParam where
  pretty (FParam n i) = mconcat [pretty n, text $ "[" ++ show i ++ "]"]

prettyHPTNode1 (tag, args) = pretty tag <> list (fmap pretty args)

instance Pretty NodeSet where
  pretty (NodeSet m) = encloseSep lbrace rbrace comma (prettyHPTNode1 <$> Map.toList m) where

instance Pretty TypeSet where
  pretty (TypeSet ts (NodeSet ns)) = encloseSep lbrace rbrace comma ((prettyHPTNode1 <$> Map.toList ns) ++ fmap pretty (Set.toList ts))

instance Pretty FetchTo where
  pretty = \case
    FetchName n -> pretty n
    FetchNode v t i -> pretty (v, t, i)
    FetchFunc n -> pretty n

instance Pretty Equation where
  pretty = \case
    Store loc ref -> mconcat [pretty loc, text " := ", pretty ref]
    Fetch name val -> mconcat [pretty name, text " := ", text "FETCH ", pretty val]
    Update name val -> mconcat [text "UPDATE ", pretty name, text " ", pretty val]
    VVar name  val -> mconcat [pretty name, text " := ", pretty val]
    Param param val -> mconcat [pretty param, text " := ", pretty val]
    Fun name  val -> mconcat [pretty name, text " := ", pretty val]
    Access name (ts,t,i) -> mconcat [pretty name, text " := ", pretty ts , text "|", pretty t, text "|", text $ show i]


_T_Location :: Traversal' SimpleType Int
_T_Location f (T_Location l) = T_Location <$> f l
_T_Location _ rest           = pure rest

-- * Test

test1 = pretty $ heapPointsTo testExp1
test2 = pretty $ heapPointsTo testExp2

testExp1 :: Exp
testExp1 = [prog|
  grinMain = t1 <- store (CInt 1)      -- t1 := {Loc 1}, 1 := { CInt [{BAS}] }
             t2 <- store (CInt 10000)  -- t2 := {Loc 2}, 2 := { CInt [{BAS}] }
             t3 <- store (Fupto t1 t2) -- t3 := {Loc 3}, m := t1, n := t2, 3 := { FUpto [ t1, t2] }, upto_p1 := t1, upto_p2 := t2
             t4 <- store (Fsum t3)     -- t4 := {Loc 4}, l := t3, sum_p1 := t3
             (CInt r') <- eval t4      -- r' := eval :| CInt 1, eval_p1 := t4
             _prim_int_print r'        -- grinMain := {BAS}, r' := {BAS}

  --fetchTest = t9 <- store (CInt 10000)
  --            fetch t9

  upto m n = (CInt m') <- eval m      -- m' := eval :| CInt 1
             (CInt n') <- eval n      -- n' := eval :| CInt 1
             b' <- _prim_int_gt m' n' -- b' := {BAS}, m' := {BAS}, n' := {BAS}
             if b' then
               pure (CNil) -- upto := {CNil []}
             else
               m1' <- _prim_int_add m' 1 -- m1' := {BAS}
               m1 <- store (CInt m1')    -- m1 := {Loc 5}
               p <- store (Fupto m1 n)   -- p := {Loc 5}, m := m1, upto_p1 := m1, upto_p2 := n
               pure (CCons m p) -- upto := {CCons [m, p]}

  sum l = l2 <- eval l -- l2 := eval, eval_p1 := l
          case l2 of
            (CNil)       -> pure (CInt 0) -- sum := {CInt [{BAS}]}
            (CCons x xs) -> (CInt x') <- eval x -- x' := {BAS}, x := l2 :| CCons 1, xs := l2 :| CCons 2, eval_p1 := x
                            (CInt s') <- sum xs -- s' := sum, sum_p1 := xs
                            ax' <- _prim_int_add x' s' -- ax' := {BAS}
                            pure (CInt ax') -- sum := {CInt ax'}

  eval     q = v <- fetch q -- v := EVAL(FETCH heap q)
               case v of
                 (CInt x'1)    -> pure v -- x'1 := v :| CInt 1, eval := v
                 (CNil)        -> pure v -- eval := v
                 (CCons y ys)  -> pure v -- y := v :| CCons 1, ys := v :| CCons 2, eval := v
                 (Fupto a b)   -> w <- upto a b -- w := upto, a := v :| Fupto 1, b := v :| Fupto 2, upto_p1 := a, upto_p2 := b
                                  update q w -- (loc q) := w
                                  pure w -- eval := w
                 (Fsum c)      -> z <- sum c -- z := sum, sum_p1 := c
                                  update q z -- (loc q) := z
                                  pure z -- eval := z
|]

testExp2 :: Exp
testExp2 = [prog|
grinMain =
  t1 <- store (CInt 1)
  t2 <- store (CInt 10000)
  t3 <- store (Fupto t1 t2)
  t4 <- store (Fsum t3)
  v.0 <- fetch t4
  (CInt r') <- case v.0 of
    (Fsum c.0) ->
      z.0 <- sum c.0
      update t4 z.0
      pure z.0
  _prim_int_print r'

upto m n =
  q.1 <- pure m
  v.1 <- fetch q.1
  (CInt m') <- pure v.1
  v.2 <- fetch n
  (CInt n') <- pure v.2
  b' <- _prim_int_gt m' n'
  case b' of
    #True ->
      pure (CNil)
    #False ->
      m1' <- _prim_int_add m' 1
      m1 <- store (CInt m1')
      p <- store (Fupto m1 n)
      pure (CCons m p)

sum l =
  v.3 <- fetch l
  l2 <- case v.3 of
    (CNil) ->
      pure (CNil)
    (CCons y.3 ys.3) ->
      pure (CCons y.3 ys.3)
    (Fupto a.3 b.3) ->
      w.3 <- upto l b.3
      update l w.3
      pure w.3
  do
    ccp.0 <- case l2 of
      (CNil) ->
        pure 0
      (CCons x xs) ->
        v.4 <- fetch x
        (CInt x') <- pure v.4
        (CInt s') <- sum xs
        ax' <- _prim_int_add x' s'
        pure ax'
    pure (CInt ccp.0)
|]
