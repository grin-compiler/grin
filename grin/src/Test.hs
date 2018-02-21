{-# LANGUAGE DeriveGeneric, LambdaCase, TypeApplications, StandaloneDeriving, RankNTypes #-}
module Test where

import Prelude hiding (GT)

import Control.Applicative
import Control.Monad
import Control.Monad.Extra (loopM)
import Control.Monad.Logic
import Control.Monad.Trans (lift)
import Control.Monad.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State -- .Plus
import Control.Monad.Trans.Reader
import qualified Control.Monad.State.Class as CMS
import qualified Control.Monad.Reader.Class as CMR
import Data.Bifunctor
import Data.Functor.Infix
import Data.Functor.Foldable
import Data.List ((\\))
import Data.Maybe (fromJust)
import Data.Semigroup
import qualified Data.Text as Text
import GHC.Generics
import Grin hiding (Def)
import qualified Grin
import qualified PrimOps
import Test.QuickCheck
import Generic.Random.Generic
import Lens.Micro
import Lens.Micro.Mtl

import Data.Set (Set); import qualified Data.Set as Set
import Data.Map (Map); import qualified Data.Map as Map
import Data.List

import Debug.Trace


data TName = TName { unTName :: String }
  deriving (Eq, Generic, Show)

deriving instance Generic (NonEmptyList a)

data TProg = TProg (NonEmptyList TDef)
  deriving (Generic, Show)

data TDef = TDef TName [TName] TExp
  deriving (Generic, Show)

data TExp
  = TEBind TSExp TLPat TExp
  | TECase TVal (NonEmptyList TAlt)
  | TSExp TSExp
  deriving (Generic, Show)

data TAlt = TAlt CPat TExp
  deriving (Generic, Show)

data TSExp
  = TSApp     TName [TSimpleVal]
  | TSReturn  TVal
  | TSStore   TVal
  | TSFetchI  TName (Maybe Int)
  | TSUpdate  TName TVal
  | TSBlock   TExp
  deriving (Generic, Show)

data TVal
  = TConstTagNode  Tag   [TSimpleVal]
  | TVarTagNode    TName [TSimpleVal]
  | TValTag        Tag
  | TUnit
  | TSimpleVal TSimpleVal
  deriving (Eq, Generic, Show)

data TSimpleVal
  = TLit Lit
  | TVar TName
  deriving (Eq, Generic, Show)

data TLPat
  = TLPatVal  TVal
  | TLPatSVal TSimpleVal
  deriving (Generic, Show)

type Loc = Int

data TExtraVal
  = TLoc Loc
  deriving (Eq, Generic, Show)


toName (TName n) = n

class AsVal t where
  asVal :: t -> Val

instance AsVal TVal where
  asVal = \case
    TConstTagNode  tag  simpleVals -> ConstTagNode tag (asVal <$> simpleVals)
    TVarTagNode    name simpleVals -> VarTagNode (toName name) (asVal <$> simpleVals)
    TValTag        tag             -> ValTag tag
    TUnit                          -> Unit
    TSimpleVal     simpleVal       -> asVal simpleVal

instance AsVal TSimpleVal where
  asVal = \case
    TLit lit  -> Lit lit
    TVar name -> Var (toName name)

instance AsVal TLPat where
  asVal = \case
    TLPatVal  val  -> asVal val
    TLPatSVal sval -> asVal sval

class AsExp t where
  asExp :: t -> Exp

nonWellFormedPrograms :: Gen Exp
nonWellFormedPrograms = resize 1 (asExp <$> arbitrary @TProg)

instance AsExp TProg where
  asExp = \case
    TProg defs -> Program (asExp <$> getNonEmpty defs)

instance AsExp TDef where
  asExp = \case
    TDef name params exp -> Grin.Def (toName name) (toName <$> params) (asExp exp)

instance AsExp TSExp where
  asExp = \case
    TSApp     name simpleVals -> SApp (toName name) (asVal <$> simpleVals)
    TSReturn  val -> SReturn (asVal val)
    TSStore   val -> SStore (asVal val)
    TSFetchI  name pos -> SFetchI (toName name) pos
    TSUpdate  name val -> SUpdate (toName name) (asVal val)
    TSBlock   exp -> SBlock (asExp exp)

instance AsExp TExp where
  asExp = \case
    TEBind sexp lpat exp -> EBind (asExp sexp) (asVal lpat) (asExp exp)
    TECase val alts      -> ECase (asVal val) (asExp <$> getNonEmpty alts)
    TSExp sexp           -> asExp sexp

instance AsExp TAlt where
  asExp = \case
    TAlt cpat exp -> Alt cpat (asExp exp)

downScale :: Gen a -> Gen a
downScale = scale (`div` 2)

instance Arbitrary Text.Text where arbitrary = Text.pack <$> arbitrary

instance Arbitrary TProg where arbitrary = genericArbitraryU
instance Arbitrary TDef where arbitrary = genericArbitraryU
instance Arbitrary TExp where arbitrary = downScale genericArbitraryU
instance Arbitrary TSExp where arbitrary = genericArbitraryU
instance Arbitrary TAlt where arbitrary = genericArbitraryU
instance Arbitrary Val where arbitrary = genericArbitraryU
instance Arbitrary Lit where arbitrary = genericArbitraryU
instance Arbitrary TagType where arbitrary =genericArbitraryU
instance Arbitrary TVal where arbitrary = genericArbitraryU
instance Arbitrary TSimpleVal where arbitrary = genericArbitraryU
instance Arbitrary TExtraVal where arbitrary = genericArbitraryU
instance Arbitrary TLPat where arbitrary = genericArbitraryU

instance Arbitrary CPat where
  arbitrary = oneof
    [ NodePat <$> arbitrary <*> (unTName <$$> listOf1 arbitrary)
    , TagPat  <$> arbitrary
    , LitPat  <$> arbitrary
    ]

instance Arbitrary Tag where
  arbitrary = Tag
    <$> arbitrary
    <*> (unTName <$> arbitrary)

instance Arbitrary TName where
  arbitrary = TName . concat <$> listOf1 hiragana

-- | Increase the size parameter until the generator succeds.
suchThatIncreases :: Gen a -> (a -> Bool) -> Gen a
suchThatIncreases g p =
  resize 1 $ flip loopM () $ \() -> do
    scale (+1)
      $ fmap (maybe (Left ()) Right)
      $ suchThatMaybe g p

hiragana :: Gen String
hiragana = elements $
  ( [ c ++ v
    | v <- ["a",  "e",  "i",  "o",  "u"]
    , c <- ["", "k", "s", "t", "n", "h", "m", "y", "r", "w"]
    ] \\ ["yi", "ye", "wu"]) ++ ["n"]

data Env
  = Env
    { vars :: Map Name Type
    , funs :: Map Name ([Type], Type, [Eff])
    , adts :: Set Type -- The collection of user defined types.
    }
  deriving (Eq, Show)

adtsL :: Lens' Env (Set Type)
adtsL = lens adts (\e a -> e { adts = a})

funsL :: Lens' Env (Map Name ([Type], Type, [Eff]))
funsL = lens funs (\e f -> e { funs = f })

instance Monoid Env where
  mempty = Env mempty mempty mempty
  mappend (Env v0 f0 a0) (Env v1 f1 a1) = Env (Map.unionWith (<>) v0 v1) (f0 <> f1) (a0 <> a1)

insertVar :: Name -> Either TVal TExtraVal -> Env -> Env
insertVar name val (Env vars funs adts) = Env (Map.singleton name (typeOf val) <> vars) funs adts

insertVarT :: Name -> Type -> Env -> Env
insertVarT name ttype (Env vars funs adts) = Env (Map.singleton name ttype <> vars) funs adts

insertVars :: [(Name, Type)] -> Env -> Env
insertVars vars' (Env vars funs adts) = Env ((Map.fromList vars') <> vars) funs adts

insertFun :: (Name, [Type], Type, [Eff]) -> Env -> Env
insertFun (fname, params, rtype, effs) (Env vars funs adts) =
    Env vars funs' adts
  where
    funs' = Map.insert fname (params, rtype, effs) funs

data Store = Store (Map Loc (TVal, Type))
  deriving (Eq, Show)

instance Monoid Store where
  mempty = Store mempty
  mappend (Store s0) (Store s1) = Store (s0 <> s1)

data Eff
  = NoEff Type -- Generate a value returning expression of the given type
  | NewLoc Type -- Store a value of a given type
  | ReadLoc Loc Type -- Read a location with a given type
  | UpdateLoc Loc Type -- Update a location with a given type
  deriving (Eq, Generic, Ord, Show)

instance Arbitrary Eff where arbitrary = genericArbitraryU

data Type
  = TTUnit
  | TInt
  | TFloat
  | TBool
  | TWord
  | TTLoc
  | TTag String [Type] -- Only constant tags, only simple types, or variables with location info
  | TUnion (Set Type)
  deriving (Eq, Generic, Ord, Show)

instance Arbitrary Type where arbitrary = genericArbitraryU


simpleType :: GoalM Type
simpleType = melements
  [ TInt
  , TFloat
  , TWord
  , TTUnit
  , TBool
--  , TTLoc
  ]

primitiveType :: GoalM Type
primitiveType = melements
  [ TInt
  , TFloat
  , TWord
  , TBool
--  , TTUnit
--  , TTLoc
  ]


instance Semigroup Type where
  (TUnion as) <> (TUnion bs) = TUnion (as `Set.union` bs)
  (TUnion as) <> a = TUnion (Set.insert a as)
  a <> (TUnion as) = TUnion (Set.insert a as)
  a <> b = TUnion $ Set.fromList [a,b]

class TypeOf t where
  typeOf :: t -> Type

instance TypeOf TSimpleVal where
  typeOf = \case
    TLit (LInt64 _)  -> TInt
    TLit (LWord64 _) -> TWord
    TLit (LFloat _)  -> TFloat
    TLit (LBool _)   -> TBool
    bad              -> error $ "typeOf @TSimpleVal got:" ++ show bad

instance TypeOf TVal where
  typeOf = \case
    TConstTagNode  tag vals -> TTag (tagName tag) (typeOf <$> vals)
    TValTag        tag      -> TTag (tagName tag) []
    TUnit                   -> TTUnit
    TSimpleVal val          -> typeOf val
    bad -> error $ "typeOf got:" ++ show bad

instance TypeOf TExtraVal where
  typeOf = \case
    TLoc _ -> TTLoc

instance (TypeOf l, TypeOf r) => TypeOf (Either l r) where
  typeOf = either typeOf typeOf

type Context = (Env, Store)

ctxEnv :: Lens' Context Env
ctxEnv = _1

getADTs :: GoalM (Set Type)
getADTs = view (ctxEnv . adtsL)

-- ctxStore = _2

data Goal
  = Exp [Eff] Type
  | SExp Eff
  | GVal Type
  | Prog
  deriving (Eq, Ord, Show)

genProg :: Gen Exp
genProg =
  fmap head $
  asExp <$$>
  (runGoalM $
    withADTs 10 $
    solve @TProg Prog)

sampleGoalM :: Show a => GoalM a -> IO ()
sampleGoalM g = sample $ runGoalM g

type GoalM a = ReaderT Context (LogicT Gen) a

initContext :: Context
initContext = (Env mempty primitives mempty, mempty)
  where
    primitives = Map.map (\(params, ret) -> (convPrimTypes <$> params, convPrimTypes ret, [])) PrimOps.primOps
    convPrimTypes = \case
      PrimOps.TInt   -> TInt
      PrimOps.TWord  -> TWord
      PrimOps.TFloat -> TFloat
      PrimOps.TBool  -> TBool
      PrimOps.TUnit  -> TTUnit

runGoalM :: GoalM a -> Gen [a]
runGoalM = observeManyT 1 . flip runReaderT initContext

runGoalUnsafe :: GoalM a -> Gen a
runGoalUnsafe = fmap checkSolution . runGoalM
  where
    checkSolution [] = error "No solution is found."
    checkSolution xs = head xs

gen :: Gen a -> GoalM a
gen = lift . lift

tagNames :: Type -> [String]
tagNames (TTag name _)  = [name]
tagNames (TUnion types) = concatMap tagNames (Set.toList types)
tagNames _              = []

newName :: GoalM String
newName = do
  (Env vars funs adts) <- view _1
  let names = Map.keys vars <> Map.keys funs <> (concatMap tagNames $ Set.toList adts)
  gen $ ((unTName <$> arbitrary) `suchThatIncreases` (`notElem` names))

newNames :: Int -> GoalM [String]
newNames = go [] where
  go names 0 = pure names
  go names n = do
    name <- newName `mSuchThat` (`notElem` names)
    go (name:names) (n-1)

newVar :: Type -> (String -> GoalM a) -> GoalM a
newVar t k = do
  (Env vars funs adts) <- view _1
  name <- newName
  CMR.local (ctxEnv %~ insertVarT name t) $ do
    k name

withVars :: [(String, Type)] -> GoalM a -> GoalM a
withVars vars = CMR.local (ctxEnv %~ insertVars vars)

type GBool = Type

adt :: GoalM Type
adt = do
  constructors <- newNames =<< gen (choose (1, 5))
  fmap (TUnion . Set.fromList) $ forM constructors $ \name -> do
    fields <- gen $ choose (0, 5)
    TTag name <$> replicateM fields primitiveType

-- | Select a variable from a context which has a given type.
gEnv :: Type -> GoalM Name
gEnv t = do
  (Env vars funs adts) <- view _1
  melements . Map.keys $ Map.filter (==t) vars

gLiteral :: Type -> GoalM TSimpleVal
gLiteral = fmap TLit . \case
  TInt   -> LInt64  <$> gen arbitrary
  TFloat -> LFloat  <$> gen arbitrary
  TWord  -> LWord64 <$> gen arbitrary
  TBool  -> LBool   <$> gen arbitrary
  _      -> mzero

varFromEnv :: Type -> GoalM TSimpleVal
varFromEnv t = (TVar . TName <$> gEnv t)

gSimpleVal :: Type -> GoalM TSimpleVal
gSimpleVal = \case
  TInt   -> varFromEnv TInt `mplus` gLiteral TInt
  TFloat -> varFromEnv TFloat `mplus` gLiteral TFloat
  TWord  -> varFromEnv TWord `mplus` gLiteral TWord
  TBool  -> varFromEnv TBool `mplus` gLiteral TBool
  TTLoc  -> mzero -- TODO: Locations are created via stores of function parameters ... varFromEnv TTLoc
  _      -> mzero

gNodeValue :: Type -> GoalM TVal
gNodeValue = \case
  TTag tag types ->
    (TSimpleVal <$> varFromEnv (TTag tag types)) `mplus`
    (TConstTagNode (Tag C tag) <$> mapM gSimpleVal types)
  _ -> mzero

gValue :: Type -> GoalM TVal
gValue = \case
  TTUnit          -> pure TUnit
  TInt            -> TSimpleVal <$> gSimpleVal TInt
  TFloat          -> TSimpleVal <$> gSimpleVal TFloat
  TWord           -> TSimpleVal <$> gSimpleVal TWord
  TTLoc           -> TSimpleVal <$> gSimpleVal TTLoc
  TBool           -> TSimpleVal <$> gSimpleVal TBool
  TTag tag types  -> gNodeValue $ TTag tag types
  TUnion types    -> do
    t <- melements (Set.toList types)
    solve (GVal t)

gPureFunction :: (Name -> Bool) -> Type -> GoalM (Name, [Type])
gPureFunction p t = do
  (Env vars funs adts) <- view _1
  funs <- gen $ shuffle $ filter (p . fst) $ Map.toList $ Map.filter (\(_, r, eff) -> r == t && eff == []) funs
  (name, (params, ret, [])) <- melements funs
  pure (name, params)

gPureNonPrimFun :: Type -> GoalM (Name, [Type])
gPureNonPrimFun = gPureFunction (not . ("_prim_" `isPrefixOf`))

gPurePrimFun :: Type -> GoalM (Name, [Type])
gPurePrimFun = gPureFunction ("_prim_" `isPrefixOf`)

mGetSize :: GoalM Int
mGetSize = gen $ sized pure

gSExp :: Eff -> GoalM TSExp
gSExp e = do
  s <- mGetSize
  gSExpSized s e

gFunctionCall :: Type -> GoalM TSExp
gFunctionCall t =
  do (funName, paramTypes) <- (gPureNonPrimFun t `mplus` gPurePrimFun t)
     TSApp (TName funName) <$> forM paramTypes gSimpleVal

gSExpSized :: Int -> Eff -> GoalM TSExp
gSExpSized s = \case
  NoEff t ->
    case s of
      0 -> moneof
            [ gFunctionCall t
            , TSReturn <$> solve (GVal t)
            ]
      n -> mfreq
            [ (45, gFunctionCall t)
            , (45, TSReturn <$> solve (GVal t))
            , (10, fmap TSBlock $ solve (Exp [] t))
            ]

  NewLoc t      -> TSStore <$> solve (GVal t) -- TODO: Add a block
  ReadLoc l t   -> mzero -- find a name that contains the location and the given type.
  UpdateLoc l t -> mzero -- fing a name that contains the location and generate  value of a given type

tryout :: [GoalM a] -> GoalM a
tryout gs = do
  (g, gs') <- select gs
  g `mplus` tryout gs'

selectF :: [(Int, a)] -> GoalM ((Int, a), [(Int, a)])
selectF []  = mzero
selectF [a] = pure (a, [])
selectF gs  = do
  let s = sum $ map fst gs
  n <- gen $ choose (0, s)
  pure $ go n gs []
  where
    go n [] _   = error "selectF: impossible"
    go n [a] rs = (a, [])
    go n (a@(m, _):rest) skipped
      | (n - m) <= 0 = (a, skipped ++ rest)-- in range
      | otherwise    = go (n - m) rest (a:skipped)

tryoutF :: [(Int, GoalM a)] -> GoalM a
tryoutF []       = mzero
tryoutF [(_, g)] = g
tryoutF gs = do
  gs0 <- gen $ shuffle gs
  ((_, g), gs1) <- selectF gs0
  g `mplus` tryoutF gs1

moneof :: [GoalM a] -> GoalM a
moneof [] = mzero
moneof gs = join $ fmap fst $ select gs

mfreq :: [(Int, GoalM a)] -> GoalM a
mfreq gs = join $ fmap (snd . fst) $ selectF gs


-- TODO: Limit the number of retries
mSuchThat :: GoalM a -> (a -> Bool) -> GoalM a
mSuchThat g p = go 100 where
  go 0 = mzero
  go n = do
    x <- g
    if (p x) then pure x else go (n - 1)

melements :: [a] -> GoalM a
melements [] = mzero
melements es = gen $ elements es

select :: [a] -> GoalM (a, [a])
select []  = mzero
select [a] = pure (a, [])
select xs  = do
  n <- gen $ choose (0, length xs - 1)
  case (splitAt n xs) of
    ([]    , [])     -> mzero
    ((a:as), [])     -> pure (a, as)
    ([]    , (b:bs)) -> pure (b, bs)
    (as    , (b:bs)) -> pure (b, as ++ bs)

definedAdt :: GoalM Type
definedAdt = do
  (Env funs vars adts) <- view ctxEnv
  melements $ Set.toList adts

retry :: Int -> GoalM a -> GoalM a
retry n _ | n < 0 = mzero
retry 0 g = g
retry n g = g `mplus` retry (n-1) g

liftGenTr :: (forall r . Gen r -> Gen r) -> GoalM a -> GoalM a
liftGenTr fg (ReaderT g) =
  ReaderT $ \ctx ->
    let l' = unLogicT (g ctx)
    in (LogicT (\f g1 -> fg $ l' (\a g0 -> (f a g0)) g1))

mresize :: Int -> GoalM a -> GoalM a
mresize n = liftGenTr (resize n)

mscale :: (Int -> Int) -> GoalM a -> GoalM a
mscale f = liftGenTr (scale f)

-- TODO: Effects
-- TODO: Always succedd with a trivial function
-- TODO: Self Recursive
gDef :: Type -> GoalM (TDef, ([Type], Type, [Eff]))
gDef retType = do
  let effs = []
  n <- gen $ choose (1, 5)
  ptypes <- replicateM n $ mfreq [ (90, simpleType), (10, definedAdt) ]
  (fname:pnames) <- newNames (n+1)
  CMR.local
--    TODO: Self recursive: Generate eval creates in infinite loop
--    kahe ya = kahe ya
--    (ctxEnv %~ (insertFun (fname, ptypes, retType, effs) .
--                insertVars (pnames `zip` ptypes))
    (ctxEnv %~ insertVars (pnames `zip` ptypes)
    ) $ do
        body <- solve (Exp effs retType)
        pure $
          ( TDef (TName fname) (map TName pnames) body
          , (ptypes, retType, effs)
          )

gExp :: Type -> [Eff] -> GoalM TExp
gExp t es = do
  s <- mGetSize
  gExpSized s t es

-- TODO: Generate values for effects
-- TODO: Limit exp generation by values
-- TODO: Use size parameter to limit the generation of programs.
gExpSized :: Int -> Type -> [Eff] -> GoalM TExp
gExpSized n t = \case
  [] -> case n of
    0 -> TSExp <$> (solve (SExp (NoEff t)))
    _ -> tryoutF
            [ -- (10, TSExp <$> (solve (SExp (NoEff t))))
              (80, do t' <- tryout [simpleType, definedAdt]
                      se <- (solve (SExp (NoEff t')))
                      newVar t' $ \n -> do -- TODO: Gen LPat
                        rest <- solve (Exp [] t)
                        pure (TEBind se (TLPatSVal (TVar (TName n))) rest))
            , (20, gCase t)
            ]
  es -> mzero

gCase :: Type -> GoalM TExp
gCase t = tryout
  [ -- Make variable for the case
    do t'   <- tryout [simpleType, definedAdt]
       se   <- gFunctionCall t'
       newVar t' $ \n -> do
         alts <- gAlts Nothing t' t
         pure
           $ TEBind se (TLPatSVal (TVar (TName n)))
           $ TECase (TSimpleVal (TVar (TName n))) $ NonEmpty alts
    -- Try to lookup variable or make a value
  , do t'   <- tryout [simpleType, definedAdt]
       val  <- gValue t'
       alts <- gAlts (Just val) t' t
       pure $ TECase val $ NonEmpty alts
  ]

-- TODO: Effects
-- TODO: Remove overlappings
-- TODO: Mix values and variables in tags
gAlts :: Maybe TVal -> Type -> Type -> GoalM [TAlt]
gAlts val typeOfVal typeOfExp = case typeOfVal of
  TTag name params -> do
    names <- newNames (length params)
    pure . TAlt (NodePat (Tag C name) names)
      <$> withVars (names `zip` params) (solve (Exp [] typeOfExp))
  TUnion types -> fmap concat . forM (Set.toList types) $ \typOfV ->
    gAlts val typOfV typeOfExp
  _ -> case val of
        (Just (TSimpleVal (TLit lit))) -> do
          n <- gen $ choose (1, 5)
          alts0 <- replicateM n $ do
            (TLit lit0) <- gLiteral typeOfVal
            TAlt (LitPat lit0) <$> (solve (Exp [] typeOfExp))
          matching <- TAlt (LitPat lit) <$> (solve (Exp [] typeOfExp))
          let alts = Map.elems $
                     Map.fromList $
                     map (\v@(TAlt pat body) -> (pat, v)) $
                     matching:alts0
          gen $ shuffle alts
        _ -> mzero

gMain :: GoalM TDef
gMain = fmap (TDef (TName "grinMain") []) $ mresize 20 $ solve (Exp [] TTUnit)

-- | Generate n functions and extend the context with the definitions,
-- run the final computation.
gDefs :: [Type] -> ([TDef] -> GoalM a) -> GoalM a
gDefs n f = go n f [] where
  go [] f defs = f defs
  go (t:ts) f defs = do
    (def@(TDef (TName name) _ _), (ptypes, rtype, effs)) <- mresize 20 $ gDef t
    CMR.local (ctxEnv %~ insertFun (name, ptypes, rtype, effs)) $
      -- TODO: Make this as a config parameter
      go ts f (def:defs)

definedAdts :: GoalM (Set Type)
definedAdts = do
  (Env _ _ adts) <- view ctxEnv
  pure adts

gProg :: GoalM TProg
gProg = retry 10 $ do
  n <- gen $ choose (0, 10)
  adts <- Set.toList <$> definedAdts
  ts <- replicateM n $ moneof [simpleType, definedAdt]
  gDefs (adts ++ ts) $ \defs -> do
    m <- gMain
    defs1 <- gen $ shuffle defs
    pure $ TProg $ NonEmpty (defs1 ++ [m])

-- | Generate the given number of ADTs, and register them
-- in the context, running the computation with the new context.
withADTs :: Int -> GoalM a -> GoalM a
withADTs n g = do
  k <- foldM combine id [1 .. n]
  k g
  where
    combine :: (GoalM a -> GoalM a) -> Int -> GoalM (GoalM a -> GoalM a)
    combine k _ = do
      x <- adt
      return (k . (CMR.local ((ctxEnv . adtsL) %~ (Set.union (Set.singleton x)))))

class Solve t where
  solve' :: Goal -> GoalM t

-- TODO: Remove debug...
solve :: Solve t => Goal -> GoalM t
solve g = do
--  (Env vars funs adts) <- view _1
--  traceShowM adts
--  traceShowM (Map.keys funs)
--  s <- gen $ sized pure
--  traceShowM s
--  traceShowM ("Solve", g)
  mscale (\x -> if x > 0 then x - 1 else 0) $ solve' g

instance Solve TVal where
  solve' = \case
    GVal e -> gValue e
    _      -> mzero

instance Solve TSExp where
  solve' = \case
    SExp e -> gSExp e
    _      -> mzero

instance Solve TExp where
  solve' = \case
    Exp es t -> gExp t es
    _        -> mzero

instance Solve TProg where
  solve' = \case
    Prog -> gProg
    _    -> mzero
