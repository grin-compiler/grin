{-# LANGUAGE DeriveGeneric, LambdaCase, TypeApplications, StandaloneDeriving, RankNTypes #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
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
import Data.Maybe (fromJust, maybeToList)
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
import qualified Grammar as G

import Data.Set (Set); import qualified Data.Set as Set
import Data.Map (Map); import qualified Data.Map as Map
import Data.List

import Debug.Trace
import Data.Text (pack)
import Pretty
import GrinTH
import TypeEnv (TypeEnv, emptyTypeEnv)
import Test.Hspec
import Control.Monad
import Data.List


type TestExpContext = (String, (TypeEnv, Exp) -> (TypeEnv, Exp))

testExprContext :: (((TypeEnv, Exp) -> (TypeEnv, Exp)) -> Spec) -> Spec
testExprContext mkSpec = forM_ contexts $ \(label, ctx) -> describe (concat ["(", label, ")"]) $ mkSpec ctx

testExprContextE :: ((Exp -> Exp) -> Spec) -> Spec
testExprContextE mkSpec =
  forM_ contexts $ \(label, ctx) ->
    describe (concat ["(", label, ")"]) $ mkSpec (\e -> snd $ ctx (emptyTypeEnv, e))

stressTest
  :: ((TypeEnv, Exp) -> (TypeEnv, Exp))
  -> (TypeEnv, Exp)
  -> (TypeEnv, Exp)
  -> Spec
stressTest f before after = it "Stress test" $ forAllShrink (listOf1 arbitrary) shrink $ \ctx ->
  let c = createExpr ctx
  in (f (c before)) == (c after)

data ExpContext
  = EmptyCtx
  | LastBindR
  | BindL
  | LastBindL
  | FirstAlt
  | MiddleAlt
  | LastAlt
  deriving (Eq, Show, Generic)

instance Arbitrary ExpContext where arbitrary = genericArbitraryU

createExpr :: [ExpContext] -> (TypeEnv, Exp) -> (TypeEnv, Exp)
createExpr xs te = foldl' combine te (xs `zip` [0..]) where
  combine te (ctx, n) =
    (case ctx of
      EmptyCtx   -> snd emptyCtx
      LastBindR  -> snd lastBindR
      BindL      -> snd $ bindL n
      LastBindL  -> snd $ lastBindL n
      FirstAlt   -> snd firstAlt
      MiddleAlt  -> snd middleAlt
      LastAlt    -> snd lastAlt)
    $ te

contexts :: [TestExpContext]
contexts =
  [ emptyCtx
  , lastBindR
  , bindL 0
  , lastBindL 0
  , firstAlt
  , middleAlt
  , lastAlt
  ]

contexts1 :: [TestExpContext]
contexts1 =
  [ middleBindR
  ]

emptyCtx :: TestExpContext
emptyCtx = ("empty", id)

exprText = pack . show . PP

firstBindR1 :: TestExpContext
firstBindR1 = ("first bind right", second tr) where
  tr (exprText -> e) = [expr|
      $e
      _prim_int_print 1
    |]

changeLast :: Exp -> Exp -> Exp
changeLast e (EBind l p r) = EBind l p (changeLast e r)
changeLast e r@(ECase{})   = EBind (SBlock r) (Var "cl") e
changeLast e r             = EBind r (Var "cl") e

firstBindR :: TestExpContext
firstBindR = ("first bind right", second tr) where
  tr e = changeLast (SReturn (Lit (LInt64 1))) e

middleBindR :: TestExpContext
middleBindR = ("middle bind right", second tr) where
  tr (exprText -> e) = [expr|
      _prim_int_print 42
      $e
      _prim_int_print 1
    |]

lastBindR :: TestExpContext
lastBindR = ("last bind right", second tr) where
  tr (exprText -> e) = [expr|
      _prim_int_print 42
      $e
    |]

bindL :: Int -> TestExpContext
bindL (pack . show -> n) = ("bind left", second tr) where
  tr (exprText -> e) = [expr|
      fb$n <- do
        $e
      _prim_int_print 1
    |]

lastBindL :: Int -> TestExpContext
lastBindL (pack . show -> n) = ("last bind left", second tr) where
  tr (exprText -> e) = [expr|
      md$n <- do
        _prim_int_print 42
        $e
      _prim_int_print 1
    |]

firstAlt :: TestExpContext
firstAlt = ("first alt", second tr) where
  tr (exprText -> e) = [expr|
      case 1 of
        1 -> _prim_int_print 42
             $e
        2 -> _prim_int_print 1
        3 -> _prim_int_print 1
    |]

middleAlt :: TestExpContext
middleAlt = ("middle alt", second tr) where
  tr (exprText -> e) = [expr|
      case 1 of
        1 -> _prim_int_print 1
        2 -> _prim_int_print 1
             $e
        3 -> _prim_int_print 1
    |]

lastAlt :: TestExpContext
lastAlt = ("last alt", second tr) where
  tr (exprText -> e) = [expr|
      case 1 of
        1 -> _prim_int_print 1
        2 -> _prim_int_print 1
        3 -> _prim_int_print 1
             $e
    |]



programGenerators :: [(String, Gen Exp)]
programGenerators =
  [ ("Semantically incorrect programs", semanticallyIncorrectPrograms)
  , ("Semantically correct programs", genProg)
  ]

semanticallyIncorrectPrograms :: Gen Exp
semanticallyIncorrectPrograms = resize 1 (G.asExp <$> arbitrary @G.Prog)


downScale :: Gen a -> Gen a
downScale = scale (`div` 2)

instance Arbitrary Text.Text where arbitrary = Text.pack <$> arbitrary

instance Arbitrary G.Prog where arbitrary = genericArbitraryU
instance Arbitrary G.Def where arbitrary = genericArbitraryU
instance Arbitrary G.Exp where arbitrary = downScale genericArbitraryU
instance Arbitrary G.Alt where arbitrary = genericArbitraryU
instance Arbitrary Val where arbitrary = genericArbitraryU
instance Arbitrary Lit where arbitrary = genericArbitraryU
instance Arbitrary TagType where arbitrary =genericArbitraryU
instance Arbitrary G.Val where arbitrary = genericArbitraryU
instance Arbitrary G.SimpleVal where arbitrary = genericArbitraryU
instance Arbitrary G.ExtraVal where arbitrary = genericArbitraryU
instance Arbitrary G.LPat where arbitrary = genericArbitraryU

instance Arbitrary G.SExp where
  arbitrary = genericArbitraryU `suchThat` validStoreOp
    where
      isNode = \case
        G.ConstTagNode  _ _ -> True
        G.VarTagNode    _ _ -> True
        _                   -> False

      validStoreOp = \case
        G.SStore  val   -> isNode val
        G.SUpdate _ val -> isNode val
        _               -> True

instance Arbitrary CPat where
  arbitrary = oneof
    [ NodePat <$> arbitrary <*> (G.unName <$$> listOf1 arbitrary)
    , TagPat  <$> arbitrary
    , LitPat  <$> arbitrary
    , pure DefaultPat
    ]

instance Arbitrary Tag where
  arbitrary = Tag
    <$> arbitrary
    <*> (G.unName <$> arbitrary)

instance Arbitrary G.Name where
  arbitrary = G.Name . concat <$> listOf1 hiragana

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

insertVar :: Name -> Either G.Val G.ExtraVal -> Env -> Env
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

data Store = Store (Map G.Loc (G.Val, Type))
  deriving (Eq, Show)

instance Monoid Store where
  mempty = Store mempty
  mappend (Store s0) (Store s1) = Store (s0 <> s1)

data Eff
  = NoEff          -- Generate a value returning expression of the given type
  | NewLoc    Type -- Store a value of a given type
  | ReadLoc   Type -- Read a location with a given type
  | UpdateLoc Type -- Update a location with a given type
  deriving (Eq, Generic, Ord, Show)

getSExpTypeInEff :: Eff -> Maybe Type
getSExpTypeInEff = \case
  NoEff       -> mzero
  NewLoc    t -> pure (TLoc t)
  ReadLoc   t -> pure t
  UpdateLoc t -> pure TUnit

instance Arbitrary Eff where arbitrary = genericArbitraryU

data Type
  = TUnit -- TODO: Rename
  | TInt
  | TFloat
  | TBool
  | TWord
  | TLoc Type
  | TTag String [Type] -- Only constant tags, only simple types, or variables with location info
  | TUnion (Set Type)
  deriving (Eq, Generic, Ord, Show)

instance Arbitrary Type where arbitrary = genericArbitraryU


simpleType :: GoalM Type
simpleType = melements
  [ TInt
  , TFloat
  , TWord
  , TUnit
  , TBool
--  , TLoc
  ]

primitiveType :: GoalM Type
primitiveType = melements
  [ TInt
  , TFloat
  , TWord
  , TBool
--  , TUnit
--  , TLoc
  ]


instance Semigroup Type where
  (TUnion as) <> (TUnion bs) = TUnion (as `Set.union` bs)
  (TUnion as) <> a = TUnion (Set.insert a as)
  a <> (TUnion as) = TUnion (Set.insert a as)
  a <> b = TUnion $ Set.fromList [a,b]

class TypeOf t where
  typeOf :: t -> Type

instance TypeOf G.SimpleVal where
  typeOf = \case
    G.Lit (LInt64 _)  -> TInt
    G.Lit (LWord64 _) -> TWord
    G.Lit (LFloat _)  -> TFloat
    G.Lit (LBool _)   -> TBool
    bad              -> error $ "typeOf @G.SimpleVal got:" ++ show bad

instance TypeOf G.Val where
  typeOf = \case
    G.ConstTagNode  tag vals -> TTag (tagName tag) (typeOf <$> vals)
    G.ValTag        tag      -> TTag (tagName tag) []
    G.Unit                   -> TUnit
    G.SimpleVal val          -> typeOf val
    bad -> error $ "typeOf got:" ++ show bad

instance TypeOf G.ExtraVal where
  typeOf = \case
    G.Loc _ -> TLoc TInt -- TODO: More types...

instance (TypeOf l, TypeOf r) => TypeOf (Either l r) where
  typeOf = either typeOf typeOf

data Context = Context
  { _ctxEnv    :: Env
  , _ctxStore  :: Store
  , _ctxExpGen :: GoalM G.Exp
  }

ctxEnv :: Lens' Context Env
ctxEnv = lens _ctxEnv (\c e -> c { _ctxEnv = e })

ctxExpGen :: Lens' Context (GoalM G.Exp)
ctxExpGen = lens _ctxExpGen (\c e -> c { _ctxExpGen = e })

getADTs :: GoalM (Set Type)
getADTs = view (ctxEnv . adtsL)

-- ctxStore = _2

data Goal
  = Exp [Eff] Type
  | SExp Eff Type
  | GVal Type
  | Prog
  deriving (Eq, Ord, Show)

genProg :: Gen Exp
genProg = genProgWith mzero

sampleProg :: IO ()
sampleProg = sample $ fmap PP $ genProg

genProgWith :: GoalM G.Exp -> Gen Exp
genProgWith gexp =
  fmap head $
  G.asExp <$$>
  (runGoalM gexp $
    withADTs 10 $
    solve @G.Prog Prog)

sampleGoalM :: Show a => GoalM a -> IO ()
sampleGoalM g = sample $ runGoalM mzero g

type GoalM a = ReaderT Context (LogicT Gen) a

initContext :: GoalM G.Exp -> Context
initContext expGen = Context (Env mempty primitives mempty) mempty expGen
  where
    primitives = Map.map (\(params, ret) -> (convPrimTypes <$> params, convPrimTypes ret, [])) PrimOps.primOps
    convPrimTypes = \case
      PrimOps.TInt   -> TInt
      PrimOps.TWord  -> TWord
      PrimOps.TFloat -> TFloat
      PrimOps.TBool  -> TBool
      PrimOps.TUnit  -> TUnit

runGoalM :: GoalM G.Exp -> GoalM a -> Gen [a]
runGoalM expGen = observeManyT 1 . flip runReaderT (initContext expGen)

runGoalUnsafe :: GoalM a -> Gen a
runGoalUnsafe = fmap checkSolution . runGoalM mzero
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
  (Env vars funs adts) <- view ctxEnv
  let names = Map.keys vars <> Map.keys funs <> (concatMap tagNames $ Set.toList adts)
  gen $ ((G.unName <$> arbitrary) `suchThatIncreases` (`notElem` names))

newNames :: Int -> GoalM [String]
newNames = go [] where
  go names 0 = pure names
  go names n = do
    name <- newName `mSuchThat` (`notElem` names)
    go (name:names) (n-1)

newVar :: Type -> (String -> GoalM a) -> GoalM a
newVar t k = do
  (Env vars funs adts) <- view ctxEnv
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
  (Env vars funs adts) <- view ctxEnv
  melements . Map.keys $ Map.filter (==t) vars

gLiteral :: Type -> GoalM G.SimpleVal
gLiteral = fmap G.Lit . \case
  TInt   -> LInt64  <$> gen arbitrary
  TFloat -> LFloat  <$> gen arbitrary
  TWord  -> LWord64 <$> gen arbitrary
  TBool  -> LBool   <$> gen arbitrary
  _      -> mzero

varFromEnv :: Type -> GoalM G.SimpleVal
varFromEnv t = (G.Var . G.Name <$> gEnv t)

gSimpleVal :: Type -> GoalM G.SimpleVal
gSimpleVal = \case
  TInt   -> varFromEnv TInt `mplus` gLiteral TInt
  TFloat -> varFromEnv TFloat `mplus` gLiteral TFloat
  TWord  -> varFromEnv TWord `mplus` gLiteral TWord
  TBool  -> varFromEnv TBool `mplus` gLiteral TBool
  (TLoc t) -> varFromEnv (TLoc t) -- Locations have no literals
  _      -> mzero

gNodeValue :: Type -> GoalM G.Val
gNodeValue = \case
  TTag tag types ->
    (G.SimpleVal <$> varFromEnv (TTag tag types)) `mplus`
    (G.ConstTagNode (Tag C tag) <$> mapM gSimpleVal types)
  _ -> mzero

gValue :: Type -> GoalM G.Val
gValue = \case
  TUnit          -> pure G.Unit
  TInt            -> G.SimpleVal <$> gSimpleVal TInt
  TFloat          -> G.SimpleVal <$> gSimpleVal TFloat
  TWord           -> G.SimpleVal <$> gSimpleVal TWord
  TLoc t        -> G.SimpleVal <$> gSimpleVal (TLoc t)
  TBool           -> G.SimpleVal <$> gSimpleVal TBool
  TTag tag types  -> gNodeValue $ TTag tag types
  TUnion types    -> do
    t <- melements (Set.toList types)
    solve (GVal t)

gPureFunction :: (Name -> Bool) -> Type -> GoalM (Name, [Type])
gPureFunction p t = do
  (Env vars funs adts) <- view ctxEnv
  funs <- gen $ shuffle $ filter (p . fst) $ Map.toList $ Map.filter (\(_, r, eff) -> r == t && eff == []) funs
  (name, (params, ret, [])) <- melements funs
  pure (name, params)

gPureNonPrimFun :: Type -> GoalM (Name, [Type])
gPureNonPrimFun = gPureFunction (not . ("_prim_" `isPrefixOf`))

gPurePrimFun :: Type -> GoalM (Name, [Type])
gPurePrimFun = gPureFunction ("_prim_" `isPrefixOf`)

mGetSize :: GoalM Int
mGetSize = gen $ sized pure

gSExp :: Eff -> Type -> GoalM G.SExp
gSExp e t = do
  s <- mGetSize
  gSExpSized s t e

gFunctionCall :: Type -> GoalM G.SExp
gFunctionCall t =
  do (funName, paramTypes) <- (gPureNonPrimFun t `mplus` gPurePrimFun t)
     G.SApp (G.Name funName) <$> forM paramTypes gSimpleVal

gSExpSized :: Int -> Type -> Eff -> GoalM G.SExp
gSExpSized s t = \case
  NoEff ->
    case s of
      0 -> moneof
            [ gFunctionCall t
            , G.SReturn <$> solve (GVal t)
            ]
      n -> mfreq
            [ (45, gFunctionCall t)
            , (45, G.SReturn <$> solve (GVal t))
            , (10, fmap G.SBlock $ solve (Exp [] t))
            ]

  NewLoc t' -> case t of
    TLoc t0 -> G.SStore <$> solve (GVal t0) -- TODO: Add a block
    _        -> mzero

  -- find a name that contains the location and the given type.
  ReadLoc t' -> do
    (G.Var name) <- varFromEnv (TLoc t')
    pure $ G.SFetchI name Nothing

  UpdateLoc t' -> do
    (G.Var name) <- varFromEnv (TLoc t')
    val <- solve (GVal t')
    pure $ G.SUpdate name val -- fing a name that contains the location and generate  value of a given type

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

gEffs :: GoalM [Eff]
gEffs = do
  adts <- Set.toList <$> definedAdts
  moneof
    [ pure []
    , gen $ do
        n <- choose (0, 3)
        let noeffs = replicate n NoEff
        adt <- elements adts
        rest <- listOf1 $ elements [NoEff, NewLoc adt, ReadLoc adt, UpdateLoc adt]
        pure $ noeffs ++ [NewLoc adt] ++ rest
    ]

-- TODO: Effects
-- TODO: Always succedd with a trivial function
-- TODO: Self Recursive
gDef :: Type -> GoalM (G.Def, ([Type], Type, [Eff]))
gDef retType = do
  effs <- gEffs
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
          ( G.Def (G.Name fname) (map G.Name pnames) body
          , (ptypes, retType, effs)
          )

gExp :: Type -> [Eff] -> GoalM G.Exp
gExp t es = do
  s <- mGetSize
  gExpSized s t es

-- TODO: Generate values for effects
-- TODO: Limit exp generation by values
-- TODO: Use size parameter to limit the generation of programs.
gExpSized :: Int -> Type -> [Eff] -> GoalM G.Exp
gExpSized n t = \case
  [] -> case n of
    0 -> G.SExp <$> (solve (SExp NoEff t))
    _ -> tryoutF
          [ -- (10, G.SExp <$> (solve (SExp (NoEff t))))
            (80, do (t', se) <-
                      tryout
                        [ do t' <- tryout [simpleType, definedAdt]
                             se <- (solve (SExp NoEff t'))
                             pure (t', se)
                        , do se <- fmap G.SBlock $ join $ view ctxExpGen
                             pure (TUnit, se)
                        ]
                    newVar t' $ \n -> do -- TODO: Gen LPat
                      rest <- solve (Exp [] t)
                      pure (G.EBind se (G.LPatSVal (G.Var (G.Name n))) rest))
          , (20, gCase [] t)
          ]
  (e:es) -> case n of
    0 -> G.SExp <$> (solve (SExp NoEff t)) -- TODO: Consume all effects
    _ -> tryoutF
            [ -- (10, G.SExp <$> (solve (SExp (NoEff t))))
              (80, do t' <- maybe (tryout [simpleType, definedAdt]) pure
                            $ getSExpTypeInEff e
                      se <- (solve (SExp e t'))
                      newVar t' $ \n -> do -- TODO: Gen LPat
                        rest <- solve (Exp es t)
                        pure (G.EBind se (G.LPatSVal (G.Var (G.Name n))) rest))
            , (20, gCase (e:es) t)
            ]

gCase :: [Eff] -> Type -> GoalM G.Exp
gCase eff t = tryout
  [ -- Make variable for the case
    do t'   <- tryout [simpleType, definedAdt]
       se   <- gFunctionCall t'
       newVar t' $ \n -> do
         alts <- gAlts eff Nothing t' t
         pure
           $ G.EBind se (G.LPatSVal (G.Var (G.Name n)))
           $ G.ECase (G.SimpleVal (G.Var (G.Name n))) $ NonEmpty alts
    -- Try to lookup variable or make a value
  , do t'   <- tryout [simpleType, definedAdt]
       val  <- gValue t'
       alts <- gAlts eff (Just val) t' t
       mDefAlt <- moneof [pure Nothing, (Just . G.Alt DefaultPat <$> (solve (Exp eff t)))]
       pure $ G.ECase val $ NonEmpty (alts ++ maybeToList mDefAlt)
  ]

-- TODO: Effects
-- TODO: Remove overlappings
-- TODO: Mix values and variables in tags
gAlts :: [Eff] -> Maybe G.Val -> Type -> Type -> GoalM [G.Alt]
gAlts eff val typeOfVal typeOfExp = case typeOfVal of
  TTag name params -> do
    names <- newNames (length params)
    pure . G.Alt (NodePat (Tag C name) names)
      <$> withVars (names `zip` params) (solve (Exp eff typeOfExp))
  TUnion types -> fmap concat . forM (Set.toList types) $ \typOfV ->
    gAlts eff val typOfV typeOfExp
  _ -> case val of
        (Just (G.SimpleVal (G.Lit lit))) -> do
          n <- gen $ choose (0, 5)
          alts0 <- replicateM n $ do
            (G.Lit lit0) <- gLiteral typeOfVal
            G.Alt (LitPat lit0) <$> (solve (Exp eff typeOfExp))
          matching <- G.Alt (LitPat lit) <$> (solve (Exp eff typeOfExp))
          let alts = Map.elems $
                     Map.fromList $
                     map (\v@(G.Alt pat body) -> (pat, v)) $
                     matching:alts0
          gen $ shuffle alts
        _ -> mzero

gMain :: GoalM G.Def
gMain =
  fmap (G.Def (G.Name "grinMain") [])
  $ mresize 20
  $ do effs <- gEffs
       solve (Exp effs TUnit)

-- | Generate n functions and extend the context with the definitions,
-- run the final computation.
gDefs :: [Type] -> ([G.Def] -> GoalM a) -> GoalM a
gDefs n f = go n f [] where
  go [] f defs = f defs
  go (t:ts) f defs = do
    (def@(G.Def (G.Name name) _ _), (ptypes, rtype, effs)) <- mresize 20 $ gDef t
    CMR.local (ctxEnv %~ insertFun (name, ptypes, rtype, effs)) $
      -- TODO: Make this as a config parameter
      go ts f (def:defs)

definedAdts :: GoalM (Set Type)
definedAdts = do
  (Env _ _ adts) <- view ctxEnv
  pure adts

gProg :: GoalM G.Prog
gProg = retry 10 $ do
  n <- gen $ choose (0, 10)
  adts <- Set.toList <$> definedAdts
  ts <- replicateM n $ moneof [simpleType, definedAdt]
  gDefs (ts ++ adts) $ \defs -> do
    m <- gMain
    defs1 <- gen $ shuffle defs
    pure $ G.Prog $ NonEmpty (defs1 ++ [m])

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

instance Solve G.Val where
  solve' = \case
    GVal e -> gValue e
    _      -> mzero

instance Solve G.SExp where
  solve' = \case
    SExp e t -> gSExp e t
    _        -> mzero

instance Solve G.Exp where
  solve' = \case
    Exp es t -> gExp t es
    _        -> mzero

instance Solve G.Prog where
  solve' = \case
    Prog -> gProg
    _    -> mzero

changed :: (Testable prop) => Exp -> Exp -> prop -> Property
changed old new = cover (old /= new) 1 "Transformation has effect"
