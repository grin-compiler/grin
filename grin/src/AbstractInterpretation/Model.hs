{-# LANGUAGE TemplateHaskell, LambdaCase, MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, QuasiQuotes #-}
module AbstractInterpretation.Model where

import Grin.Grin
import Grin.TH
import AbstractInterpretation.HPTResult (SimpleType(..))

import Data.Functor.Foldable
import Lens.Micro.Platform
import Data.Map as Map
-- import Control.Comonad.Trans.Env
import Control.Monad.Identity hiding (fix)
import Debug.Trace
import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import AbstractInterpretation.PrettyHPT


-- The abstract environment contains the varaibles of the GRIN program, plus one variable for each procedure,
-- which denotes the return value of such a call.

{-
TODO
 * Change Equations to a command of a program, as they are just copying information from one register to another.
 * Change the order of computation calls, set the partial computation after the local one.
-}

type Loc = Int

data FParam = FParam Name Int deriving (Show, Eq, Ord)
data Ref a = Ind Name | IndFP FParam | Dir a deriving (Show, Eq, Ord)
type PrimitiveTypeSet = Set.Set (Ref SimpleType)
data TypeSet = TypeSet { _simpleTypes :: PrimitiveTypeSet , _nodes :: NodeSet } deriving (Show, Eq)
newtype NodeSet = NodeSet { _nodeSet :: Map.Map Tag [PrimitiveTypeSet] } deriving (Show, Eq)
data VE = VE Name   (Ref TypeSet)
        | FP FParam (Ref TypeSet)
        | FR Name   (Ref TypeSet)
        | Access Name (Name, Tag, Int)
        | Fetch Name (Ref NodeSet)
        | Update Name (Ref NodeSet)
        deriving (Show, Eq)
data HE = HE Int (Ref NodeSet) deriving (Show, Eq)

data Equations = Equations
  { _env  :: [VE]
  , _heap :: [HE]
  } deriving (Show, Eq)

makeLenses ''Equations
makeLenses ''NodeSet

deriveEquations :: Exp -> Equations
deriveEquations = snd . flip execState (0, mempty) . para buildEquations

testDeriveEquations :: Doc
testDeriveEquations = pretty $ deriveEquations testExp

type BuildState = (Int, Equations)

maxLoc = _1
equations = _2

class SetEquation lhs rhs where
  setEq :: lhs -> rhs -> State BuildState ()

infixr 5 ~>
(~>) :: (SetEquation lhs rhs) => lhs -> rhs -> State BuildState ()
n ~> p = setEq n p

(||:) :: Name -> Tag -> (Int -> (Name, Tag, Int))
n ||: t = \i -> (n,t,i)

fNode :: Val -> State BuildState ()
fNode (ConstTagNode (Tag F name) params) = todo
fNode _ = pure ()

deriveFunctionType :: Name -> ExpF (State BuildState ()) -> State BuildState ()
deriveFunctionType name = \case
  EBindF _ _ rhs -> rhs
  SUpdateF _ _ -> name ~> T_Unit
  SAppF n _    -> name ~> n
  SFetchIF _ _ -> todo
  SStoreF _    -> todo
  SReturnF v -> case v of
    Var w -> name ~> w
    _     -> name ~> typeSet v
  rest         -> sequence_ rest

buildEquations :: ExpF (Exp, State BuildState ()) -> State BuildState ()
buildEquations = \case

    DefF name params (body, cbody) -> do
      forM_ ([1..] `zip` params) $ \(i,p) -> p ~> (FParam name i)
      cbody
      cata (deriveFunctionType name) body

    EBindF (SStore val, clhs) (Var storeVar) (rhs, crhs)
      | match _CNode val || match _Var val -> do
          loc <- maxLoc <<%= succ
          case val of
            Var v               -> loc ~> v
            (ConstTagNode t ps) -> loc ~> val
          storeVar ~> loc
          clhs >> crhs 
      | otherwise -> do
          clhs >> crhs
          warning $ "Invalid store value: " ++ show val

    EBindF (SFetchI name Nothing, clhs) pat (rhs, crhs)
      | match _CNode pat || match _Var pat -> do
          clhs >> crhs
          case pat of
            Var var -> equations . env %= (:) (Fetch var (Ind name))
            (ConstTagNode tag ps) -> todo
      | otherwise -> do
          clhs >> crhs
          warning $ "Invalid fetch pattern: " ++ show pat

    EBindF (SApp name params, clhs) (Var primRetVar) (rhs, crhs) | Just (paramTypes, retType) <- primitive name -> do
      primRetVar ~> retType
      forM_ (zip3 [1..] params paramTypes) $ \(i,p,t) -> FParam name i ~> t
      clhs >> crhs
    
    EBindF (SApp name params, clhs) pat (rhs, crhs) -> do
      case pat of
        Var appVar -> appVar ~> name
        val@(ConstTagNode tag params) -> do
          forM_ ([1..] `zip` params) $ \(i,p) -> case p of
            Var np -> np ~> (name ||: tag $ i)
            _ -> warning $ "Invalid pattern for app const tag binding: " ++ show (name, params, pat)
          fNode val
        _ -> warning $ "Invalid pattern for app: " ++ show (name, params, pat)
      clhs >> crhs

    SStoreF val -> fNode val

    SUpdateF var val ->
      case val of
        Var ref -> equations . env %= (:) (Update var (Ind ref))
        (ConstTagNode t ps) -> equations . env %= (:) (Update var $ Dir $ valToNodeSet val)
        _ -> warning $ "Invalid value for an update: " ++ show val

    SAppF name params ->
      forM_ ([1 ..] `zip` params) $ \(i, p) -> case p of
        Lit l -> FParam name i ~> typeSet l
        Var n -> FParam name i ~> n
        _ -> warning $ "Invalid parameter function application: " ++ show (name, i, p)

    ECaseF (Var v) alts -> forM_ alts $ \case
      (Alt (NodePat tag ns) _, calt) -> do
        forM_ ([1..] `zip` ns) $ \(i,n) -> n ~> (v ||: tag $ i)
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

instance Monoid Equations where
  mempty = Equations mempty mempty
  mappend (Equations e1 h1) (Equations e2 h2) = Equations (e1 `mappend` e2) (h1 `mappend` h2)

instance Monoid NodeSet where
  mempty = NodeSet mempty
  mappend (NodeSet a) (NodeSet b) = NodeSet $ Map.unionWith (zipWith Set.union) a b

instance Monoid TypeSet where
  mempty = TypeSet mempty mempty
  mappend (TypeSet p1 n1) (TypeSet p2 n2) = TypeSet (p1 `mappend` p2) (n1 `mappend` n2)

instance SetEquation Name Loc where setEq n l = setEq n (T_Location l)
instance SetEquation Name SimpleType where setEq n t = setEq n (typeSet t)
instance SetEquation Name TypeSet where setEq n ts = equations . env  %= (:) (VE n (Dir ts))
instance SetEquation Name Name    where setEq n nr = equations . env  %= (:) (VE n (Ind nr))
instance SetEquation Name (Name, Tag, Int) where setEq n nti = equations. env %= (:) (Access n nti)
instance SetEquation Name FParam where setEq n fp = equations . env %= (:) (VE n (IndFP fp))
instance SetEquation FParam Name  where setEq fp nr = equations . env %= (:) (FP fp (Ind nr))
instance SetEquation FParam TypeSet where setEq fp ts = equations . env %= (:) (FP fp (Dir ts))
instance SetEquation Loc  Name    where setEq l n  = equations . heap %= (:) (HE l (Ind n))
instance SetEquation Loc  Val     where setEq l val = equations . heap %= (:) (HE l $ Dir $ valToNodeSet val)

warning :: String -> State BuildState ()
warning _ = pure ()

todo :: State BuildState ()
todo = pure ()

class ToTypeSet t where typeSet :: t -> TypeSet

instance ToTypeSet SimpleType where typeSet t = mempty { _simpleTypes = Set.singleton (Dir t) }
instance ToTypeSet Lit where typeSet = typeSet . typeOfLiteral
instance ToTypeSet Val where typeSet = valToTypeSet

typeOfLiteral :: Lit -> SimpleType
typeOfLiteral = \case
  LInt64  _ -> T_Int64
  LWord64 _ -> T_Word64
  LFloat  _ -> T_Float
  LBool   _ -> T_Bool

valToTypeSet :: Val -> TypeSet
valToTypeSet = \case
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

valToSimpleTypeSet :: Val -> Set.Set (Ref SimpleType)
valToSimpleTypeSet = \case
  Lit l -> Set.singleton $ Dir $ typeOfLiteral l
  Var v -> Set.singleton $ Ind v
  _     -> Set.empty

-- * Primitive operations

primitive :: String -> Maybe ([TypeSet], TypeSet)
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

instance (Pretty a) => Pretty (Ref a) where
  pretty = \case
    Ind   n -> pretty n
    IndFP p -> pretty p
    Dir   t -> pretty t

instance Pretty FParam where
  pretty (FParam n i) = mconcat [pretty n, text $ "[" ++ show i ++ "]"]

prettyHPTNode1 (tag, args) = pretty tag <> list (fmap pretty args)

instance Pretty NodeSet where
  pretty (NodeSet m) = encloseSep lbrace rbrace comma (prettyHPTNode1 <$> Map.toList m) where

instance Pretty TypeSet where
  pretty (TypeSet ts (NodeSet ns)) = encloseSep lbrace rbrace comma ((prettyHPTNode1 <$> Map.toList ns) ++ fmap pretty (Set.toList ts))

instance Pretty HE where
  pretty = \case
    HE loc ref -> mconcat [pretty loc, text " := ", pretty ref]

instance Pretty VE where
  pretty = \case
    VE name  val -> mconcat [pretty name, text " := ", pretty val]
    FP param val -> mconcat [pretty param, text " := ", pretty val]
    FR name  val -> mconcat [pretty name, text " := ", pretty val]
    Access name (nm1,t,i) -> mconcat [pretty name, text " := ", text nm1 , text "|", pretty t, text "|", text $ show i]
    Fetch name val -> mconcat [pretty name, text " := ", text "FETCH ", pretty val]
    Update name val -> mconcat [text "UPDATE ", pretty name, text " ", pretty val]

instance Pretty Equations where
  pretty (Equations env heap) = vsep
    [ text "Environment" <$$> vsep (fmap pretty env)
    , text "Heap" <$$> vsep (fmap pretty heap)
    ]

-- * Test

testExp :: Exp
testExp = [prog|
  grinMain = t1 <- store (CInt 1)      -- t1 := {Loc 1}, 1 := { CInt [{BAS}] }
             t2 <- store (CInt 10000)  -- t2 := {Loc 2}, 2 := { CInt [{BAS}] }
             t3 <- store (Fupto t1 t2) -- t3 := {Loc 3}, m := t1, n := t2, 3 := { FUpto [ t1, t2] }, upto_p1 := t1, upto_p2 := t2
             t4 <- store (Fsum t3)     -- t4 := {Loc 4}, l := t3, sum_p1 := t3
             (CInt r') <- eval t4      -- r' := eval :| CInt 1, eval_p1 := t4
             _prim_int_print r'        -- grinMain := {BAS}, r' := {BAS}

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

  eval q = v <- fetch q -- v := EVAL(FETCH heap q)
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
