{-# LANGUAGE LambdaCase, FlexibleContexts, RecordWildCards #-}
module Transformations.ExtendedSyntax.Util where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Foldable as Foldable

import Lens.Micro.Platform

import Grin.ExtendedSyntax.Grin
import Grin.ExtendedSyntax.Pretty
import Grin.ExtendedSyntax.TypeEnvDefs

{-
  HINT: Name usage in Exp
    - variable def
        names in CPat
        names in LPat
        arg names in Def

    - variable use
        names in Val
        names in FetchI and Update

    - function binder
        function name in Def

    - function reference
        function name in SApp
-}

foldNameUseExpF :: (Monoid m) => (Name -> m) -> ExpF a -> m
foldNameUseExpF f = \case
  ECaseF v _        -> f v
  SAppF fun args    -> f fun <> foldMap f args
  SReturnF val      -> foldNames f val
  SStoreF v         -> f v
  SUpdateF p v      -> f p <> f v
  SFetchF p         -> f p
  _                 -> mempty

data DefRole = FunName | FunParam | BindVar | AltVar
  deriving (Eq, Show)


foldNameDefExpF :: (Monoid m) => (DefRole -> Name -> m) -> ExpF a -> m
foldNameDefExpF f = \case
  DefF name args _      -> mconcat $ (f FunName name) : map (f FunParam) args
  EBindF _ bPat _       -> f BindVar (_bPatVar bPat)
  -- QUESTION: What should be the alt name's DefRole? Now it is BindVar, because it rebinds the scrutinee.
  AltF cpat n _         -> f BindVar n <> foldNames (f AltVar) cpat
  _                     -> mempty

mapNamesCPat :: (Name -> Name) -> CPat -> CPat
mapNamesCPat f = \case
  NodePat tag args  -> NodePat tag (map f args)
  cpat              -> cpat

-- apply a function to all @Name@s in a @Val@
mapNamesVal :: (Name -> Name) -> Val -> Val
mapNamesVal f = \case
  ConstTagNode tag args -> ConstTagNode tag (map f args)
  Var name              -> Var $ f name
  val                   -> val

mapNamesBPat :: (Name -> Name) -> BPat -> BPat
mapNamesBPat f = \case
  VarPat v         -> VarPat (f v)
  AsPat tag vars v -> AsPat tag (map f vars) (f v)

-- TODO: replace at use sites with
-- mapValVal :: (Val -> Val) -> Val -> Val
-- mapValVal f val = case f val of
--   ConstTagNode tag vals -> ConstTagNode tag (map (mapValVal f) vals)
--   VarTagNode name vals  -> VarTagNode name (map (mapValVal f) vals)
--   val                   -> val

mapValsExp :: (Val -> Val) -> Exp -> Exp
mapValsExp f = \case
  -- NOTE: does not recurse into alts
  ECase scrut alts -> ECase scrut alts
  SReturn val      -> SReturn $ f val
  exp              -> exp

mapValsExpM :: Monad m => (Val -> m Val) -> Exp -> m Exp
mapValsExpM f = \case
  SReturn val    -> SReturn <$> f val
  exp            -> pure exp

mapNameUseExp :: (Name -> Name) -> Exp -> Exp
mapNameUseExp f = \case
  SStore    v      -> SStore (f v)
  SFetch  p        -> SFetch (f p)
  SUpdate p v      -> SUpdate (f p) (f v)
  -- NOTE: does not recurse into alts
  ECase scrut alts -> ECase (f scrut) alts
  SApp fun args    -> SApp (f fun) (map f args)
  exp              -> mapValsExp (mapNamesVal f) exp

subst :: Ord a => Map a a -> a -> a
subst env x = Map.findWithDefault x x env

-- substitute all @Names@s in an @Exp@ (non-recursive)
substVarRefExp :: Map Name Name -> Exp -> Exp
substVarRefExp env = mapNameUseExp (subst env)

-- substitute all @Names@s in a @Val@ (non-recursive)
substNamesVal :: Map Name Name -> Val -> Val
substNamesVal env = mapNamesVal (subst env)

-- specialized version of @subst@ to @Val@s (non-recursive)
substValsVal :: Map Val Val -> Val -> Val
substValsVal env = subst env

-- substitute all @Val@s in an @Exp@ (non-recursive)
substVals :: Map Val Val -> Exp -> Exp
substVals env = mapValsExp (subst env)

cPatToVal :: CPat -> Val
cPatToVal = \case
  NodePat tag args  -> ConstTagNode tag args
  LitPat  lit       -> Lit lit
  DefaultPat        -> Unit

cPatToAsPat :: Name -> CPat -> BPat
cPatToAsPat name (NodePat tag args) = AsPat tag args name
cPatToAsPat _ cPat = error $ "cPatToAsPat: cannot convert to as-pattern: " ++ show (PP cPat)

-- monadic recursion schemes
--  see: https://jtobin.io/monadic-recursion-schemes

cataM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t a -> m a) -> t ->  m a
cataM alg = c where
    c = alg <=< traverse c . project

anaM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t a)) -> a -> m t
anaM coalg = a where
  a = (pure . embed) <=< traverse a <=< coalg

paraM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t (t, a) -> m a) -> t -> m a
paraM alg = p where
  p   = alg <=< traverse f . project
  f t = liftM2 (,) (pure t) (p t)

apoM
  :: (Monad m, Traversable (Base t), Corecursive t)
  => (a -> m (Base t (Either t a))) -> a -> m t
apoM coalg = a where
  a = (pure . embed) <=< traverse f <=< coalg
  f = either pure a

hyloM
  :: (Monad m, Traversable t)
  => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h
  where h = alg <=< traverse h <=< coalg

histoM
  :: (Monad m, Traversable (Base t), Recursive t)
  => (Base t (Cofree (Base t) a) -> m a) -> t -> m a
histoM h = pure . extract <=< worker where
  worker = f <=< traverse worker . project
  f x = (:<) <$> h x <*> pure x

-- misc

-- QUESTION: How should this be changed?
skipUnit :: ExpF Exp -> Exp
skipUnit = \case
  -- EBindF (SReturn Unit) _ rightExp -> rightExp
  exp -> embed exp

newtype TagInfo = TagInfo { _tagArityMap :: Map.Map Tag Int }
  deriving (Eq, Show)

updateTagInfo :: Tag -> Int -> TagInfo -> TagInfo
updateTagInfo t n ti@(TagInfo m) =
  case Map.lookup t m of
    Just arity | arity < n -> TagInfo $ Map.insert t n m
    Nothing                -> TagInfo $ Map.insert t n m
    _                      -> ti

collectTagInfo :: Exp -> TagInfo
collectTagInfo = flip execState (TagInfo Map.empty) . cataM alg
  where
    alg :: ExpF () -> State TagInfo ()
    alg = \case
      SReturnF val   -> goVal val
      AltF cpat _ _  -> goCPat cpat
      _              -> pure ()

    goVal :: Val -> State TagInfo ()
    goVal (ConstTagNode t args) = modify $ updateTagInfo t (length args)
    goVal _ = pure ()

    goCPat :: CPat -> State TagInfo ()
    goCPat (NodePat t args) = modify $ updateTagInfo t (length args)
    goCPat _ = pure ()

lookupExcept :: (Monad m, Ord k) =>
                String ->
                k -> Map k v ->
                ExceptT String m v
lookupExcept err k = maybe (throwE err) pure . Map.lookup k

lookupExceptT :: (MonadTrans t, Monad m, Ord k) =>
                 String ->
                 k -> Map k v ->
                 t (ExceptT String m) v
lookupExceptT err k = lift . lookupExcept err k

mapWithDoubleKey :: (Ord k1, Ord k2) =>
                    (k1 -> k2 -> a -> b) ->
                    Map k1 (Map k2 a) ->
                    Map k1 (Map k2 b)
mapWithDoubleKey f = Map.mapWithKey (\k1 m -> Map.mapWithKey (f k1) m)

mapWithDoubleKeyM :: (Ord k1, Ord k2, Monad m) =>
                     (k1 -> k2 -> a -> m b) ->
                     Map k1 (Map k2 a) ->
                     m (Map k1 (Map k2 b))
mapWithDoubleKeyM f = sequence . Map.mapWithKey (\k1 m -> sequence $ Map.mapWithKey (f k1) m)

lookupWithDoubleKey :: (Ord k1, Ord k2) => k1 -> k2 -> Map k1 (Map k2 v) -> Maybe v
lookupWithDoubleKey k1 k2 m = Map.lookup k1 m >>= Map.lookup k2

lookupWithDoubleKeyExcept :: (Monad m, Ord k1, Ord k2) =>
                             String -> k1 -> k2 ->
                             Map k1 (Map k2 v) ->
                             ExceptT String m v
lookupWithDoubleKeyExcept err k1 k2 = maybe (throwE err) pure
                                    . lookupWithDoubleKey k1 k2

lookupWithDoubleKeyExceptT :: (MonadTrans t, Monad m, Ord k1, Ord k2) =>
                              String -> k1 -> k2 ->
                              Map k1 (Map k2 v) ->
                              t (ExceptT String m) v
lookupWithDoubleKeyExceptT err k1 k2 = lift . lookupWithDoubleKeyExcept err k1 k2


notFoundIn :: Show a => String -> a -> String -> String
notFoundIn n1 x n2 = n1 ++ " " ++ show x ++ " not found in " ++ n2

markToRemove :: a -> Bool -> Maybe a
markToRemove x True  = Just x
markToRemove _ False = Nothing

zipFilter :: [a] -> [Bool] -> [a]
zipFilter xs = catMaybes . zipWith markToRemove xs

bindToUndefineds :: Monad m => TypeEnv -> Exp -> [Name] -> ExceptT String m Exp
bindToUndefineds TypeEnv{..} = foldM bindToUndefined where

  bindToUndefined :: Monad m => Exp -> Name -> ExceptT String m Exp
  bindToUndefined rhs v = do
    ty <- lookupExcept (notInTypeEnv v) v _variable
    let ty' = simplifyType ty
    pure $ EBind (SReturn (Undefined ty')) (VarPat v) rhs

  notInTypeEnv v = "Variable " ++ show (PP v) ++ " was not found in the type environment."


simplifySimpleType :: SimpleType -> SimpleType
simplifySimpleType (T_Location _) = T_UnspecifiedLocation
simplifySimpleType t = t

simplifyNodeSet :: NodeSet -> NodeSet
simplifyNodeSet = fmap (fmap simplifySimpleType)

simplifyType :: Type -> Type
simplifyType (T_SimpleType st) = T_SimpleType $ simplifySimpleType st
simplifyType (T_NodeSet    ns) = T_NodeSet $ simplifyNodeSet ns
