-- Based on the https://brianmckenna.org/blog/type_annotation_cofree
--
-- This module is an example how one can generate constraints based
-- on the GRIN AST. Solve the constraints and annotate the nodes
-- with some information.
--
-- Thus this modules serves as a fundation for quick prototyping and
-- a tool which enables to create simple test oracle for DataFlow IR
-- based abstract interpretation.

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
module Grin.Research where

import Data.Functor.Foldable
import Grin.Syntax
import Lens.Micro.Platform
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Control.Comonad.Cofree
import qualified Control.Comonad.Trans.Cofree as CCTC
import qualified Data.Text as Text
import Grin.TH (prog)
import Grin.PrettyLint (prettyAnnExp)
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty ((<$>))
import Grin.Pretty (showWide)
import Control.Arrow ((&&&))
import Data.Maybe (fromJust)
import Data.Functor.Infix ((<$$$>))



-- * Data Types

data Info i
  = INone
  | IVar Int
  | IInfo i -- This will be the node related info
  deriving (Functor, Show)

data Constraint i = EqualityConstraint (Info i) (Info i)
  deriving (Functor, Show)

data Result t = Result
  { _constraints :: !([Constraint t])
  , _assumptions :: !(Map.Map String [Info t]) -- String <=> Var NM
  } deriving (Functor, Show)

instance Semigroup (Result t) where
  (Result c1 a1) <> (Result c2 a2) = Result (c1 <> c2) (Map.unionWith (<>) a1 a2)

instance Monoid (Result t) where
  mempty  = Result mempty mempty
  mappend = (<>)

-- * Generate Constraints

type NodeId = Int
data CState i = CState
  { _varId    :: !Int
  , _nodeInfo :: !(Map.Map NodeId [Info i])
  , _result   :: !(Result i)
  }

makeLenses ''CState
makeLenses ''Result

type ConstraintM t = State (CState t) (Info t)

mkVariable :: Text.Text -> Info t -> Result t
mkVariable n t = Result mempty (Map.singleton (Text.unpack n) [t])

mkConstraint :: Info t -> Info t -> Result t
mkConstraint t1 t2 = Result [EqualityConstraint t1 t2] mempty

newtype NodeIdM = NodeIdM { _nodeId :: Int }
makeLenses ''NodeIdM

annotate :: Exp -> Cofree ExpF NodeId
annotate = flip evalState (NodeIdM 0) . sequence . cata ((nodeId <<%= succ) :<)

variable n t   = result %= (<> (mkVariable n t))
equality t1 t2 = result %= (<> (mkConstraint t1 t2))
nodeType n t   = nodeInfo %= (Map.unionWith (++) $ Map.singleton n [t])
freshVarId     = fmap IVar $ varId <<%= succ

generateConstraints
  :: (CCTC.CofreeF ExpF NodeId (ConstraintM t) -> ConstraintM t)
  -> Exp
  -> (Cofree ExpF (Maybe [Info t]), Result t)
generateConstraints algebra exp = (fmap (flip Map.lookup nodeInfo) cexp, result) where
  cexp = annotate exp
  (CState _ nodeInfo result) = execState (cata algebra cexp) (CState 0 mempty mempty)

-- * Pretty

instance Pretty t => Pretty (Constraint t) where
  pretty (EqualityConstraint t1 t2) = hsep [pretty t1, ":=", pretty t2]

instance Pretty t => Pretty (Info t) where
  pretty = \case
    IVar  v -> string $ "v" ++ show v
    IInfo t -> pretty t
    INone   -> string $ "-"

instance Pretty t => Pretty (Result t) where
  pretty (Result cr as) = hsep (map pretty cr) <+> hsep (map prettyMap $ Map.toList as)
    where
      prettyMap :: Pretty t => (String, [Info t]) -> Doc
      prettyMap (k,v) = hsep [string k, ":=", list (map pretty v)]

-- * Example of simple type unification

calcSimpleTypesAlg :: CCTC.CofreeF ExpF NodeId (ConstraintM SimpleType) -> ConstraintM SimpleType
calcSimpleTypesAlg = \case
  (_ CCTC.:< ProgramF externals defs) -> do
    forM externals $ \(External{..}) ->
      forOf_ (_TySimple . to IInfo) eRetType (variable $ unNM eName)
    sequence_ defs
    pure INone

  (node CCTC.:< DefF n ps body) -> do
    t <- body
    nodeType node t
    v <- freshVarId
    variable (unNM n) v
    equality v t
    pure t

  (node CCTC.:< EBindF lhs (Var (NM n)) rhs) -> do
    lt <- lhs
    t  <- freshVarId
    equality t lt
    variable n t
    rt <- rhs
    nodeType node rt
    pure rt

  (_ CCTC.:< SAppF n ps) -> do
    v <- freshVarId
    variable (unNM n) v
    pure v

  (_ CCTC.:< SReturnF v) ->
    case v of
      (Lit (LInt64  _)) -> pure $ IInfo T_Int64
      (Lit (LWord64 _)) -> pure $ IInfo T_Word64
      (Lit (LFloat  _)) -> pure $ IInfo T_Float
      (Lit (LBool   _)) -> pure $ IInfo T_Bool
      (Lit (LString _)) -> pure $ IInfo T_String
      (Lit (LChar   _)) -> pure $ IInfo T_Char
      Unit              -> pure $ IInfo T_Unit
      (Var v)           -> do
        t <- preuse $ result . assumptions . at (Text.unpack $ unNM v) . _Just . ix 0
        pure $ fromJust t

  (_ CCTC.:< rest) -> do
    sequence_ rest
    pure INone

type Type = Info SimpleType
solveConstraints :: [Constraint SimpleType] -> Maybe (Map.Map Int Type)
solveConstraints = foldl (\b a -> liftM2 mappend (solve b a) b) $ Just Map.empty where
  solve maybeSubs (EqualityConstraint a b) = do
    subs <- maybeSubs
    mostGeneralUnifier (substiute subs a) (substiute subs b)

mostGeneralUnifier :: Type -> Type -> Maybe (Map.Map Int Type)
mostGeneralUnifier (IVar i) a = Just $ Map.singleton i a
mostGeneralUnifier a (IVar i) = Just $ Map.singleton i a
mostGeneralUnifier (IInfo t1) (IInfo t2) | t1 == t2 = Just $ Map.empty
mostGeneralUnifier _ _ = Nothing

substiute :: Map.Map Int Type -> Type -> Type
substiute subs v@(IVar i) = maybe v (substiute subs) $ Map.lookup i subs
substiute _ t = t

calcSimpleTypes :: Exp -> Maybe (Cofree ExpF (Maybe [Info SimpleType]), Map.Map String [Type])
calcSimpleTypes exp = do
  let (cexp, Result constraints assumptions) = generateConstraints calcSimpleTypesAlg exp
  subs <- solveConstraints constraints
  pure ((substiute subs) <$$$> cexp, Map.map (fmap (substiute subs)) assumptions)

testProg :: Exp
testProg = [prog|
    grinMain =
      i <- pure 1
      c <- pure #'a'
      s <- pure #"abc"
      d <- pure c
      pure ()
  |]

testMain = do
  let Just (cexp, res) = calcSimpleTypes testProg
  print res
  putStrLn $ showWide $ prettyAnnExp $ fmap (const id) cexp
