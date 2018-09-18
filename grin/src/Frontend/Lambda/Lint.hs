{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Frontend.Lambda.Lint where

import Text.Printf

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Functor.Foldable
import qualified Data.Foldable

import Frontend.Lambda.Syntax
import Grin.Grin (isPrimName)
import Transformations.Util

lintLambda :: Program -> IO ()
lintLambda prg = do
  let Env{..} = test prg
  printf "node pats:\n%s" . unlines . map show $ Set.toList envCon
  printf "unknown:\n%s" . unlines . map show $ Set.toList (Set.difference envUse envDef)
  printf "errors:\n%s" . unlines . map show $ Set.toList envErr
  --printf "unused:\n%s" . unlines . map show $ Set.toList (Set.difference envDef envUse)

data Env
  = Env
  { envDef  :: Set String
  , envUse  :: Set String
  , envCon  :: Set String
  , envErr  :: Set String
  }

instance Monoid Env where
  mempty = env
  mappend (Env a1 b1 c1 d1) (Env a2 b2 c2 d2) = Env (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

env = Env
  { envDef  = mempty
  , envUse  = mempty
  , envCon  = mempty
  , envErr  = mempty
  }

test = cata folder where
  folder = \case
    -- use
    VarF name -> env {envUse = Set.singleton name}
    -- def
    DefF name args e  -> env {envDef = Set.fromList $ name : args} <> e
    LetRecF binds e   -> mconcat [env {envDef = Set.singleton name} <> a | (name, a) <- binds] <> e
    LetSF binds e     -> mconcat [env {envDef = Set.singleton name} <> a | (name, a) <- binds] <> e
    LetF binds e      -> mconcat [env {envDef = Set.singleton name} <> a | (name, a) <- binds] <> e
    LamF name e       -> env {envDef = Set.singleton name} <> e
    AltF (NodePat con args) e -> env {envDef = Set.fromList $ args, envCon = Set.singleton $ show (length args) ++ "-" ++ con} <> e
    -- err
    LitF (LError err) -> env {envErr = Set.singleton err}
    e -> Data.Foldable.fold e

{-
data Exp
  = Program     [Exp]
  -- Binding
  | Def         Name [Name] Exp
  -- Exp
  | App         Name [Exp]
  | AppCore     Exp Exp
  | Case        Exp [Exp]
  | Let         [(Name, Exp)] Exp -- lazy let
  | LetRec      [(Name, Exp)] Exp -- lazy let with mutually recursive bindings
  | LetS        [(Name, Exp)] Exp -- strict let
  | Con         Name [Exp]
  -- Atom
  | Var         Name
  | Lit         Lit
  -- Alt
  | Alt         Pat Exp
  -- Extra
  | Lam         Name Exp
  deriving (Eq, Ord, Show)

data Lit
  = LInt64  Int64
  | LWord64 Word64
  | LFloat  Float
  | LBool   Bool
  | LError  String
  deriving (Eq, Ord, Show)

data Pat
  = NodePat Name [Name]
  | LitPat  Lit
  | DefaultPat
  deriving (Eq, Show, Ord)
-}
