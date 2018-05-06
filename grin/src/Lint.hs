{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Lint (lint, Error) where

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Control.Comonad.Cofree
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)
import qualified Control.Comonad.Trans.Cofree as CCTC

import Data.Map (Map)
import qualified Data.Map as Map

import Grin
import TypeEnv hiding (typeOfVal)
import Transformations.Util

{-
  - AST shape (syntax)
    done - exp
    - val
  - scope checking ; requires monad (full traversal)
  - type checking (using the type env)
  - pattern checking
  - node item checking (no node in node)
  - case:
    - overlapping alternatives
    - uncovered cases
-}

{-
  question:
    how to show errors?
      - annotate expressions with the error using cofree
      - annotate expressionf with id using cofree, then build error map referencing to expressions
-}

type Error = String


data SyntaxCtx
  = ProgramCtx
  | DefCtx
  | ExpCtx
  | SimpleExpCtx
  | AltCtx
  deriving Eq

showCtx :: SyntaxCtx -> String
showCtx = \case
  ProgramCtx    -> "Program"
  DefCtx        -> "Def"
  ExpCtx        -> "Exp"
  SimpleExpCtx  -> "SimpleExp"
  AltCtx        -> "Alt"

data Env
  = Env
  { envNextId     :: Int
  , envVars       :: Map Name Int -- exp id
  , envErrors     :: Map Int [Error]
  }

emptyEnv = Env
  { envNextId     = 0
  , envVars       = mempty
  , envErrors     = mempty
  }

type Lint   = State Env
type Check  = WriterT [Error] Lint

expId :: Lint Int
expId = gets envNextId

nextId :: Lint ()
nextId = modify' $ \env@Env{..} -> env {envNextId = succ envNextId}

check :: ExpF (SyntaxCtx, Exp) -> Check () -> Lint (CCTC.CofreeF ExpF Int (SyntaxCtx, Exp))
check exp m = do
  idx <- expId
  errors <- execWriterT m
  unless (null errors) $ do
    modify' $ \env@Env{..} -> env {envErrors = Map.insert idx errors envErrors}
  nextId
  pure (idx CCTC.:< exp)

isSimpleVal :: Val -> Bool
isSimpleVal = \case
  Lit{} -> True
  Var{} -> True
  _ -> False
{-
checkVal val :: Val -> Check ()
checkVal _ = pure ()
-}
lint :: TypeEnv -> Exp -> (Cofree ExpF Int, Map Int [Error])
lint typeEnv exp = fmap envErrors $ runState (anaM builder (ProgramCtx, exp)) emptyEnv where
  builder :: (SyntaxCtx, Exp) -> Lint (CCTC.CofreeF ExpF Int (SyntaxCtx, Exp))
  builder (ctx, e) = case e of
    Program{} -> check ((DefCtx,) <$> project e) $ do
      checkSyntax ProgramCtx
      -- check multiple definitions

    Def{} -> check ((ExpCtx,) <$> project e) $ do
      checkSyntax DefCtx

    -- Exp
    EBind leftExp lpat rightExp -> check (EBindF (SimpleExpCtx, leftExp) lpat (ExpCtx, rightExp)) $ do
      checkSyntax ExpCtx
    ECase{} -> check ((AltCtx,) <$> project e) $ do
      checkSyntax ExpCtx

    -- Simple Exp
    SApp name args -> check ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
      unless (all isSimpleVal args) $ tell ["simple val expected"]
    SReturn{} -> check ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SStore{} -> check ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SFetchI{} -> check ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SUpdate name val -> check ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
      --isNode val
      --isNodePtr name val
    SBlock{} -> check ((ExpCtx,) <$> project e) $ do
      checkSyntax SimpleExpCtx

    -- Alt
    Alt{} -> check ((ExpCtx,) <$> project e) $ do
      checkSyntax AltCtx

    where
      checkSyntax :: SyntaxCtx -> Check ()
      checkSyntax expCtx
        | expCtx == ctx = pure ()
        | expCtx == SimpleExpCtx && ctx == ExpCtx = pure ()
        | otherwise = tell ["Syntax error - expected " ++ showCtx ctx]

{-
  = Program     [Def]
  | Def         Name [Name] Exp
  -- Exp
  | EBind       SimpleExp LPat Exp
  | ECase       Val [Alt]
  -- Simple Exp
  | SApp        Name [SimpleVal]
  | SReturn     Val
  | SStore      Val
  | SFetchI     Name (Maybe Int) -- fetch a full node or a single node item in low level GRIN
  | SUpdate     Name Val
  | SBlock      Exp
  -- Alt
  | Alt CPat Exp
-}
