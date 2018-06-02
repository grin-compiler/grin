{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Lint (lint, Error) where

import Text.Printf

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Control.Comonad.Cofree
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)
import qualified Control.Comonad.Trans.Cofree as CCTC

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

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


data ExpCtx
  = ProgramCtx
  | DefCtx
  | ExpCtx
  | SimpleExpCtx
  | AltCtx
  deriving Eq

data ValCtx
  = ValCtx
  | SimpleValCtx
  deriving Eq

showExpCtx :: ExpCtx -> String
showExpCtx = \case
  ProgramCtx    -> "Program"
  DefCtx        -> "Def"
  ExpCtx        -> "Exp"
  SimpleExpCtx  -> "SimpleExp"
  AltCtx        -> "Alt"

showValCtx :: ValCtx -> String
showValCtx = \case
  ValCtx        -> "Val"
  SimpleValCtx  -> "SimpleVal"

data Env
  = Env
  { envNextId       :: Int
  , envVars         :: Map Name Int -- exp id
  , envErrors       :: Map Int [Error]
  , envDefinedNames :: Set Name
  }

emptyEnv = Env
  { envNextId       = 0
  , envVars         = mempty
  , envErrors       = mempty
  , envDefinedNames = mempty
  }

type Lint   = State Env
type Check  = WriterT [Error] Lint

expId :: Lint Int
expId = gets envNextId

nextId :: Lint ()
nextId = modify' $ \env@Env{..} -> env {envNextId = succ envNextId}

{-
  TODO:
    type check
-}

syntaxVal :: ValCtx -> Val -> Check ()
syntaxVal ctx = \case
  Lit{} -> pure ()
  Var{} -> pure ()

  ConstTagNode _ args
    | ctx == ValCtx
    -> mapM_ (syntaxVal SimpleValCtx) args

  VarTagNode _ args
    | ctx == ValCtx
    -> mapM_ (syntaxVal SimpleValCtx) args

  _ | ctx == ValCtx
    -> pure ()

  _ -> tell ["Syntax error - expected " ++ showValCtx ctx]

{-
  ConstTagNode  Tag  [SimpleVal] -- complete node (constant tag) ; HIGH level GRIN
  VarTagNode    Name [SimpleVal] -- complete node (variable tag)
  ValTag        Tag
  Unit                           -- HIGH level GRIN
  Lit Lit                        -- HIGH level GRIN
  Var Name                       -- HIGH level GRIN
-}

syntaxExp :: ExpCtx -> ExpCtx -> Check ()
syntaxExp ctx expCtx
  | expCtx == ctx = pure ()
  | expCtx == SimpleExpCtx && ctx == ExpCtx = pure () -- simple exp is also an exp
  | otherwise = tell ["Syntax error - expected " ++ showExpCtx ctx]

check :: ExpF (ExpCtx, Exp) -> Check () -> Lint (CCTC.CofreeF ExpF Int (ExpCtx, Exp))
check exp nodeCheckM = do
  idx <- expId
  errors <- execWriterT (nodeCheckM >> checkVarScopeM exp)
  unless (null errors) $ do
    modify' $ \env@Env{..} -> env {envErrors = Map.insert idx errors envErrors}
  nextId
  pure (idx CCTC.:< exp )

checkNameDef :: Name -> Check ()
checkNameDef name = do
  defined <- state $ \env@Env{..} -> (Set.member name envDefinedNames, env {envDefinedNames = Set.insert name envDefinedNames})
  when defined $ do
    tell [printf "multiple defintion of %s" name]

checkNameUse :: Name -> Check ()
checkNameUse name = do
  defined <- state $ \env@Env{..} -> (Set.member name envDefinedNames, env {envDefinedNames = Set.insert name envDefinedNames})
  unless defined $ do
    tell [printf "undefined variable: %s" name]

checkVarScopeM :: ExpF a -> Check ()
checkVarScopeM exp = mapM_ checkNameUse $ foldNameUseExpF (:[]) exp

lint :: Maybe TypeEnv -> Exp -> (Cofree ExpF Int, Map Int [Error])
lint mTypeEnv exp = fmap envErrors $ runState (anaM builder (ProgramCtx, exp)) emptyEnv where

  builder :: (ExpCtx, Exp) -> Lint (CCTC.CofreeF ExpF Int (ExpCtx, Exp))
  builder (ctx, e) = case e of

    Program{} -> checkWithChild DefCtx $ do
      syntaxE ProgramCtx

    Def name args _ -> checkWithChild ExpCtx $ do
      syntaxE DefCtx
      -- check for multiple definitions
      mapM_ checkNameDef $ name : args

    -- Exp
    EBind leftExp lpat rightExp -> check (EBindF (SimpleExpCtx, leftExp) lpat (ExpCtx, rightExp)) $ do
      syntaxE ExpCtx
      -- check for multiple definitions
      mapM_ checkNameDef $ foldNamesVal (:[]) lpat

    ECase val alts -> checkWithChild AltCtx $ do
      syntaxE SimpleExpCtx
      {-
        val must not have location type
      -}

    -- Simple Exp
    SApp name args -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      mapM_ (syntaxV SimpleValCtx) args

    SReturn val -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      syntaxV ValCtx val

    SStore val -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      syntaxV ValCtx val
      --typeV NodeTypeCtx val

    SFetchI name _ -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      --typeN LocationType name

    SUpdate name val -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      syntaxV ValCtx val
      --typeN LocationType name
      --isNodePtr name val

    SBlock{} -> checkWithChild ExpCtx $ do
      syntaxE SimpleExpCtx

    -- Alt
    Alt cpat _ -> checkWithChild ExpCtx $ do
      syntaxE AltCtx
      -- check for multiple definitions
      case cpat of
        NodePat _ args -> mapM_ checkNameDef args
        _ -> pure ()

    where
      syntaxE = syntaxExp ctx
      syntaxV = syntaxVal
      checkWithChild childCtx m = check ((childCtx,) <$> project e) m
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
