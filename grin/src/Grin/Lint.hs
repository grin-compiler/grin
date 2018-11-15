{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
module Grin.Lint (lint, Error(..)) where

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
import Data.List (findIndices)
import Lens.Micro.Platform
import Data.Text.Short (isPrefixOf)
import Text.PrettyPrint.ANSI.Leijen (Pretty, plain)
import Data.String


import Grin.Grin
import Grin.Pretty
import Grin.TypeEnv hiding (typeOfVal)
import Transformations.Util

import Debug.Trace
import Data.Maybe

{-
Linter is responsible for the semantical checks of the program.
-}

{-
  - AST shape (syntax)
    done - exp
    - val
  - recognise primitive functions
  - scope checking ; requires monad (full traversal)
  - type checking (using the type env)
  - pattern checking
  - node item checking (no node in node)
  - case:
    - overlapping alternatives
    - uncovered cases
    - fully saturated alternatives
  - fully saturated node creation eg (CPair 1 2) vs (CPair 1)
-}

{-
  question:
    how to show errors?
      - annotate expressions with the error using cofree
      - annotate expressionf with id using cofree, then build error map referencing to expressions
-}

data Error = Error
  { before  :: Bool
  , message :: String
  }

msg :: String -> Error
msg = Error False

beforeMsg :: String -> Error
beforeMsg = Error True

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
  , envDefinedNames :: Map Name DefRole
  , envFunArity     :: Map Name Int
  }

emptyEnv = Env
  { envNextId       = 0
  , envVars         = mempty
  , envErrors       = mempty
  , envDefinedNames = mempty
  , envFunArity     = mempty
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

  _ -> tell [msg $ "Syntax error - expected " ++ showValCtx ctx]

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
  | otherwise = tell [msg $ "Syntax error - expected " ++ showExpCtx ctx]

check :: ExpF (ExpCtx, Exp) -> Check () -> Lint (CCTC.CofreeF ExpF Int (ExpCtx, Exp))
check exp nodeCheckM = do
  idx <- expId
  errors <- execWriterT (nodeCheckM >> checkVarScopeM exp)
  unless (null errors) $ do
    modify' $ \env@Env{..} -> env {envErrors = Map.insert idx errors envErrors}
  nextId
  pure (idx CCTC.:< exp )

checkNameDef :: DefRole -> Name -> Check ()
checkNameDef role name = do
  defined <- state $ \env@Env{..} ->
    ( Map.member name envDefinedNames
    , env {envDefinedNames = Map.insert name role envDefinedNames}
    )
  when defined $ do
    tell [msg $ printf "multiple defintion of %s" name]

checkNameUse :: Name -> Check ()
checkNameUse name = do
  defined <- state $ \env@Env{..} -> (Map.member name envDefinedNames, env)
  unless defined $ do
    tell [msg $ printf "undefined variable: %s" name]

checkVarScopeM :: ExpF a -> Check ()
checkVarScopeM exp = do
  case exp of
    DefF _ _ _ -> pure () -- Function definitions are already registered
    _          -> mapM_ (uncurry checkNameDef) $ foldNameDefExpF (\r n -> [(r,n)]) exp
  mapM_ checkNameUse $ foldNameUseExpF (:[]) exp

plainShow :: (Pretty p) => p -> String
plainShow = show . plain . pretty

lint :: Maybe TypeEnv -> Exp -> (Cofree ExpF Int, Map Int [Error])
lint mTypeEnv exp = fmap envErrors $ flip runState emptyEnv $ do
  cata functionNames exp
  anaM builder (ProgramCtx, exp)
  where
  functionNames :: ExpF (Lint ()) -> Lint ()
  functionNames = \case
    ProgramF defs -> sequence_ defs
    DefF name args body -> do
      modify' $ \env@Env{..} -> env
        { envDefinedNames = Map.insert name FunName envDefinedNames
        , envFunArity = Map.insert name (length args) envFunArity
        }
      forM_ args $ \p -> modify' $ \env@Env{..} -> env { envDefinedNames = Map.insert p FunParam envDefinedNames }
      body
    rest -> pure ()

  builder :: (ExpCtx, Exp) -> Lint (CCTC.CofreeF ExpF Int (ExpCtx, Exp))
  builder (ctx, e) = case e of

    Program{} -> checkWithChild DefCtx $ do
      syntaxE ProgramCtx

    Def name args _ -> checkWithChild ExpCtx $ do
      syntaxE DefCtx

    -- Exp
    EBind leftExp lpat rightExp -> check (EBindF (SimpleExpCtx, leftExp) lpat (ExpCtx, rightExp)) $ do
      syntaxE ExpCtx

    ECase val alts -> checkWithChild AltCtx $ do
      syntaxE SimpleExpCtx

      -- Overlapping node alternatives
      let tagOccurences =
            Map.unionsWith (+) $
            map (`Map.singleton` 1) $
            concatMap (^.. _AltCPat . _CPatNodeTag) alts
      forM_ (Map.keys $ Map.filter (>1) tagOccurences) $ \(tag :: Tag) ->
        tell [beforeMsg $ printf "case has overlapping node alternatives %s" (plainShow tag)]

      -- Overlapping literal alternatives
      let literalOccurences =
            Map.unionsWith (+) $
            map (`Map.singleton` 1) $
            concatMap (^.. _AltCPat . _CPatLit) alts
      forM_ (Map.keys $ Map.filter (>1) literalOccurences) $ \(lit :: Lit) ->
        tell [beforeMsg $ printf "case has overlapping literal alternatives %s" (plainShow lit)]

      let noOfDefaults = length $ findIndices (has (_AltCPat . _CPatDefault)) alts
      -- More than one default
      when (noOfDefaults > 1) $ do
        tell [beforeMsg $ "case has more than one default alternatives"]

      forM_ mTypeEnv $ \typeEnv -> do
        -- Case variable has a location type
        case val of
          (Var name) | Just _ <- typeEnv ^? variable . at name . _Just . _T_SimpleType . _T_Location ->
            tell [beforeMsg $ printf "case variable %s has a location type" name]
          _ -> pure () -- TODO

        -- Non-covered alternatives
        when (noOfDefaults == 0) $ do
          case val of
            (Var name) | Just tags <- typeEnv ^? variable . at name . _Just . _T_NodeSet . to Map.keys -> do
              forM_ tags $ \tag -> when (Map.notMember tag tagOccurences) $ do
                tell [beforeMsg $ printf "case has non-covered alternative %s" (plainShow tag)]
            _ -> pure () -- TODO

    -- Simple Exp
    SApp name args -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      -- Test existence of the function.
      Env{..} <- get
      when (not $ "_prim_" `isPrefixOf` name) $
        case Map.lookup name envDefinedNames of
          (Just FunName) -> pure ()
          (Just _)       -> tell [msg $ printf "non-function in function call: %s" name]
          Nothing        -> tell [msg $ printf "non-defined function is called: %s" name]
      -- Non saturated function call
      forM_ (Map.lookup name envFunArity) $ \n -> when (n /= length args) $ do
        tell [msg $ printf "non-saturated function call: %s" name]
      mapM_ (syntaxV SimpleValCtx) args

    SReturn val -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      syntaxV ValCtx val

    SStore val -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      syntaxV ValCtx val
      forM_ mTypeEnv $ \typeEnv -> do
        -- Store has given a primitive type
        case val of
          (Lit lit) -> tell [msg $ printf "store has given a primitive value: %s" (plainShow val)]
          (ConstTagNode _ _) -> pure ()
          (Var name) | Just tags <- typeEnv ^? variable . at name . _Just . _T_NodeSet . to Map.keys -> pure ()
                     | Just st   <- typeEnv ^? variable . at name . _Just . _T_SimpleType -> do
                        tell [msg $ printf "store has given a primitive value: %s :: %s" (plainShow val) (plainShow st)]
          _ -> pure ()

    SFetchI name _ -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      -- Non location parameter for fetch
      forM_ mTypeEnv $ \typeEnv -> do
        case (typeEnv ^? variable . at name . _Just . _T_SimpleType . _T_Location) of
          Just _ -> pure ()
          Nothing -> tell [msg $ printf "the parameter of fetch is non-location: %s" (plainShow name)]

    SUpdate name val -> checkWithChild ctx $ do
      syntaxE SimpleExpCtx
      syntaxV ValCtx val

      -- Non location parameter for update
      forM_ mTypeEnv $ \typeEnv -> do
        case (typeEnv ^? variable . at name . _Just . _T_SimpleType . _T_Location) of
          Just _ -> pure ()
          Nothing -> tell [msg $ printf "the parameter of update is non-location: %s" (plainShow name)]

      forM_ mTypeEnv $ \typeEnv -> do
        -- Update has given a primitive type
        case val of
          (Lit lit) -> tell [msg $ printf "update has given a primitive value: %s" (plainShow val)]
          (ConstTagNode _ _) -> pure ()
          (Var name) | Just tags <- typeEnv ^? variable . at name . _Just . _T_NodeSet . to Map.keys -> pure ()
                     | Just st   <- typeEnv ^? variable . at name . _Just . _T_SimpleType -> do
                        tell [msg $ printf "update has given a primitive value: %s :: %s" (plainShow val) (plainShow st)]
          _ -> pure ()

      --typeN LocationType name
      --isNodePtr name val

    SBlock{} -> checkWithChild ExpCtx $ do
      syntaxE SimpleExpCtx

    -- Alt
    Alt cpat _ -> checkWithChild ExpCtx $ do
      syntaxE AltCtx

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
