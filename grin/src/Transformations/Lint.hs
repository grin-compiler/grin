{-# LANGUAGE LambdaCase, TupleSections, RecordWildCards #-}
module Transformations.Lint where

import Data.Functor.Foldable as Foldable
import qualified Data.Foldable
import Control.Comonad.Cofree
import Control.Monad.State
import Control.Monad.Writer hiding (Alt)
import qualified Control.Comonad.Trans.Cofree as CCTC

import Data.Map (Map)
import qualified Data.Map as Map

import Grin
import TypeEnv

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

lint :: TypeEnv -> Exp -> Cofree ExpF [Error]
lint typeEnv exp = ana builder (ProgramCtx, exp) where
  builder :: (SyntaxCtx, Exp) -> CCTC.CofreeF ExpF [Error] (SyntaxCtx, Exp)
  builder (ctx, e) = case e of
    Program{} -> ann ((DefCtx,) <$> project e) $ do
      checkSyntax ProgramCtx
      -- check multiple definitions

    Def{} -> ann ((ExpCtx,) <$> project e) $ do
      checkSyntax DefCtx

    -- Exp
    EBind leftExp lpat rightExp -> ann (EBindF (SimpleExpCtx, leftExp) lpat (ExpCtx, rightExp)) $ do
      checkSyntax ExpCtx
    ECase{} -> ann ((AltCtx,) <$> project e) $ do
      checkSyntax ExpCtx

    -- Simple Exp
    SApp{} -> ann ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SReturn{} -> ann ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SStore{} -> ann ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SFetchI{} -> ann ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SUpdate{} -> ann ((ctx,) <$> project e) $ do
      checkSyntax SimpleExpCtx
    SBlock{} -> ann ((ExpCtx,) <$> project e) $ do
      checkSyntax SimpleExpCtx

    -- Alt
    Alt{} -> ann ((ExpCtx,) <$> project e) $ do
      checkSyntax AltCtx

    where
      checkSyntax :: SyntaxCtx -> Writer [Error] ()
      checkSyntax expCtx
        | expCtx == ctx = pure ()
        | expCtx == SimpleExpCtx && ctx == ExpCtx = pure ()
        | otherwise = tell ["Syntax error - expected " ++ showCtx ctx]

ann exp m = execWriter m CCTC.:< exp

----------

data Env
  = Env
  { envNextId     :: Int
  , envSyntaxCtx  :: SyntaxCtx
  , envVars       :: Map Name Int -- exp id
  , envErrors     :: Map Int [Error]
  }

emptyEnv = Env
  { envNextId     = 0
  , envSyntaxCtx  = ProgramCtx
  , envVars       = mempty
  , envErrors     = mempty
  }

type Lint = State Env

gen :: Lint Int
gen = state $ \env@Env{..} -> (envNextId, env {envNextId = succ envNextId})

checkSyntax = undefined
setSyntax = undefined

lint2 :: Exp -> (Cofree ExpF Int, Map Int [Error])
lint2 exp = fmap envErrors $ runState (cata folder exp) emptyEnv where
  folder e = do
    expId <- gen
    (expId :<) <$> case e of
      ProgramF defsM -> do
        checkSyntax ProgramCtx
        ProgramF <$> sequence [setSyntax DefCtx >> d | d <- defsM]

      DefF name args bodyM -> do
        checkSyntax DefCtx
        DefF name args <$> (setSyntax ExpCtx >> bodyM)

      -- Exp
      EBindF leftExpM lpat rightExpM -> do
        checkSyntax ExpCtx
        EBindF <$> (setSyntax SimpleExpCtx >> leftExpM) <*> pure lpat <*> (setSyntax ExpCtx >> rightExpM)

      -- Case: Simple Exp + Alt
      ECaseF val altsM -> do
        checkSyntax SimpleExpCtx
        ECaseF val <$> sequence [setSyntax AltCtx >> a | a <- altsM]

      AltF cpat expM -> do
        checkSyntax AltCtx
        AltF cpat <$> (setSyntax ExpCtx >> expM)

      -- Simple Exp
      SAppF name args -> do
        checkSyntax SimpleExpCtx
        pure $ SAppF name args

      SReturnF val -> do
        checkSyntax SimpleExpCtx
        pure $ SReturnF val

      SStoreF val -> do
        checkSyntax SimpleExpCtx
        pure $ SStoreF val

      SFetchIF name idx -> do
        checkSyntax SimpleExpCtx
        pure $ SFetchIF name idx

      SUpdateF name val -> do
        checkSyntax SimpleExpCtx
        pure $ SUpdateF name val

      SBlockF expM -> do
        checkSyntax SimpleExpCtx
        SBlockF <$> (setSyntax ExpCtx >> expM)
