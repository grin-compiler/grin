module Main where

import GHC
import GHC.Paths (libdir)

import HscTypes
import CorePrep
import CoreToStg

import Control.Monad
import Control.Monad.Trans

import System.Environment
import System.Exit

import Frontend.GHC.FromSTG

data Opts
  = Opts
  { inputs :: [FilePath]
  , output :: FilePath
  }

showUsage = do putStrLn "Usage: ghc-grin <haskell-files> [-o <output-file>]"
               exitWith ExitSuccess

getOpts :: IO Opts
getOpts = do xs <- getArgs
             return $ process (Opts [] "a.out") xs
  where
    process opts ("-o":o:xs) = process (opts { output = o }) xs
    process opts (x:xs) = process (opts { inputs = x:inputs opts }) xs
    process opts [] = opts

cg_main :: Opts -> IO ()
cg_main opts = runGhc (Just libdir) $ do
  env <- getSession
  dflags <- getSessionDynFlags
  setSessionDynFlags $ dflags { hscTarget = HscInterpreted, ghcLink = NoLink }

  targets <- forM (inputs opts) $ \input -> guessTarget input Nothing
  setTargets targets
  load LoadAllTargets
  modSum <- getModSummary $ mkModuleName "Main"

  pmod <- parseModule modSum      -- ModuleSummary
  tmod <- typecheckModule pmod    -- TypecheckedSource
  dmod <- desugarModule tmod      -- DesugaredModule
  let core = coreModule dmod      -- CoreModule

  -- Run the Core prep pass
  prep <- liftIO $ corePrepPgm env (mg_module core) (ms_location modSum) (mg_binds core) (mg_tcs core)

  -- Transform Core into STG
  let stg = coreToStg dflags (mg_module core) prep

  -- TODO: convert to grin
  grin <- liftIO $ codegenGrin dflags stg
  pure ()

main :: IO ()
main = do opts <- getOpts
          if (null (inputs opts))
             then showUsage
             else cg_main opts
