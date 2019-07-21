{-# LANGUAGE TypeFamilies, LambdaCase #-}
module Test.Hspec.Compiler where

import Control.Monad (forM, when)
import Test.Hspec.Core.Spec hiding (pending)
import System.Directory.Tree
import System.FilePath.Posix
import System.Directory (doesFileExist)
import Data.Yaml as Yaml
import CLI.Lib (mainWithArgs)
import Data.IORef


data InputFile
  = Binary  FilePath
  | Textual FilePath
  deriving (Eq, Show)

inputToFilePath :: InputFile -> FilePath
inputToFilePath = \case
  Binary fp -> fp
  Textual fp -> fp

data CompilerTest
  = PipelineTest
    { compilerInput    :: InputFile  -- name.grin name.binary
    , compilerOptions  :: [String]   -- input.opts
    , compilerExpected :: FilePath   -- input.expected
    }
  | EndToEndTest
    { compilerInput :: InputFile
    }

-- TODO: Documentation
evaluatePipelineTest input options expected params actionWith progressCallback = do
  result <- newIORef $ Result "" Success
  actionWith $ \() -> do
    let args = case input of
                  Binary fp -> [fp, "--load-binary"]
                  Textual fp -> [fp]
    let outFile = inputToFilePath input <.> "out"
    mainWithArgs $ args ++ options ++ ["--save-grin=" ++ outFile]
    content  <- readFile outFile
    expected <- readFile expected
    when (content /= expected) $ error "BLAH."
  readIORef result

-- TODO: Checking run result and bisecting
evaluateEndToEndTest input params actionWith progressCallback = do
  result <- newIORef $ Result "" Success
  actionWith $ \() -> do
    let args = case input of
                  Binary fp  -> [fp, "--load-binary"]
                  Textual fp -> [fp]
    mainWithArgs args
  readIORef result

instance Example CompilerTest where
  type Arg CompilerTest = ()
  evaluateExample compilerTest = case compilerTest of
    PipelineTest i o e -> evaluatePipelineTest i o e
    EndToEndTest i     -> evaluateEndToEndTest i

endToEnd :: FilePath -> Spec
endToEnd dp = describe "End to end tests" $ do
  compilerTests <- runIO $ readDirectoryWith createTest dp
  treeToSpec $ dirTree compilerTests

createTest :: FilePath -> IO (Maybe (Either String CompilerTest))
createTest fp = do
  let isTest = takeExtension fp `elem` [".grin", ".binary"]
  case isTest of
    False -> pure Nothing
    True -> do
      hasOpts     <- doesFileExist (fp <.> "opts")
      hasExpected <- doesFileExist (fp <.> "expected")
      case (hasOpts, hasExpected) of
        (True, False)  -> pure $ Just $ Left $ unwords ["Missing expected result file:", fp <.> "expected"]
        (False, True)  -> pure $ Just $ Left $ unwords ["Missing options file:", fp <.> "opts"]
        (False, False) -> pure $ Just $ Right $ EndToEndTest $ inputFile fp
        (True, True)   -> Just <$> createPipelineTest fp

inputFile :: FilePath -> InputFile
inputFile fp = case takeExtension fp of
  ".grin"   -> Textual fp
  ".binary" -> Binary  fp

createPipelineTest :: FilePath -> IO (Either String CompilerTest)
createPipelineTest fp = do
  eopts <- Yaml.decodeFileWithWarnings (fp <.> "opts")
  case eopts of
    Left errs -> pure $ Left $ show errs
    Right (warnings, opts) -> do
      print ("YAML warnings:", warnings)
      pure $ Right $ PipelineTest
        { compilerInput = inputFile fp
        , compilerOptions = opts
        , compilerExpected = fp <.> "expected"
        }

treeToSpec :: DirTree (Maybe (Either String CompilerTest)) -> Spec
treeToSpec = \case
  Failed name ex    -> it       name $ pendingWith $ show ex
  Dir name contents -> describe name $ mapM_ treeToSpec contents
  File name test    -> maybe (pure ()) (either (it name . pendingWith) (it name)) test
