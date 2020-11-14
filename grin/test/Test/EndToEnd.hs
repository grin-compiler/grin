{-# LANGUAGE TypeFamilies, LambdaCase, TypeApplications, DeriveGeneric, ScopedTypeVariables #-}
module Test.EndToEnd where

import CLI.Lib (mainWithArgs)
import Control.Arrow ((&&&))
import Control.Exception (catch)
import Control.Monad (forM, when)
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit)
import Data.IORef
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.String.Utils (replace)
import Data.Yaml as Yaml
import GHC.Generics
import GHC.IO.Handle
import System.Directory
import System.Directory (doesFileExist, removeDirectoryRecursive)
import System.Directory (doesFileExist, removeFile)
import System.Directory.Tree
import System.FilePath.Posix
import System.Posix.Redirect
import System.Process
import Test.Hspec.Core.Spec hiding (pending)
import Text.Printf
import qualified Data.Map as Map


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
    { compilerInput     :: InputFile  -- name.grin name.binary
    , compilerOptions   :: [String]   -- input.opts
    , compilerExpected  :: FilePath   -- input.expected
    , compilerOutputExt :: FilePath   -- input.out -- defined in configuration
    }
  | EndToEndTest
    { compilerInput :: InputFile
    }

-- | evaluatePipelineTest reads the input file, runs grin compiler using the
-- defined options. All the $$$OUT$$$ template value replaced the output file.
-- Only one output file should be configured in the options as we can define
-- only one expected output. The output file should be text file of some sort.
evaluatePipelineTest input options expected ext params actionWith progressCallback = do
  result <- newIORef $ Result "" $ Failure Nothing $ Reason "End-to-end test did not set test as success."
  actionWith $ \() -> catch
    (do let args = case input of
                    Binary fp -> [fp, "--load-binary"]
                    Textual fp -> [fp]
        let outFile = inputToFilePath input <.> "out"
        mainWithArgs $ args ++ (map (replace "$$$OUT$$$" outFile) options)
        content  <- readFile (inputToFilePath input <.> ext)
        expected <- readFile expected
        if (content == expected)
          then writeIORef result $ Result "" Success
          else writeIORef result $ Result "" $ Failure Nothing $ ExpectedButGot Nothing expected content
    )
    (writeIORef result . Result "" . Failure Nothing . Error Nothing)
  readIORef result

-- | evaluateEndToEndTest reads the input file, interprets it without any
-- optimization, after runs the full optimization pipeline, generates an executable and runs
-- the executable. It compares its stdout and if the stdout differs from the
-- interpreted one it tries to find the culprit optimization step.
--
-- Currently this approach doesn't support reading input from the stdin. It will be added
-- when we have a tests which need them.
evaluateEndToEndTest input params actionWith progressCallback = do
  result <- newIORef $ Result "" $ Failure Nothing $ Reason "End-to-end test did not set test as success."
  actionWith $ \() -> catch
    (do let fileArgs = case input of
                        Binary fp  -> [fp, "--load-binary"]
                        Textual fp -> [fp]
        let evalArgs = ["--output-dir=.end-to-end-test", "--quiet", "--eval"]
        (grinOut, ()) <- redirectStdout $ mainWithArgs $ fileArgs ++ evalArgs
        removeDirectoryRecursive ".end-to-end-test"
        let compArgs =
              [ "--output-dir=.end-to-end-test"
              , "--quiet"
              , "--continue-on-failed-lint"
              , "--save-binary-intermed"
              , "--optimize"
              , "--save-elf=end-to-end-test.bin"
              , "--c-file=./test-runtime/runtime.c"
              , "--c-file=./prim_ops.c"
              ]
        mainWithArgs $ fileArgs ++ compArgs
        let runTest = (shell "./end-to-end-test.bin")
                      { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
        (mIn, Just out, Just err, runTestPh) <- createProcess_ "./end-to-end-test.bin" runTest
        runTestExitCode <- waitForProcess runTestPh
        doesFileExist "./end-to-end-test.bin" >>= flip when (removeFile "./end-to-end-test.bin")
        testOut <- hGetContents out
        if (grinOut == fromString testOut)
          then writeIORef result $ Result "" Success
          else do
            writeIORef result $ Result "" $ Failure Nothing $ Reason "End-to-end test started bisecting but it did not finish."
            res <- bisect ".end-to-end-test" (fromString testOut)
            writeIORef result res
    )
    (writeIORef result . Result "" . Failure Nothing . Error Nothing)
  readIORef result

loopM :: (Monad m) => (a -> m (Either a b)) -> a -> m b
loopM n a0 = n a0 >>= \case
  Left a  -> loopM n a
  Right b -> pure b

-- BisectM is a collection of operations that are needed for the bisecting algorithm.
class Monad m => BisectM m where
  -- | createFileMap creates a Map that associates intermediate step numbers with
  -- files created for the step.
  createFileMap :: FilePath -> m (Map.Map Int FilePath)
  -- | runTest checks if the intermediate grin file produces the expected result to the stdout.
  -- returns True if the stdout is the same as the expected result, otherwise False.
  runTest :: FilePath -> ByteString -> m Bool

instance BisectM IO where
  createFileMap directory =
      fmap (cFileMap . filter isGrinFile) $ listDirectory directory
    where
      noOfDigits = 3
      isGrinFile name = (all isDigit (take noOfDigits name)) && ".binary" `isSuffixOf` name
      cFileMap files = Map.fromList $
        [ (itr, directory </> name)
        | name <- files
        , let itr = read @Int (take noOfDigits name)
        ]

  runTest file exp = do
    let compArgs =
          [ file
          , "--quiet"
          , "--load-binary"
          , "--eval"
          , "--continue-on-failed-lint"
          ]
    (grinOut, ()) <- redirectStdout $ mainWithArgs compArgs
    pure $ grinOut == exp

bisect :: forall m . (BisectM m) => FilePath -> ByteString -> m Result
bisect directory expected = do
  fileMap <- createFileMap directory
  let (mn, mx) = findRange fileMap
  let fileToTest x = fromMaybe (error $ show mn) $ Map.lookup x fileMap
  tn <- runTest (fileToTest mn) expected
  tx <- runTest (fileToTest mx) expected
  loopM (reduceRange fileMap) ((mn,tn), (mx,tx))
  where
    -- The range is represented as ((Int,Bool),(Int,Bool)) where
    -- the (Int, Bool) means the intermediate step its test result.
    -- Storing the test result in the form of Bool is an optimization step.
    -- The reduceRange will compute a new range, checking the result
    -- of the test run associated with the middle element, and decides
    -- which direction to go (min,mid) or (mid,max)
    -- The assumption is that min fails and max succeeds.
    reduceRange :: Map.Map Int FilePath -> ((Int,Bool),(Int,Bool)) -> m (Either ((Int,Bool),(Int,Bool)) Result)
    reduceRange fm ((mn,tn), (mx,tx))
      | not tn && not tx = pure $ Right $ Result "" $ Failure Nothing
                                $ Reason $ printf "Min (%d) and Max (%d) were failures. This could indicate different errors." mn mx
      |     tn &&     tx = pure $ Right $ Result "" $ Failure Nothing
                                $ Reason $ printf "Min (%d) and Max (%d) were success. This shouldn't have happened." mn mx
      | mn > mx  = pure $ Right $ Result "" $ Failure Nothing $ Reason "Min exceeded Max, something went really wrong."
      | mn == mx = pure $ Right $ Result "" $ Failure Nothing $ Reason "Min==Max this should have not happened."
      | mn + 1 == mx = case (tn, tx) of
          (True, False) -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Test failed in pipeline step: " ++ show mx
          (False, True) -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Test failed in pipeline step: " ++ show mn
          conf          -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Non-handled configuration: " ++ show conf
          -- report the one which failed
      | mn < mx = do
          let fileToTest x = fromMaybe (error $ show mn) $ Map.lookup x fm
          let md = (((mx - mn) `div` 2) + mn)
          td <- runTest (fileToTest md) expected -- We assume that the middle element exists.
          case (tn, td, tx) of
            (False, False, True) -> pure $ Left ((md,td), (mx,tx))
            (False, True, True)  -> pure $ Left ((mn,tn), (md,td))
            (True, False, False) -> pure $ Left ((mn,tn), (md,td))
            (True, True, False)  -> pure $ Left ((md,td), (mx,tx))
            conf -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Non-handled configuration: " ++ show conf
    findRange = (minimum &&& maximum) . Map.keys

instance Example CompilerTest where
  type Arg CompilerTest = ()
  evaluateExample compilerTest = case compilerTest of
    PipelineTest i o e x -> evaluatePipelineTest i o e x
    EndToEndTest i       -> evaluateEndToEndTest i

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

data Options = Options
  { outputExtension :: String
  , grinOptions     :: [String]
  } deriving (Generic)

instance FromJSON Options

createPipelineTest :: FilePath -> IO (Either String CompilerTest)
createPipelineTest fp = do
  eopts <- Yaml.decodeFileWithWarnings (fp <.> "opts")
  case eopts of
    Left errs -> pure $ Left $ show errs
    Right (warnings, Options ext opts) -> do
      pure $ Right $ PipelineTest
        { compilerInput = inputFile fp
        , compilerOptions = opts
        , compilerExpected = fp <.> "expected"
        , compilerOutputExt = ext
        }

treeToSpec :: DirTree (Maybe (Either String CompilerTest)) -> Spec
treeToSpec = \case
  Failed name ex    -> it       name $ pendingWith $ show ex
  Dir name contents -> describe name $ mapM_ treeToSpec contents
  File name test    -> maybe (pure ()) (either (it name . pendingWith) (itName name)) test
  where
    itName name test = case test of
      PipelineTest{} -> it ("pipeline-test: " ++ name) test
      EndToEndTest{} -> it ("end-to-end: " ++ name) test
