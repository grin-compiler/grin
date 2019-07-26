{-# LANGUAGE TypeFamilies, LambdaCase, TypeApplications, DeriveGeneric #-}
module Test.Hspec.Compiler where

import CLI.Lib (mainWithArgs)
import Control.Arrow ((&&&))
import Control.Exception (catch)
import Control.Monad (forM, when)
import Data.ByteString.Char8 (ByteString)
import Data.Char (isDigit)
import Data.IORef
import Data.List (isSuffixOf)
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

-- TODO: Documentation
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

-- TODO: add input information
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
              , "--save-binary-intermed"
              , "--optimize"
              , "--save-elf=end-to-end-test.bin"
              , "--runtime-c-path=./grin/test-runtime/runtime.c"
              , "--primops-c-path=./grin/prim_ops.c"
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
            res <- bisect ".end-to-end-test" grinOut
            writeIORef result res
    )
    (writeIORef result . Result "" . Failure Nothing . Error Nothing)
  readIORef result

loopM :: (Monad m) => (a -> m (Either a b)) -> a -> m b
loopM n a0 = n a0 >>= \case
  Left a  -> loopM n a
  Right b -> pure b

class Monad m => BisectM m where
  createFileMap :: FilePath -> m (Map.Map Int FilePath)
  runTest       :: FilePath -> ByteString -> m Bool

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
          ]
    (grinOut, ()) <- redirectStdout $ mainWithArgs compArgs
    pure $ grinOut == exp

bisect :: (BisectM m) => FilePath -> ByteString -> m Result
bisect directory expected = do
  fileMap <- createFileMap directory
  let (mn, mx) = findRange fileMap
  tn <- runTest (fileMap Map.! mn) expected
  tx <- runTest (fileMap Map.! mx) expected
  loopM (go fileMap) ((mn,tn), (mx, tx))
  where
    go fm ((mn,tn), (mx, tx))
      | not tn && not tx = pure $ Right $ Result "" $ Failure Nothing $ Reason "Min and max were failures. This could indicate different errors."
      |     tn &&     tx = pure $ Right $ Result "" $ Failure Nothing $ Reason "Min and max were success. This shouldn't have happened."
      | mn > mx  = pure $ Right $ Result "" $ Failure Nothing $ Reason "Min exceded max, something went really wrong."
      | mn == mx = pure $ Right $ Result "" $ Failure Nothing $ Reason "Min==max this should have not happened."
      | mn + 1 == mx = case (tn, tx) of
          (True, False) -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Test failed in pipeline step: " ++ show tx
          (False, True) -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Test failed in pipeline step: " ++ show tn
          conf          -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Unhandled configuration: " ++ show conf
          -- report the one which failed
      | mn < mx = do
          let md = (mx - mn) `div` 2
          td <- runTest (fm Map.! md) expected -- We suppose that md exists
          case (tn, td, tx) of
            (False, False, True) -> pure $ Left ((md,td), (mx,tx))
            (False, True, True)  -> pure $ Left ((mn,tn), (md,td))
            (True, False, False) -> pure $ Left ((mn,tn), (md,td))
            (True, True, False)  -> pure $ Left ((md,td), (mx,tx))
            conf -> pure $ Right $ Result "" $ Failure Nothing $ Reason $ "Unhandled configuration: " ++ show conf
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
