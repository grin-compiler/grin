module Parse.ParseSpec where

import System.FilePath

import Test.IO
import Test.Hspec

import Grin.Grin
import Grin.Parse

import Parse.Tests.Interleaved

spec :: Spec
spec = runIO runTests

runTests :: IO ()
runTests = runTestsFrom stackRoot

runTestsGHCi :: IO ()
runTestsGHCi = runTestsFrom stackTest

runTestsFrom :: FilePath -> IO ()
runTestsFrom fromCurDir = do
  runTestsFromWith fromCurDir id 
    [ interleavedSrc 
    ] 
    [ interleavedAstParseSpec
    ]
  runTestsFromWith' parseMarkedTypeEnv fromCurDir id 
    [ interleavedSrc 
    ] 
    [ interleavedTypeEnvParseSpec
    ]


