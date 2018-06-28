{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module LintSpec where

import Test.Hspec
import Grin.Grin
import Grin.GrinTH
import Grin.Lint
import Grin.TypeEnv
import qualified Data.Map as Map


runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = do
  describe "Undefined parameter" $ do
    it "is found" $ do
      let program = [prog|
          undefinedParam p1 p2 =
            _prim_int p3 p2
        |]
      let (_, errors) = lint Nothing program
      -- TODO: Better check.
      Map.null errors `shouldBe` False
