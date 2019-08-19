module Transformations.SyntaxSpec where

import Control.DeepSeq

import Grin.Grin
import Transformations.Syntax

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Test.Assertions
import Test.ExtendedSyntax.Test()
import qualified Test.ExtendedSyntax.Grammar as G

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = describe "Syntax transformation QuickCheck tests" $
         prop "Old isalways convertible to New" $
          convertibleToNew

-- NOTE: The conversion itself is the proof that it is convertible
-- QUESTION: There must be a better way to do this
-- ANSWER: The conversion function could an Either
convertibleToNew :: G.Exp -> Bool
convertibleToNew exp = force (convertToNew $ G.asExp exp) `seq` True
