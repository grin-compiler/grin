module Transformations.ExtendedSyntax.ConversionSpec where

import Control.DeepSeq

import Grin.Grin
import Grin.Syntax (Exp)
import qualified Grin.ExtendedSyntax.Syntax as New (Exp)
import Transformations.ExtendedSyntax.Conversion

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck

import Test.Assertions
import Test.ExtendedSyntax.Old.Test (SemanticallyCorrectProgram(..))

runTests :: IO ()
runTests = hspec spec

spec :: Spec
spec = describe "Syntax transformation QuickCheck tests" $ do
         prop "Old is always convertible to New" $
           convertibleToNew
         prop "Old is always convertible to New then back to Old" $
           roundtripConvertibleOld

-- NOTE: The conversion itself is the proof that it is convertible
-- QUESTION: There must be a better way to do this
-- ANSWER: The conversion function could an Either
convertibleToNew :: SemanticallyCorrectProgram -> Bool
convertibleToNew exp = force (convertToNew $ correctProg exp) `seq` True

roundtripConvertibleOld :: SemanticallyCorrectProgram -> Bool
roundtripConvertibleOld exp = force (convertToOld $ convertToNew $ correctProg exp) `seq` True where

  convertToOld :: New.Exp -> Exp
  convertToOld = convert
