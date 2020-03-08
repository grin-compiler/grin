module Main where

import Test.Hspec (hspec)
import Test.EndToEnd (endToEnd)
import qualified Test.ExtendedSyntax.EndToEnd as ES (endToEnd)


main :: IO ()
main = do
  hspec $ endToEnd "./test-data/"
  hspec $ ES.endToEnd "./test-data-es/"
