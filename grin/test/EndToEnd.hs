module Main where

import Test.Hspec (hspec)
import Test.EndToEnd (endToEnd)


main :: IO ()
main = hspec $ endToEnd "./test-data/"
