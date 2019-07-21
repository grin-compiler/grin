module Main where

import Test.Hspec (hspec)
import Test.Hspec.Compiler (endToEnd)


main :: IO ()
main = hspec $ endToEnd "./grin/test-data/"
