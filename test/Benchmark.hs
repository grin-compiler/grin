module Main where

import Criterion
import Criterion.Main

import ParseGrin


fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = do
  defaultMainWith
    defaultConfig
    [ bench "sum_opt" $ nfIO $ eval' "grin/sum_opt.grin"
    , bench "sum_simple" $ nfIO $ eval' "grin/sum_simple.grin"
    ]
