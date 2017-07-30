module Main where

import Criterion
import Criterion.Main

import Eval


fact :: Integer -> Integer
fact 1 = 1
fact n = n * fact (n - 1)

main :: IO ()
main = do
  defaultMainWith
    defaultConfig
    [ bench "sum_opt" $ nfIO $ eval' PureReducer "grin/sum_opt.grin"
    , bench "sum_opt" $ nfIO $ eval' STReducer "grin/sum_opt.grin"
    , bench "do" $ nfIO $ eval' PureReducer "grin/do.grin"
    , bench "do" $ nfIO $ eval' STReducer "grin/do.grin"
    ]
