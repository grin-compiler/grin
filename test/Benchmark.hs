module Main where

import Criterion
import Criterion.Main

import Eval

main :: IO ()
main = do
  defaultMainWith defaultConfig
    [ bgroup "Pure"
      [ bench "sum_simple" $ nfIO $ eval' PureReducer "grin/sum_simple.grin"
      , bench "sum_opt" $ nfIO $ eval' PureReducer "grin/sum_opt.grin"
      ]

    , bgroup "ST"
      [ {-bench "sum_simple" $ nfIO $ eval' STReducer "grin/sum_simple.grin"
      , -}bench "sum_opt" $ nfIO $ eval' STReducer "grin/sum_opt.grin"
      ]
{-
    , bgroup "Other"
      [ bench "do/pure" $ nfIO $ eval' PureReducer "grin/do.grin"
--      , bench "do/st" $ nfIO $ eval' STReducer "grin/do.grin"
      ]
-}
    ]
