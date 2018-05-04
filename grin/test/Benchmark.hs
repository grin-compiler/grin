module Main where

import Criterion
import Criterion.Main

import Eval

hs_sum_opt :: IO Float
hs_sum_opt = do
  n13 <- sum 0 1 10000
  pure n13
 where
  sum :: Float -> Float -> Float -> IO Float
  sum n29 n30 n31 = do
    b2 <- pure $ n30 > n31
    if b2 then
      return n29
    else do
      n18 <- pure $ n30 + 1
      n28 <- pure $ n29 + n30
      sum n28 n18 n31

hs_sum_pure :: IO Float
hs_sum_pure = pure $ sum 0 1 100000
 where
  sum :: Float -> Float -> Float -> Float
  sum n29 n30 n31
    | n30 > n31 = n29
    | otherwise = sum (n29 + n30) (n30 + 1) n31

hs_sum_naive :: IO Float
hs_sum_naive = pure $ sum [1..100000]

main :: IO ()
main = do
  defaultMainWith defaultConfig
    [ bgroup "Pure"
      [ bench "sum_simple" $ nfIO $ eval' PureReducer "grin/sum_simple.grin"
      , bench "sum_opt" $ nfIO $ eval' PureReducer "grin/sum_opt.grin"
      ]

    , bgroup "IO"
      [ bench "sum_simple" $ nfIO $ eval' IOReducer "grin/sum_simple.grin"
      , bench "sum_opt" $ nfIO $ eval' IOReducer "grin/sum_opt.grin"
      ]
    , bgroup "LLVM"
      [ bench "sum_simple" $ nfIO $ eval' LLVMReducer "grin/sum_simple.grin"
      , bench "sum_opt" $ nfIO $ eval' LLVMReducer "grin/sum_opt.grin"
      ]
    , bgroup "GHC"
      [ bench "hs_sum_opt" $ nfIO $ hs_sum_opt
      , bench "hs_sum_pure" $ nfIO $ hs_sum_pure
      , bench "hs_sum_naive" $ nfIO $ hs_sum_naive
      ]
{-
    , bgroup "Other"
      [ bench "do/pure" $ nfIO $ eval' PureReducer "grin/do.grin"
--      , bench "do/st" $ nfIO $ eval' IOReducer "grin/do.grin"
      ]
-}
    ]
