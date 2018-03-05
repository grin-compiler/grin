module Sum where

hs_sum_pure :: IO ()
hs_sum_pure = print $ sum 0 1 100000
 where
  sum :: Int -> Int -> Int -> Int
  sum n29 n30 n31
    | n30 > n31 = n29
    | otherwise = sum (n29 + n30) (n30 + 1) n31
