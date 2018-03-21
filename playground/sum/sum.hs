module SumSimpleBasic where

data List a
  = Nil
  | Cons a (List a)

hs_sum_pure :: IO ()
hs_sum_pure = print $ sum $ upto 1 100000
 where
  upto :: Int -> Int -> List Int
  upto m n = if m > n
              then Nil
              else Cons m $ upto (m+1) n

  sum :: List Int -> Int
  sum l = case l of
    Nil       -> 0
    Cons n ns -> n + sum ns
