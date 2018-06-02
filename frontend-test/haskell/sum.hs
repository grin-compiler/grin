import Prelude hiding (sum)

data List a
  = Nil
  | Cons a (List a)

main = print $ sum $ upto 1 100000

upto :: Int -> Int -> List Int
upto m n = if m > n
            then Nil
            else Cons m $ upto (m+1) n

sum :: List Int -> Int
sum l = case l of
  Nil       -> 0
  Cons n ns -> n + sum ns
