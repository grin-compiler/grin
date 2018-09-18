module Main

import GrinFFI


fn : Int -> Int
fn x = if x > 0
  then x + fn (x - 1)
  else 0

lazy_fn : Int -> Lazy Int
lazy_fn x = if x > 0
  then (x + Force (lazy_fn (x - 1)))
  else 0

{-
test_int_op : Int -> Int -> Int -> Int
test_int_op op 0 y = y
test_int_op op x y = case op of
  0 => x + y
  1 => x - y
  2 => x * y
--  3 => div x y -- It needs support for SError
  _ => y

test_int_comp : Int -> Int -> Int -> Bool
test_int_comp 0 _ _  = False
test_int_comp op x y = case op of
  1 => x < y
  2 => x <= y
  3 => x == y
  4 => x >= y
  5 => x > y
  _ => True
-}

main : IO ()
main = do
  int_print (fn (fn 10))
--  int_print (test_int_op 0 10 10)
--  bool_print (test_int_comp 0 10 10)
  int_print $ Force $ lazy_fn (Force (lazy_fn 10))
  pure ()
