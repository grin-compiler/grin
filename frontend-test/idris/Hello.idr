module Main

import GrinFFI


fn : Int -> Int
fn x = if x > 0
  then x + fn (x - 1)
  else 0

main : IO ()
main = do
  int_print (fn (fn 10))
  pure ()
