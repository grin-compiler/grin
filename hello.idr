module Main

int_print : Int -> IO Int
int_print i
   = foreign FFI_C "_prim_int_print" (Int -> IO Int) i

fn : Int -> Int
fn x = if x > 0
  then x + fn (x - 1)
  else 0

main : IO ()
main = do
  int_print (fn (fn 10))
  pure ()
