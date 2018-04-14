module Main

int_print : Int -> IO Int
int_print i
   = foreign FFI_C "_prim_int_print" (Int -> IO Int) i

main : IO ()
main = do
  int_print 5
  pure ()
