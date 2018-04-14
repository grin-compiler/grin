module GrinFFI

public export
int_print : Int -> IO Int
int_print i
   = foreign FFI_C "_prim_int_print" (Int -> IO Int) i

