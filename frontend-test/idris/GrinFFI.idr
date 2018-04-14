module GrinFFI

public export
int_print : Int -> IO Int
int_print i
   = foreign FFI_C "_prim_int_print" (Int -> IO Int) i

public export
bool_print : Bool -> IO Int
bool_print b =
  foreign FFI_C "_prim_int_print" (Int -> IO Int) $
  case b of
    False => 0
    True  => 1
