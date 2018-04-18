module GrinFFI

public export
int_print : Int -> IO ()
int_print i
   = foreign FFI_C "idris_int_print" (Int -> IO ()) i

public export
bool_print : Bool -> IO ()
bool_print b =
  foreign FFI_C "idris_int_print" (Int -> IO ()) $
  case b of
    False => 0
    True  => 1
