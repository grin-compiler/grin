{-# LANGUAGE OverloadedStrings, QuasiQuotes, ViewPatterns #-}
module Grin.PrimOpsPrelude where

import Grin.Grin
import Grin.TH

{-
  These predefined primitive operations are optional.
  This is a utility module for front-ends and the grin CLI
  use `grin --no-prelude` to exclude these predefined primitive operations
-}

primPrelude :: Program
primPrelude = [progConst|

  ffi effectful
    _prim_int_print     :: T_Int64 -> T_Unit
    _prim_usleep        :: T_Int64 -> T_Unit
    _prim_string_print  :: T_String -> T_Unit
    _prim_read_string   :: T_String
    _prim_error         :: T_String -> T_Unit
    _prim_ffi_file_eof  :: T_Int64 -> T_Int64

  -- Everything that handles Strings are FFI implemented now.
  ffi pure
    -- String
    _prim_string_concat  :: T_String -> T_String -> T_String
    _prim_string_reverse :: T_String -> T_String
    _prim_string_lt      :: T_String -> T_String -> T_Int64
    _prim_string_eq      :: T_String -> T_String -> T_Int64
    _prim_string_head    :: T_String -> T_Int64 -- TODO: Change to Char
    _prim_string_tail    :: T_String -> T_String
    _prim_string_cons    :: T_Int64  -> T_String -> T_String
    _prim_string_len     :: T_String -> T_Int64

  ffi pure
    -- Conversion
    _prim_int_str       :: T_Int64  -> T_String
    _prim_str_int       :: T_String -> T_Int64
    _prim_int_float     :: T_Int64  -> T_Float
    _prim_float_string  :: T_Float  -> T_String
    _prim_int_double    :: T_Int64  -> T_Double
    _prim_double_string :: T_Double -> T_String
    _prim_char_int      :: T_Char   -> T_Int64
    _prim_double_int    :: T_Double -> T_Int64
    _prim_string_double :: T_String -> T_Double

  primop pure
    -- Int
    _prim_int_shr   :: T_Int64 -> T_Int64 -- TODO: Remove?
    _prim_int_add   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_sub   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_mul   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_div   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_ashr  :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_lshr  :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_shl   :: T_Int64 -> T_Int64 -> T_Int64
    _prim_int_eq    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_ne    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_gt    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_ge    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_lt    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_le    :: T_Int64 -> T_Int64 -> T_Bool
    _prim_int_and   :: T_Int64 -> T_Int64 -> T_Int64

    -- Word
    _prim_word_add  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_sub  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_mul  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_div  :: T_Word64 -> T_Word64 -> T_Word64
    _prim_word_eq   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_ne   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_gt   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_ge   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_lt   :: T_Word64 -> T_Word64 -> T_Bool
    _prim_word_le   :: T_Word64 -> T_Word64 -> T_Bool

    -- Float
    _prim_float_add :: T_Float -> T_Float -> T_Float
    _prim_float_sub :: T_Float -> T_Float -> T_Float
    _prim_float_mul :: T_Float -> T_Float -> T_Float
    _prim_float_div :: T_Float -> T_Float -> T_Float
    _prim_float_eq  :: T_Float -> T_Float -> T_Bool
    _prim_float_ne  :: T_Float -> T_Float -> T_Bool
    _prim_float_gt  :: T_Float -> T_Float -> T_Bool
    _prim_float_ge  :: T_Float -> T_Float -> T_Bool
    _prim_float_lt  :: T_Float -> T_Float -> T_Bool
    _prim_float_le  :: T_Float -> T_Float -> T_Bool

    -- Double
    _prim_double_add :: T_Double -> T_Double -> T_Double
    _prim_double_sub :: T_Double -> T_Double -> T_Double
    _prim_double_mul :: T_Double -> T_Double -> T_Double
    _prim_double_div :: T_Double -> T_Double -> T_Double
    _prim_double_eq  :: T_Double -> T_Double -> T_Bool
    _prim_double_ne  :: T_Double -> T_Double -> T_Bool
    _prim_double_gt  :: T_Double -> T_Double -> T_Bool
    _prim_double_ge  :: T_Double -> T_Double -> T_Bool
    _prim_double_lt  :: T_Double -> T_Double -> T_Bool
    _prim_double_le  :: T_Double -> T_Double -> T_Bool
    _prim_double_log :: T_Double -> T_Double


    -- Bool
    _prim_bool_eq   :: T_Bool -> T_Bool -> T_Bool
    _prim_bool_ne   :: T_Bool -> T_Bool -> T_Bool

|]

withPrimPrelude :: Program -> Program
withPrimPrelude p = concatPrograms [primPrelude, p]
