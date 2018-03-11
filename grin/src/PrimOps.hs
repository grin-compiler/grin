module PrimOps where

import Data.Map.Strict as Map


data PrimType
  = TInt
  | TWord
  | TFloat
  | TBool
  | TUnit
  deriving (Eq, Show)

checkName :: String -> b -> b
checkName name = maybe (error $ "primOp is not defined:" ++ name) (const id) $ Map.lookup name primOps


-- TODO: Record effects
primOps :: Map String ([PrimType], PrimType)
primOps = Map.fromList $
  [ ("_prim_int_print", ([TInt], TInt)) -- HINT: this primop is adhoc, will be removed
  -- Int
  , ("_prim_int_add", ([TInt, TInt], TInt))
  , ("_prim_int_sub", ([TInt, TInt], TInt))
  , ("_prim_int_mul", ([TInt, TInt], TInt))
  , ("_prim_int_div", ([TInt, TInt], TInt))
  , ("_prim_int_eq", ([TInt, TInt], TBool))
  , ("_prim_int_ne", ([TInt, TInt], TBool))
  , ("_prim_int_gt", ([TInt, TInt], TBool))
  , ("_prim_int_ge", ([TInt, TInt], TBool))
  , ("_prim_int_lt", ([TInt, TInt], TBool))
  , ("_prim_int_le", ([TInt, TInt], TBool))
  -- Word
  , ("_prim_word_add", ([TWord, TWord], TWord))
  , ("_prim_word_sub", ([TWord, TWord], TWord))
  , ("_prim_word_mul", ([TWord, TWord], TWord))
  , ("_prim_word_div", ([TWord, TWord], TWord))
  , ("_prim_word_eq", ([TWord, TWord], TBool))
  , ("_prim_word_ne", ([TWord, TWord], TBool))
  , ("_prim_word_gt", ([TWord, TWord], TBool))
  , ("_prim_word_ge", ([TWord, TWord], TBool))
  , ("_prim_word_lt", ([TWord, TWord], TBool))
  , ("_prim_word_le", ([TWord, TWord], TBool))
  -- Float
  , ("_prim_float_add", ([TFloat, TFloat], TFloat))
  , ("_prim_float_sub", ([TFloat, TFloat], TFloat))
  , ("_prim_float_mul", ([TFloat, TFloat], TFloat))
  , ("_prim_float_div", ([TFloat, TFloat], TFloat))
  , ("_prim_float_eq", ([TFloat, TFloat], TBool))
  , ("_prim_float_ne", ([TFloat, TFloat], TBool))
  , ("_prim_float_gt", ([TFloat, TFloat], TBool))
  , ("_prim_float_ge", ([TFloat, TFloat], TBool))
  , ("_prim_float_lt", ([TFloat, TFloat], TBool))
  , ("_prim_float_le", ([TFloat, TFloat], TBool))
  -- Bool
  , ("_prim_bool_eq", ([TBool, TBool], TBool))
  , ("_prim_bool_ne", ([TBool, TBool], TBool))
  ]
