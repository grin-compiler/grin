{-# language TypeFamilies #-}
module Control.DeeperSeq where

import Foreign
import Control.DeepSeq

type family Result a where
    Result (IO a)   = Result a
    Result (b -> a) = Result a
    Result a        = a

type family SetResult a b where
    SetResult x (IO a)   = IO (SetResult x a)
    SetResult x (b -> a) = b -> SetResult x a
    SetResult x y        = x

class (SetResult (Result a) a ~ a) => MapResult a where
    mapResult :: (Result a -> b) -> a -> SetResult b a

instance MapResult Char     where mapResult = id
instance MapResult Double   where mapResult = id
instance MapResult Float    where mapResult = id
instance MapResult Bool     where mapResult = id
instance MapResult Int      where mapResult = id
instance MapResult Int8     where mapResult = id
instance MapResult Int16    where mapResult = id
instance MapResult Int32    where mapResult = id
instance MapResult Int64    where mapResult = id
instance MapResult Word     where mapResult = id
instance MapResult Word8    where mapResult = id
instance MapResult Word16   where mapResult = id
instance MapResult Word32   where mapResult = id
instance MapResult Word64   where mapResult = id
instance MapResult (Ptr a)          where mapResult = id
instance MapResult (FunPtr a)       where mapResult = id
instance MapResult (StablePtr a)    where mapResult = id
instance MapResult ()               where mapResult = id
instance MapResult (a, b)           where mapResult = id
instance MapResult (a, b, c)        where mapResult = id
instance MapResult (a, b, c, d)     where mapResult = id

instance MapResult b => MapResult (a -> b)  where mapResult = fmap . mapResult
instance MapResult a => MapResult (IO a)    where mapResult = fmap . mapResult

deeperSeq :: (NFData a, MapResult b) => a -> b -> b
deeperSeq a b = mapResult (deepseq a) b

