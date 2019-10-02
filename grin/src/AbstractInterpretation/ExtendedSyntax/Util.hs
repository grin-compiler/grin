{-# LANGUAGE LambdaCase, RecordWildCards #-}
module AbstractInterpretation.ExtendedSyntax.Util where

converge :: (a -> a -> Bool) -> (a -> a) -> a -> a
converge pred f x
  | pred x x' = x
  | otherwise = converge pred f x'
  where x' = f x
