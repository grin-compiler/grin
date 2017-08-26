{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import System.Environment
import Text.Printf

import Eval
import ParseGrin
import Grin
import Pretty
import Data.Functor.Foldable as Foldable

testCata :: Exp -> Int
testCata = cata folder where
  folder = \case
    EBindF    a _ b -> a + b
    ECaseF    _ a   -> sum a
    -- Simple Expr
    SAppF     {}    -> 0
    SReturnF  {}    -> 0
    SStoreF   {}    -> 1
    SFetchF   {}    -> 0
    SUpdateF  {}    -> 0
    SBlockF   a     -> a
    -- Alt
    AltF _ a        -> a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: grin GRIN_SOURCE"
    --x   -> forM_ x $ \a -> eval' PureReducer a >>= print
    x -> forM_ x $ \fname -> do
      grin <- either (fail . show) id <$> parseGrin fname
      let result = [printf "stores %s %d" name $ testCata exp | Def name _ exp <- grin]
      putStrLn $ unlines result
      printGrin grin
