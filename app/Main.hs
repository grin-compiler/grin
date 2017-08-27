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

import Control.Monad.Gen
import Text.PrettyPrint.ANSI.Leijen (pretty)


{-
  TODO
    add Def to shape functor
-}

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

type GenM = Gen Integer

vectorisation :: Exp -> Exp
vectorisation = runGen . cata folder where
  folder :: ExpF (GenM Exp) -> GenM Exp
  folder = \case
    EBindF simpleexp (Var name) exp -> do
      let newName = ('_' :) . show <$> gen
      tag <- newName
      args <- map Var <$> replicateM 3 newName
      EBind <$> simpleexp <*> pure (VarTagNode tag args) <*> exp
    e -> embed <$> sequence e

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
      putStrLn . show . pretty . vectorisation $ Program grin
      printGrin grin
