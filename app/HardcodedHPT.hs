module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Data.Set (Set)
import qualified Data.Set as Set

type VarName = String
type ConName = String

data Val
  = BAS
  | Loc     Int
  | Node    ConName [Vals]
  | Var     VarName
  | Union   Vals Vals
  | Select  VarName ConName Int
  | Eval    Val
  | Fetch   VarName
  deriving (Eq, Ord, Show)

type Vals = Set Val

infixr  6 ===
infixr  7 <>

a <> b = Set.singleton (Union a b)
key === value = (key, value)

-- operations
select :: Vals -> String -> Int -> Vals
select vals name idx = Set.unions [xs !! (idx - 1) | Node n xs <- Set.toList vals, n == name]
{-
fetch :: Vals -> Vals
fetch a = Set.unions [heap !! (idx - 1) | Loc idx <- Set.toList a]
-}
eval :: Vals -> Vals
eval = id

bas = Set.singleton BAS
loc = Set.singleton . Loc
var = Set.singleton . Var

-- Abstract environment equations
env :: Map VarName Vals
env = Map.fromList
  [ "t1"  === loc 1
  , "t2"  === loc 2
  , "t3"  === loc 3
  , "t4"  === loc 4
  , "r'"  === bas

  , "m"   === var "t1" <> var "m1"
  , "n"   === var "t2" <> var "n"
  , "m'"  === bas
  , "n'"  === bas
  , "b'"  === bas
  , "m1'" === bas
  , "m1"  === loc 5
  , "p"   === loc 6
  , "l"   === var "t3" <> var "xs"

  , "x"   === Set.fromList [Select "l2" "CCons" 1]
  , "xs"  === Set.fromList [Select "l2" "CCons" 2]

  , "x'"  === bas
  , "s'"  === bas
  , "ax'" === bas
  , "ru"  === Set.fromList [Node "CNil" [], Node "CCons" [var "m", var "p"]]
  , "rs"  === Set.fromList [Node "CInt" [bas]]

  , "l2"  === Set.fromList [Eval $ Fetch "l"]
  ]

-- Abstract heap equations
heap :: IntMap Vals
heap = IntMap.fromList
  [ 1 === Set.fromList [Node "CInt" [bas]]
  , 2 === Set.fromList [Node "CInt" [bas]]
  , 3 === Set.fromList [Node "Fupto" [var "t1", var "t2"]] <> var "ru"
  , 4 === Set.fromList [Node "Fsum" [var "t3"]] <> var "rs"
  , 5 === Set.fromList [Node "CInt" [bas]]
  , 6 === Set.fromList [Node "Fupto" [var "m1", var "n"]] <> var "ru"
  ]

main :: IO ()
main = do
  putStrLn "Env"
  print env

  putStrLn "Heap"
  print heap
