{-# LANGUAGE RankNTypes, TypeApplications, NamedFieldPuns, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
module FreshNames where

import Grin

import Data.Ord
import Data.List
import Numeric.Natural

import Control.Comonad.Cofree as Cofree
import Data.Functor.Foldable as Foldable


nodeCode :: Exp -> Cofree ExpF Natural
nodeCode = varCode . path . annotate

data N = N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10 | N11
  deriving (Enum, Eq, Show, Bounded)

annotate :: Exp -> Cofree ExpF N
annotate = cata folder where
  folder :: ExpF (Cofree ExpF N) -> Cofree ExpF N
  folder = \case
    ProgramF     defs               -> N1  :< ProgramF defs
    DefF         name names exp     -> N2  :< DefF name names exp
    EBindF       simpleExp lpat exp -> N3  :< EBindF simpleExp lpat exp
    ECaseF       val alts           -> N4  :< ECaseF val alts
    SAppF        name simpleVals    -> N5  :< SAppF name simpleVals
    SReturnF     val                -> N6  :< SReturnF val
    SStoreF      val                -> N7  :< SStoreF  val
    SFetchF      name               -> N8  :< SFetchF  name
    SUpdateF     name val           -> N9  :< SUpdateF name val
    SBlockF      exp                -> N10 :< SBlockF exp
    AltF         cpat exp           -> N11 :< AltF cpat exp

path :: Cofree ExpF a -> Cofree ExpF [a]
path cf = Cofree.unfold builder ([], cf) where
  builder :: ([a], Cofree ExpF a) -> ([a], ExpF ([a], Cofree ExpF a))
  builder (acc, a :< expf) = (a:acc, (,) (a:acc) <$> expf)

varCode :: forall s . (Bounded s, Enum s) => Cofree ExpF [s] -> Cofree ExpF Natural
varCode = fmap (encode std_iso)

-- TODO: Better probabilites
varCodeSlow :: forall s . (Show s, Eq s, Bounded s, Enum s) => Cofree ExpF [s] -> Cofree ExpF Natural
varCodeSlow = fmap (encode (ans_iso $ classify_prob (replicate 11 (1.0 / 11))))

-- * Number systems

{- https://ro-che.info/articles/2017-08-20-understanding-ans -}

data Iso a b = Iso
  { to :: a -> b
  , from :: b -> a
  }

encode :: Iso (s, Natural) Natural -> [s] -> Natural
encode Iso{to} = foldl' (\acc s -> to (s, acc)) 1

decode :: Iso (s, Natural) Natural -> Natural -> [s]
decode Iso{from} = reverse . unfoldr
  (\n ->
    if n == 1
      then Nothing
      else Just $ from n
  )

std_iso :: forall s . (Bounded s, Enum s) => Iso (s, Natural) Natural
std_iso = Iso (\(s,n) -> s2n s + base @s * n) (\n -> (n2s $ n `mod` base @s, n `div` base @s))

s2n :: forall s . (Bounded s, Enum s) => s -> Natural
s2n s = fromIntegral $
  ((fromIntegral . fromEnum) s             :: Integer) -
  ((fromIntegral . fromEnum) (minBound @s) :: Integer)

n2s :: forall s . (Bounded s, Enum s) => Natural -> s
n2s n = toEnum . fromIntegral $
  (fromIntegral n + (fromIntegral . fromEnum) (minBound @s) :: Integer)

base :: forall s . (Bounded s, Enum s) => Natural
base = s2n (maxBound @s) + 1

classify_mod_base :: forall s . (Bounded s, Enum s) => (Natural, Natural -> s)
classify_mod_base = (base @s, \n -> n2s (n `mod` base @s))

-- * Assymetric Number System

ans_iso :: forall s . Eq s => (Natural, Natural -> s) -> Iso (s, Natural) Natural
ans_iso (b, classify) = Iso{to, from} where
  to :: (s, Natural) -> Natural
  to (s, n) = [ k | k <- [b..], classify k == s ] `genericIndex` (n-1)

  from :: Natural -> (s, Natural)
  from n =
    let s = classify n
        n' = genericLength [ () | k <- [b..n], classify k == s ]
    in (s, n')

classify_prob :: Show s => (Bounded s, Enum s) => [Double] -> (Natural, Natural -> s)
classify_prob probs =
  let beta = 2 -- arbitrary number > 1
      t = genericLength l
      l = concatMap (\(s, t) -> replicate t s)
        . sortBy (comparing (Down . snd))
        . zip [minBound..maxBound]
        $ map (round . (/ minimum probs)) probs
      g1 n = l `genericIndex` ((n-beta) `mod` t)
  in (beta, g1)
