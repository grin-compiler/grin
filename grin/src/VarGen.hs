{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module VarGen where

import Data.List
import Numeric.Natural

import Grin


type V = String
newtype VP = VP [String]

class VarGen vg where
  type VGExt vg :: *
  branchVar     :: Int -> vg -> vg
  branch        :: String -> vg -> vg
  newVarName    :: vg -> Exp -> Maybe (VGExt vg)
  newVarGen     :: vg
  extendVarGen  :: VGExt vg -> vg -> vg
  vars          :: vg       -> [V]

instance VarGen VP where
  type VGExt VP = String
  branchVar i (VP xs)    = VP $ ("b" ++ show i):xs
  branch    n (VP xs)    = VP $ (n:xs)
  newVarName _           = newVarNameVP
  newVarGen              = VP []
  extendVarGen p (VP ps) = VP (p:ps)
  vars (VP ps)           = map (\i -> intercalate "." (show i:ps)) [1..]

newVarNameVP :: Exp -> Maybe String
newVarNameVP = \case
  Def         name names exp     -> Just name
  SApp        name simpleVals    -> Just "a"
  SReturn     val                -> Just "r"
  SStore      val                -> Just "s"
  SUpdate     name val           -> Just name
  _                              -> Nothing


newtype VPM s = VPM ([String], Natural)

s2n :: forall s . (Bounded s, Enum s) => s -> Natural
s2n s = fromIntegral $
  ((fromIntegral . fromEnum) s             :: Integer) -
  ((fromIntegral . fromEnum) (minBound @s) :: Integer)

n2s :: forall s . (Bounded s, Enum s) => Natural -> s
n2s n = toEnum . fromIntegral $
  (fromIntegral n + (fromIntegral . fromEnum) (minBound @s) :: Integer)

base :: forall s . (Bounded s, Enum s) => Natural
base = s2n (maxBound @s) + 1

data Iso a b = Iso
  { to :: a -> b
  , from :: b -> a
  }

encode :: Iso (s, Natural) Natural -> [s] -> Natural
encode (Iso to _) = foldl' (\acc s -> to (s, acc)) 1

decode :: (Show s) => Iso (s, Natural) Natural -> Natural -> [s]
decode (Iso _ from) = reverse . unfoldr
  (\n ->
    if n <= 1
      then Nothing
      else Just $ from n
  )

std_iso :: forall s . (Bounded s, Enum s, Show s) => Iso (s, Natural) Natural
std_iso = Iso (\(s,n) -> s2n s + base @s * n) (\n -> (n2s $ n `mod` base @s, n `div` (base @s)))


instance (Bounded s, Enum s, Show s) => VarGen (VPM s) where
  type VGExt (VPM s) = [s]
  branchVar i             = extendVarGen (decode std_iso $ fromIntegral i)
  branch u (VPM (us, n))  = VPM (u:us, n)
  newVarName _            = newVarNameVPM
  newVarGen               = VPM ([],1)
  extendVarGen xs (VPM (us,n)) = VPM $ (us, n + encode std_iso xs)
  vars (VPM (us, v))           = map (\i -> concat $ us ++ [".",show i, ".", show v]) [1..]

newVarNameVPM :: (Bounded s, Enum s, Show s) => Exp -> Maybe [s]
newVarNameVPM = \case
  Def         name names exp     -> Just $ decode std_iso 1
  SApp        name simpleVals    -> Just $ decode std_iso 2
  SReturn     val                -> Just $ decode std_iso 3
  SStore      val                -> Just $ decode std_iso 4
  SUpdate     name val           -> Just $ decode std_iso 5
  _                              -> Nothing

modBase = 10

newtype Mod10 = Mod10 { n :: Integer }
  deriving (Eq, Show)

instance Num Mod10 where
  fromInteger n = Mod10 (mod n modBase)
  (Mod10 x) + (Mod10 y) = Mod10 $ mod (x + y) modBase
  (Mod10 x) - (Mod10 y) = Mod10 $ mod (x - y) modBase
  (Mod10 x) * (Mod10 y) = Mod10 $ mod (x * y) modBase
  abs    = id
  signum = const 1

instance Enum Mod10 where
  toEnum             = fromIntegral
  fromEnum (Mod10 x) = fromIntegral x

instance Bounded Mod10 where
  minBound = fromInteger 0
  maxBound = fromInteger (modBase - 1)
