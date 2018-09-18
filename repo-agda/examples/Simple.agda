
data Nat : Set where
  Z : Nat
  S : Nat -> Nat

_+_ : Nat -> Nat -> Nat
x + Z = x
Z + y = y
S x + y = S (x + y)
