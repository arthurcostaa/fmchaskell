module Nat where

import Prelude hiding
  ( div,
    max,
    min,
    pred,
    (*),
    (+),
    (-),
    (<),
    (^),
  )

data Nat = O | S Nat
  deriving (Eq, Show)

-- Addition
(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

-- "Monus"
(-) :: Nat -> Nat -> Nat
O - n = O
n - O = n
(S n) - (S m) = n - m

-- Multiplication
(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = n + (n * m)

-- Exponentiation
(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ S m = n * (n ^ m)

-- Double
double :: Nat -> Nat
double O = O
double (S n) = S (S (double n))

-- Fibonacci
fib :: Nat -> Nat
fib O = O
fib (S O) = S O
fib (S (S n)) = fib n + fib (S n)

-- Lucas Number
luc :: Nat -> Nat
luc O = S (S O)
luc (S O) = S O
luc (S (S n)) = luc n + luc (S n)

-- Factorial
fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

-- Predecessor
pred :: Nat -> Nat
pred O = O
pred (S n) = n

-- Min
min :: Nat -> Nat -> Nat
min n O = O
min O n = O
min (S n) (S m) = S (min n m)

-- Max
max :: Nat -> Nat -> Nat
max n O = n
max O n = n
max (S n) (S m) = S (max n m)

comb :: Nat -> Nat -> Nat
comb n O = S O
comb O r = O
comb n r = comb (pred n) (pred r) + comb (pred n) r