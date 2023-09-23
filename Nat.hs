module Nat where

import Prelude hiding
    ( (+)
    , (*)
    , (^)
    )

data Nat = O | S Nat
    deriving ( Eq , Show )

-- Addition
(+) :: Nat -> Nat -> Nat
(+) n O = n
(+) n (S m) = S((+) n m)

-- Multiplication
(*) :: Nat -> Nat -> Nat
(*) n O = O
(*) n (S m) = (+) ((*) n m) n

-- Exponentiation
(^) :: Nat -> Nat -> Nat
(^) n O = (S O)
(^) n (S m) = (*) ((^) n m) n

-- Double
double :: Nat -> Nat
double O = O
double (S n)  = (S (S (double n)))

-- Fibonacci
fib :: Nat -> Nat
fib O = O
fib (S O) = (S O)
fib (S (S n)) = (+) (fib n) (fib (S n))

-- Factorial
fact :: Nat -> Nat
fact O = (S O)
fact (S n) = (*) (S n) (fact n)
