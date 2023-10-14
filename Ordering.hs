module Ordering where

import Nat
import Bool
import Prelude hiding
    ( (>)
    , (<)
    , (>=)
    , (<=)
    , compare
    )

compare :: Nat -> Nat -> Ordering
compare O O = EQ
compare O _ = LT
compare _ O = GT
compare (S n) (S m) = compare n m

(>) :: Nat -> Nat -> Bool
n > m = (compare n m) == GT

(>=) :: Nat -> Nat -> Bool
n >= m = (n > m) || (n == m)

(<) :: Nat -> Nat -> Bool
n < m = (compare n m) == LT

(<=) :: Nat -> Nat -> Bool
n <= m = (n < m) || (n == m)