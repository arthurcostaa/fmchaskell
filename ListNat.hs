module ListNat where

import Nat
import Bool
import Prelude hiding
    ( (+)
    , (*)
    , (++)
    , Bool
    , True
    , False
    , length
    , elem
    , sum
    , product
    )

--data ListNat = Nil | Cons Nat ListNat
--    deriving ( Eq, Show )
type ListNat = [Nat]


length :: ListNat -> Nat
length [] = O
length (x : xs) = S(length xs)

elem :: Nat -> ListNat -> Bool
elem _ [] = False
elem n (x : xs) = if n == x then True else elem n xs

sum :: ListNat -> Nat
sum [] = O
sum (x : xs) = x + sum xs

product :: ListNat -> Nat
product [] = (S O)
product (x : xs) = x * product xs

(++) :: ListNat -> ListNat -> ListNat
[] ++ xs = xs
(n : ns) ++ ms = n : (ns ++ ms)
