module ListNat where

import Nat
import Bool
import Ordering
import Prelude hiding
    ( (+)
    , (*)
    , (++)
    , (^)
    , (>)
    , (>=)
    , (<)
    , (<=)
    , min
    , max
    , length
    , elem
    , sum
    , product
    , reverse
    , enumFromTo
    , head
    , tail
    , init
    , last
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

reverse :: ListNat -> ListNat
reverse [] = []
reverse (x : xs) = (reverse xs) ++ [x]

allEven :: ListNat -> Bool
allEven [] = True
allEven (x : xs) = (ev x) && (allEven xs)

anyEven :: ListNat -> Bool
anyEven [] = False
anyEven (x : xs) = (ev x) || (anyEven xs)

allOdd :: ListNat -> Bool
allOdd [] = True
allOdd (x : xs) = (od x) && (allOdd xs)

anyOdd :: ListNat -> Bool
anyOdd [] = False
anyOdd (x : xs) = (od x) || (anyOdd xs)

allZero :: ListNat -> Bool
allZero [] = True
allZero (x : xs) = (isZero x) && (allZero xs)

anyZero :: ListNat -> Bool
anyZero [] = False
anyZero (x : xs) = (isZero x) || (anyZero xs)

addNat :: Nat -> ListNat -> ListNat
addNat _ [] = []
addNat n (x : xs) = (n + x) : (addNat n xs)

mulNat :: Nat -> ListNat -> ListNat
mulNat _ [] = []
mulNat n (x : xs) = (n * x) : (mulNat n xs)

expNat :: Nat -> ListNat -> ListNat
expNat _ [] = []
expNat n (x : xs) = (n ^ x) : (expNat n xs)

enumFromTo :: Nat -> Nat -> ListNat
enumFromTo n m
    | n > m = []
    | otherwise = n : enumFromTo (S n) m

enumTo :: Nat -> ListNat
enumTo n = enumFromTo O n

head :: ListNat -> Nat
head [] = error "Empty List has no head"
head (x : xs) = x

tail :: ListNat -> ListNat
tail [] = error "Empty List has no tail"
tail (x : xs) = xs

init :: ListNat -> ListNat
init [] = error "Empty List has no init"
init [_] = []
init (x : xs) = x : init xs

last :: ListNat -> Nat
last [] = error "Empty List has no last"
last [n] = n
last (x : xs) = last xs
