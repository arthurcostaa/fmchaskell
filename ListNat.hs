module ListNat where

import Bool (ev, isZero, od)
import Nat (Nat (..), (*), (+), (^))
import Ordering ((<), (>))
import Prelude hiding
  ( drop,
    elem,
    enumFromTo,
    head,
    init,
    last,
    length,
    max,
    maximum,
    min,
    minimum,
    product,
    replicate,
    reverse,
    sum,
    tail,
    take,
    (*),
    (+),
    (++),
    (<),
    (<=),
    (>),
    (>=),
    (^),
  )

-- data ListNat = Nil | Cons Nat ListNat
--    deriving ( Eq, Show )
type ListNat = [Nat]

length :: ListNat -> Nat
length [] = O
length (x : xs) = S (length xs)

elem :: Nat -> ListNat -> Bool
elem _ [] = False
-- elem n (x : xs) = if n == x then True else elem n xs
elem n (x : xs)
  | n == x = True
  | otherwise = elem n xs

sum :: ListNat -> Nat
sum [] = O
sum (x : xs) = x + sum xs

product :: ListNat -> Nat
product [] = S O
product (x : xs) = x * product xs

(++) :: ListNat -> ListNat -> ListNat
[] ++ xs = xs
(x : xs) ++ ys = x : (xs ++ ys)

reverse :: ListNat -> ListNat
reverse [] = []
-- reverse (x : xs) = (reverse xs) ++ [x]
reverse (x : xs) = append x (reverse xs)

allEven :: ListNat -> Bool
allEven [] = True
allEven (x : xs) = (ev x) && allEven xs

anyEven :: ListNat -> Bool
anyEven [] = False
anyEven (x : xs) = (ev x) || anyEven xs

allOdd :: ListNat -> Bool
allOdd [] = True
allOdd (x : xs) = (od x) && allOdd xs

anyOdd :: ListNat -> Bool
anyOdd [] = False
anyOdd (x : xs) = (od x) || anyOdd xs

allZero :: ListNat -> Bool
allZero [] = True
allZero (x : xs) = (isZero x) && allZero xs

anyZero :: ListNat -> Bool
anyZero [] = False
anyZero (x : xs) = (isZero x) || anyZero xs

addNat :: Nat -> ListNat -> ListNat
addNat _ [] = []
addNat n (x : xs) = (n + x) : addNat n xs

mulNat :: Nat -> ListNat -> ListNat
mulNat _ [] = []
mulNat n (x : xs) = (n * x) : mulNat n xs

expNat :: Nat -> ListNat -> ListNat
expNat _ [] = []
expNat n (x : xs) = (n ^ x) : expNat n xs

enumFromTo :: Nat -> Nat -> ListNat
enumFromTo n m
  | n > m = []
  | otherwise = n : enumFromTo (S n) m

enumTo :: Nat -> ListNat
enumTo n = enumFromTo O n

take :: Nat -> ListNat -> ListNat
take _ [] = []
take O (_ : _) = []
take (S n) (x : xs) = x : take n xs

drop :: Nat -> ListNat -> ListNat
drop _ [] = []
drop O (x : xs) = x : xs
drop (S n) (x : xs) = drop n xs

pwAdd :: ListNat -> ListNat -> ListNat
pwAdd (x : xs) (y : ys) = (x + y) : pwAdd xs ys
pwAdd _ _ = []

pwMul :: ListNat -> ListNat -> ListNat
pwMul (x : xs) (y : ys) = (x * y) : pwMul xs ys
pwMul _ _ = []

pwExp :: ListNat -> ListNat -> ListNat
pwExp (x : xs) (y : ys) = (x ^ y) : pwExp xs ys
pwExp _ _ = []

filterEven :: ListNat -> ListNat
filterEven [] = []
-- filterEven (x : xs) = if ev x then x : filterEven xs else filterEven xs
filterEven (x : xs)
  | ev x = x : filterEven xs
  | otherwise = filterEven xs

filterOdd :: ListNat -> ListNat
filterOdd [] = []
-- filterOdd (x : xs) = if od x then x : filterOdd xs else filterOdd xs
filterOdd (x : xs)
  | od x = x : filterOdd xs
  | otherwise = filterOdd xs

elemIndices :: Nat -> ListNat -> ListNat
elemIndices _ [] = []
elemIndices n (x : xs)
  | n == x = O : addNat (S O) (elemIndices n xs)
  | otherwise = addNat (S O) (elemIndices n xs)

isSorted :: ListNat -> Bool
isSorted (n : (m : ms)) = n < m && isSorted (m : ms)
isSorted _ = True

minimum :: ListNat -> Nat
minimum [] = error "Empty List has no minimum"
minimum [x] = x
minimum (x : (y : ys)) = if (x < y) && (x < minimum (y : ys)) then x else minimum (y : ys)

maximum :: ListNat -> Nat
maximum [] = error "Empty List has no maximum"
maximum [x] = x
maximum (x : (y : ys)) = if (x > y) && (x > maximum (y : ys)) then x else maximum (y : ys)

isPrefixOf :: ListNat -> ListNat -> Bool
isPrefixOf [] [] = True
isPrefixOf [] (x : xs) = True
isPrefixOf (x : xs) [] = False
isPrefixOf (x : xs) (y : ys) = (x == y) && isPrefixOf xs ys

mix :: ListNat -> ListNat -> ListNat
mix (x : xs) (y : ys) = x : (y : mix xs ys)
mix _ _ = []

interspace :: Nat -> ListNat -> ListNat
interspace _ [x] = [x]
interspace n (x : xs) = x : (n : interspace n xs)
interspace _ [] = []

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

countdown :: Nat -> ListNat
countdown O = [O]
countdown (S n) = S n : countdown n

stretch :: Nat -> ListNat -> ListNat
stretch n (x : xs) = replicate n x ++ stretch n xs
stretch _ [] = []

append :: Nat -> ListNat -> ListNat
append n (x : xs) = x : append n xs
append n [] = [n]

replicate :: Nat -> Nat -> ListNat
replicate O _ = []
replicate (S n) m = m : replicate n m
