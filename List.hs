module List where

import Bool ()
import ListNat hiding (pwAdd, pwExp, pwMul, replicate, stretch, (++))
import Nat (Nat (..), (*), (+))
import Prelude hiding
  ( all,
    any,
    dropWhile,
    filter,
    map,
    replicate,
    takeWhile,
    zip,
    (*),
    (+),
    (++),
  )

replicate :: Nat -> a -> [a]
replicate O _ = []
replicate (S n) x = x : replicate n x

-- map (Nat.+ (S O)) [O, (S O), (S (S O))]
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

-- filter Bool.ev [O, (S O), (S (S O))]
filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

all :: (a -> Bool) -> [a] -> Bool
all f [] = True
all f (x : xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any f [] = False
any f (x : xs) = f x || any f xs

-- Example: pointwise (Nat.+) (ListNat.enumFromTo (S (S (S O))) (S (S (S (S (S (S (S O)))))))) (ListNat.enumTo (S (S (S (S (S (S (S (S (S (S (S O))))))))))))
pointwise :: (a -> b -> c) -> [a] -> [b] -> [c]
pointwise f (x : xs) (y : ys) = f x y : pointwise f xs ys
pointwise f _ _ = []

-- Example: fold (Nat.+) O [O, (S O), (S(S O)), (S(S(S O)))]
fold :: (a -> a -> a) -> a -> [a] -> a
fold f x [] = x
fold f x (y : ys) = f y (fold f x ys)

-- Example: takeWhile (Bool.ev) [(S(S O)), (S(S(S(S O)))), (S O)]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile f [] = []
takeWhile f (x : xs)
  | f x = x : takeWhile f xs
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile f [] = []
dropWhile f (x : xs)
  | f x = dropWhile f xs
  | otherwise = x : dropWhile f xs

fsum :: ListNat -> Nat
fsum [] = O
fsum (x : xs) = fold (+) O (x : xs)

fproduct :: ListNat -> Nat
fproduct [] = S O
fproduct (x : xs) = fold (*) (S O) (x : xs)

(++) :: [a] -> [a] -> [a]
[] ++ xs = xs
(x : xs) ++ ys = x : (xs ++ ys)

stretch :: Nat -> [a] -> [a]
stretch n (x : xs) = replicate n x ++ stretch n xs
stretch _ [] = []

zip :: [a] -> [b] -> [(a, b)]
zip = pointwise (,)

-- zip = pointwise (\x y -> (x, y))

-- zip = pointwise zip'
--   where
--     zip' :: a -> b -> (a, b)
--     zip' a b = (a, b)
