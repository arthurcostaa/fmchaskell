module ListAlpha where

import Nat
import Bool
import Prelude hiding
    ( replicate
    , map
    )

replicate :: Nat -> alpha -> [alpha]
replicate O _ = []
replicate (S n) a = a : replicate n a

map :: (alpha -> beta) -> [alpha] -> [beta]
map f [] = []
map f (x : xs) = f x : map f xs
