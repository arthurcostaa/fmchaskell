module Bool where

import Nat
import Prelude hiding (not)

-- data Bool = False | True
--     deriving ( Eq, Show )

not :: Bool -> Bool
not True = False
not False = True

leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S (S n)) = ev n

od :: Nat -> Bool
od n = not (ev n)

isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S (S (S n))) = isMul3 n
isMul3 _ = False

isZero :: Nat -> Bool
isZero O = True
isZero _ = False
