module Signed
where

  import Natural

  data Signed a = Pos a | Neg a 
    deriving (Show, Eq)

  instance (Ord a) => Ord (Signed a) where
    compare (Pos a) (Pos b) = compare a b
    compare (Neg a) (Neg b) = compare b a
    compare (Pos a) (Neg b) = GT
    compare (Neg a) (Pos b) = LT

  instance (Enum a) => Enum (Signed a) where
    toEnum i | i >= 0 = Pos $ toEnum i
             | i < 0  = Neg $ toEnum i
    fromEnum (Pos a)  = fromEnum a
    fromEnum (Neg a)  = negate (fromEnum a)

  instance (Ord a, Num a) => Num (Signed a) where
    (Pos a) + (Pos b) = Pos (a + b)
    (Neg a) + (Neg b) = Neg (a + b)
    (Pos a) + (Neg b) | a >= b  = Pos (a - b)
                      | a < b   = Neg (b - a)
    (Neg a) + (Pos b) | a >  b  = Neg (a - b)
                      | a == b  = Pos (a - b) -- 0
                      | a <  b  = Pos (b - a)
    (Pos a) - (Pos b) | a >  b  = Pos (a - b)
                      | a <  b  = Neg (b - a)
                      | a == b  = Pos (a - b)
    (Neg a) - (Neg b) | a >  b  = Neg (a - b)
                      | a == b  = Pos (a - b)
                      | a <  b  = Pos (b - a)
    (Neg a) - (Pos b) | signum b == 0 = Neg a
                      | otherwise     = (Neg a) + (Neg b)
    (Pos a) - (Neg b) = (Pos a) + (Pos b)
    (Pos a) * (Pos b) = Pos (a * b)
    (Neg a) * (Neg b) = Pos (a * b)
    (Pos a) * (Neg b) = Neg (a * b)
    (Neg a) * (Pos b) = Neg (a * b)
    negate (Pos a) | signum a == 0 = Pos a
                   | otherwise     = Neg a
    negate (Neg a)    = Pos a
    abs    (Pos a)    = Pos a
    abs    (Neg a)    = Pos a
    signum (Pos a)    = Pos (signum a)
    signum (Neg a)    = Neg (signum $ negate a)
    fromInteger i     | i >= 0 = Pos (fromInteger i)
                      | i <  0 = Neg (fromInteger $ abs i)

  instance (Real a) => Real (Signed a) where
    toRational (Pos a) = toRational a
    toRational (Neg a) = negate (toRational a)

  instance (Enum a, Integral a) => Integral (Signed a) where
    quotRem (Pos a) (Pos b) = let (q,r) = quotRem a b in (Pos q, Pos r)
    quotRem (Neg a) (Neg b) = let (q,r) = quotRem a b in (Pos q, Neg r)
    quotRem (Pos a) (Neg b) = let (q,r) = quotRem a b in (Neg q, Pos r) 
    quotRem (Neg a) (Pos b) = let (q,r) = quotRem a b in (Neg q, Neg r) 
    toInteger (Pos a) = toInteger a
    toInteger (Neg a) = negate (toInteger a)
