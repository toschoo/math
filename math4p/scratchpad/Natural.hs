module Natural
where

  import Numbers
  import Prelude hiding (gcd)

  data Natural = N Number

  instance Eq Natural where
    (N a) == (N b) = cmp a b == EQ

  instance Show Natural where
    show (N ns) = show $ toInt ns

  instance Num Natural where
    (N as) + (N bs) = N (as `add` bs)
    (N as) - (N bs) | cmp as bs == LT = error "subtraction below zero"
                    | otherwise       = N (as `sub` bs)
    (N as) * (N bs) = N (as `mul2` bs)
    negate  n       = n -- we cannot negate natural numbers
    abs     n       = n
    signum  n       = n
    fromInteger i   = N (fromInt i)

  instance Enum Natural where
    succ n = n + N (unity)
    pred (N [Zero])      = error "zero has no predecessor"
    pred n = n - N (unity)
    toEnum i | i < 0     = error "negative number to natural"
             | otherwise = N $ fromInt $ toInteger i
    fromEnum (N ns)      = fromIntegral $ toInt ns

  instance Ord Natural where
    compare (N as) (N bs) = cmp as bs

  instance Real Natural where
    toRational (N ns) = fromIntegral $ toInt ns

  instance Integral Natural where
    quotRem   (N as) (N bs) = let (q,r) = nQuotRem2 as bs in (N q, N r)
    toInteger (N ns)        = toInt ns

  gcd :: Natural -> Natural -> Natural
  gcd a 0 = a
  gcd a b = gcd b (a `rem` b)

  floorDiv :: Natural -> Natural -> Natural
  floorDiv a = fst . quotRem a

  ceilDiv :: Natural -> Natural -> Natural
  ceilDiv a b = let (q,r) = a `quotRem` b
                 in if r == 0 then q else q + 1 
