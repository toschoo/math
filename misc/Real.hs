module Real
where

  import Data.Ratio
  import Debug.Trace (trace)

  data RealN = R Integer Integer
    deriving Show

  real :: Integer -> Integer -> RealN
  real i e = simplify (R i e)

  blowup :: Integer -> RealN -> RealN
  blowup  i (R r e) | i == e = R r e
                    | e <  i = blowup i (R (10*r) (e+1))
                    | e >  i = error "cannot blowup down"
    
  simplify :: RealN -> RealN
  simplify (R a e) | e > 0 &&
                     a `rem` 10 == 0 = simplify (R (a `div` 10) (e-1))
                   | e < 0           = blowup 0 $ R a e
                   | otherwise       = R a e

  instance Eq RealN where -- equal if difference below limit
    r1@(R a e1) == r2@(R b e2) | e1 == e2 = a == b
                               | e1 >  e2 = r1 == blowup e1 r2
                               | e1 <  e2 = blowup e2 r1 == r2

  instance Ord RealN where
    compare r1@(R a e1) r2@(R b e2) | e1 == e2  = compare a b
                                    | e1 >  e2  = compare r1 (blowup e1 r2)
                                    | e1 <  e2  = compare (blowup e2 r1) r2

  instance Num RealN where
    (R a e1) + (R b e2) | e1 == e2  = R (a+b) e1
                        | e1 >  e2  = simplify $ R (a+b*10^(e1-e2)) e1
                        | otherwise = simplify $ R (a*10^(e2-e1)+b) e2
    (R a e1) - (R b e2) | e1 == e2 &&
                          a  >= b   = R (a-b) e1
                        | e1 == e2  = error "subtraction beyond zero!"
                        | e1 >  e2  = simplify $ (R a e1) - (R (b*10^(e1-e2)) e1)
                        | otherwise = simplify $ (R (a*10^(e2-e1)) e2) - (R b e2)
    (R a e1) * (R b e2) = simplify $ R (a*b) (e1+e2)
    negate  r       = r -- we cannot negate natural numbers
    abs     r       = r
    signum  r       = r
    fromInteger i   = R i 0

  instance Fractional RealN where
    (/) = rdiv 17
    fromRational r = (R (numerator   r) 0) / 
                     (R (denominator r) 0)

  instance Real RealN where
    toRational (R r e) = undefined

  pump :: Integer -> Integer -> Integer
  pump a b | a `div` b > 0 = a
           | otherwise = pump (10*a) b

  size :: Integer -> Integer
  size a | a < 1  = 0
         | a < 10 = 1
         | otherwise = 1 + size (a `div` 10)
      

  rdiv :: Integer -> RealN -> RealN -> RealN
  rdiv n (R a e1) (R b e2) = let (q,e) = go n a b 0 
                              in simplify (R q $ e1 - e2 + e)
    where go i x y e | i == 0 = (0,e)
                     | otherwise = case x `quotRem` y of
                                    (q,0) -> (q,e)
                                    (q,r) -> let r' = pump r y
                                                 i' | e == 0 = n + size q
                                                    | otherwise = i
                                                 (z,e') = go (i'-1) r' y (e+1)
                                              in (10^i' * q + z,e') 
 
  r2d :: RealN -> Double
  r2d (R a e) = (fromIntegral a) / 10^e -- (e-1)
