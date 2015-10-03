module Real
where

  import Debug.Trace (trace)

  data RealN = R Integer Integer
    deriving Show

  real :: Integer -> Integer -> RealN
  real i e = simplify (R i e)

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
    
  simplify :: RealN -> RealN
  simplify (R a e) | e > 0 &&
                     a `rem` 10 == 0 = simplify (R (a `div` 10) (e-1))
                   | e < 0           = simplify (R (a*10) (e+1))
                   | otherwise       = R a e

  pump :: Integer -> Integer -> Integer
  pump a b | a `div` b > 0 = a
           | otherwise = pump (10*a) b

  rdiv :: Integer -> RealN -> RealN -> RealN
  rdiv n (R a e1) (R b e2) = let (q,e) = go n a b 0 in simplify (R q $ e1 - e2 + e)
    where go i x y e | i == 0 = (0,e+1)
                     | otherwise = case x `quotRem` y of
                                    (q,0) -> (q,e)
                                    (q,r) -> let r' = pump r y
                                                 e1 | e == 0 && q /= 0 = 0
                                                    | otherwise        = e+1
                                                 (z,e') = go (i-1) r' y e1
                                              in (10^i * q + z,e') 
 
  r2d :: RealN -> Double
  r2d (R a 0) = fromIntegral a
  r2d (R a e) = (fromIntegral a) / 10^(e-1)
