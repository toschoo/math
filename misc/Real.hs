module Real
where

  data RealN = R Integer Integer

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
                   | otherwise = R a e

