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

  borrow :: Integer -> Integer -> (Integer,Integer)
  borrow a b | a > b     = (a,0)
             | otherwise = let (x,e) = borrow (10*a) b
                            in (x,e+1)

  rdiv :: Integer -> RealN -> RealN -> RealN
  rdiv n r1@(R a e1) r2@(R b e2) | e1 < e2 = 
                                   rdiv n (blowup e2 r1) r2
                                 | a  < b && e1 == e2 = 
                                   rdiv n (blowup (e2+1) r1) r2
                                 | otherwise = 
                                   simplify (R (go n a b) (e1 - e2 + n))
    where go 0 x y = 0 
          go i x y = case x `quotRem` y of
                       (q,0) -> 10^i * q
                       (q,r) -> let (r',e) = borrow r y
                                    q'     = 10^i * q
                                 in if e > i then q' 
                                             else q' + go (i-e) r' y
 
  r2d :: RealN -> Double
  r2d (R a e) = (fromIntegral a) / 10^e -- (e-1)

  r2R :: RealN -> Rational
  r2R (R a e) = a % (10^e)
