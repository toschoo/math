\ignore{
\begin{code}
module Real
where
  import Natural
  import Data.Ratio ((%), numerator,denominator)
  import Quoz hiding (rdiv, (%))
  import Realrep
  import Debug.Trace (trace)
\end{code}
}

\begin{code}
  instance Eq RealN where 
    r1@(R a e1) == r2@(R b e2)  | e1 == e2  = a == b
                                | e1 >  e2  = r1 == blowup e1 r2
                                | e1 <  e2  = blowup e2 r1 == r2
\end{code}

\begin{code}
  blowup :: Natural -> RealN -> RealN
  blowup  i (R r e)  | i == e  = R r e
                     | e <  i  = blowup i (R (10*r) (e+1))
                     | e >  i  = error "cannot blowup down"
\end{code}

\begin{code}
  instance Ord RealN where
    compare r1@(R a e1) r2@(R b e2)  | e1 == e2  = compare a b
                                     | e1 >  e2  = compare r1 (blowup e1 r2)
                                     | e1 <  e2  = compare (blowup e2 r1) r2
\end{code}

\begin{code}
  instance Num RealN where
    (R a e1) + (R b e2)  |  e1 == e2   = R (a+b) e1
                         |  e1 >  e2   = simplify $ R (a+b*10^(e1-e2)) e1
                         |  otherwise  = simplify $ R (a*10^(e2-e1)+b) e2
    (R a e1) - (R b e2)  |  e1 == e2 &&
                            a  >= b    = R (a-b) e1
                         |  e1 == e2   = error "subtraction beyond zero!"
                         |  e1 >  e2   = simplify $ (R a e1) - (R (b*10^(e1-e2)) e1)
                         |  otherwise  = simplify $ (R (a*10^(e2-e1)) e2) - (R b e2)
    (R a e1) * (R b e2) = real (a*b) (e1+e2)
    negate  r       = r -- we cannot negate natural numbers
    abs     r       = r
    signum  r       = r
    fromInteger i   = R (fromIntegral i) 0

  instance Fractional RealN where
    (/) = rdiv 17 
    fromRational r =  (R (fromIntegral $ numerator   r) 0) / 
                      (R (fromIntegral $ denominator r) 0)

  instance Real RealN where
    toRational (R r e) = i % (10^x)
      where  i  = fromIntegral r :: Integer
             x  = fromIntegral e :: Integer

  roundr :: Natural -> RealN -> RealN
  roundr n (R a e)  | n >= e     = R a e
                    | otherwise  =  let  b  = a `div` 10
                                         l  = a - 10*b
                                         d  | l < 5     = 0
                                            | otherwise = 1
                                    in roundr n (R (b+d) (e-1))

  borrow :: Natural -> Natural -> (Natural,Natural)
  borrow a b  | a > b      =  (a,0)
              | otherwise  =  let (x,e) = borrow (10*a) b
                              in (x,e+1)

  rdiv :: Natural -> RealN -> RealN -> RealN
  rdiv n r1@(R a e1) r2@(R b e2)  |  e1 < e2 = 
                                     rdiv n (blowup e2 r1) r2
                                  |  a  < b && e1 == e2 = 
                                     rdiv n (blowup (e2+1) r1) r2
                                  |  otherwise = 
                                     simplify (R (go n a b) (e1 - e2 + n))
    where  go 0 _ _ = 0 
           go i x y = case x `quotRem` y of
                        (q,0) -> 10^i * q
                        (q,r) -> let  (r',e)  = borrow r y
                                      q'      = 10^i * q
                                 in if e > i  then q' 
                                              else q' + go (i-e) r' y
 
  r2d :: RealN -> Double
  r2d r@(R a e)  | e > 17     = r2d (roundr 17 r)
                 | otherwise  = (fromIntegral a) / 10^e

  r2R :: RealN -> Ratio
  r2R (R a e) = ratio a (10^e)
\end{code}
