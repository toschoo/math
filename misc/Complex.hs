module Complex
where

  import Prime
  import Data.Ratio

  data Complex a = Complex a a

  instance (Eq a) => Eq (Complex a)
    where (Complex rx ix) == (Complex ry iy) = rx == ry && ix == iy

  instance (Show a, Num a, Ord a, Eq a) => Show (Complex a)
    where show (Complex 0 a) = show a ++ "i"
          show (Complex a 0) = show a 
          show (Complex a b) = let x | b < 0    = ""
                                     |otherwise = "+" 
                                in concat ["(", show a, x, show b, "i", ")"]

  instance (Num a) => Num (Complex a) 
    where (+) = add
          (-) = sub
          (*) = mul
          abs    = undefined -- ?
          signum = undefined -- ?
          negate = undefined -- ?
          fromInteger i = Complex (fromInteger i) 0

  instance (Fractional a) => Fractional (Complex a) where 
    (/) = cdiv
    fromRational r = let n = fromIntegral $ numerator   r
                         d = fromIntegral $ denominator r 
                      in Complex (n/d) 0

  infix |+
  (|+) = Complex
  
  add :: (Num a) => Complex a -> Complex a -> Complex a
  add (Complex rx ix) (Complex ry iy) = Complex (rx+ry) (ix+iy)
  
  sub :: (Num a) => Complex a -> Complex a -> Complex a
  sub (Complex rx ix) (Complex ry iy) = Complex (rx-ry) (ix-iy)

  mul :: (Num a) => Complex a -> Complex a -> Complex a
  mul (Complex rx ix) (Complex ry iy) = Complex (rx * ry - ix * iy)
                                                (rx * iy + ry * ix)

  cdiv :: (Num a, Fractional a) => Complex a -> Complex a -> Complex a
  cdiv (Complex rx ix) (Complex ry iy) = 
        Complex ((rx*ry + ix*iy) / (ry^2 + iy^2))
                ((ix*ry - rx*iy) / (ry^2 + iy^2))

  conjugate :: (Num a) => Complex a -> Complex a
  conjugate (Complex r i) = Complex r (-i)

  unity :: (Num a) => Complex a
  unity = Complex 1 0

  zero :: (Num a) => Complex a
  zero = Complex 0 0 

  ------------------------------------------------------------------------- 
  -- Gaussian Integer
  ------------------------------------------------------------------------- 
  type GInteger = Complex Integer

  gNorm :: GInteger -> Integer
  gNorm c = case c * conjugate c of
              Complex r 0 -> r
              Complex _ i -> error $ "norm is not an Integer: " ++ show c

  gPrime :: GInteger -> Bool
  gPrime (Complex r i) | r /= 0 && 
                         i /= 0    = prime (r^2 + i^2)
                       | r /= 0    = is4nPrime r 
                       | i /= 0    = is4nPrime i
                       | otherwise = False
    where is4nPrime x = case (x-3) `quotRem` 4 of -- prime numbers
                          (p,0) -> prime (abs p)  -- of this form cannot be
                          _     -> False          -- sum of two squares

  ------------------------------------------------------------------------- 
  -- Factorisation of GInteger
  ------------------------------------------------------------------------- 
  {-
  Calculate norm(G), then factor norm(G) into primes p1, p2 ... pn.
  For each remaining factor p:
   if p=2, u = (1 + i).   
      strike p from the list of remaining primes.
   else if p mod 4 = 3, q = p, and strike 2 copies of p from the list of primes.
   else find k such that k^2 = -1 mod p, then u = gcd(p, k+i)
       if G/u has remainder 0, q = u
       else q = conjugate(u)
       strike p from the list of remaining primes.
   Add q to the list of Gaussian factors.
   Replace G with G/q.
  endfor
  -}
  {-
  gFact :: GInteger -> [GInteger]
  gFact c@(Complex r i) = let n  = gNorm c
                              ps = trialfact n
                           in go ps
    where go [] = []
          go (2:ps) = (1|+1) : go ps
          go (p:ps) | p `rem` 4 == 3 = (p|+0) : go (delete p ps)
                    | otherwise =
          findk i | 2^i `rem` 
  -}
