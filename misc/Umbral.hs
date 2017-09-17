---------------------------------------------------------------------------
-- Factorial Polynomials and the Umbral Calculus
---------------------------------------------------------------------------
module Umbral
where

  import qualified Perm as Perm
  import           Poly

  -------------------------------------------------------------------------
  -- generate the linear factors of x^(n)
  -------------------------------------------------------------------------
  fpfacs :: (Integral a) => a -> [Poly a]
  fpfacs 0 = [P [1]]
  fpfacs n = [poly [-k,1] | k <- [0..n-1]]

  -------------------------------------------------------------------------
  -- compute x^(n) using the linear factors
  -------------------------------------------------------------------------
  facpoly :: (Integral a) => a -> Poly a
  facpoly = prodp mul . fpfacs

  -------------------------------------------------------------------------
  -- compute x^(n) using recursive formula
  -------------------------------------------------------------------------
  rfacpoly :: (Integral a) => a -> Poly a
  rfacpoly 0 = P [1]
  rfacpoly n = mul (rfacpoly (n-1)) (P [-(n-1),1])

  -------------------------------------------------------------------------
  -- compute the differences of x^(n) using x^(n-1)
  -- this is the umbral derivative of x^(n)
  -------------------------------------------------------------------------
  uderivative :: (Integral a) => a -> Poly a
  uderivative n = scale n (facpoly (n-1))

  -------------------------------------------------------------------------
  -- compute the coefficients of x^(n)
  -------------------------------------------------------------------------
  stir1poly :: Integer -> Poly Integer
  stir1poly n = poly [(s k) * Perm.stirling1 n k | k <- [0..n]]
    where s k | even n    = (-1)^k
              | otherwise = (-1)^(k+1)

  -------------------------------------------------------------------------
  -- Test it
  -------------------------------------------------------------------------
  testStirFac :: Integer -> Bool
  testStirFac n = map facpoly [1..n] == map stir1poly [1..n]

  -------------------------------------------------------------------------
  -- compute the stirling numbers of the first kind using x^(n)
  -------------------------------------------------------------------------
  stirling1 :: (Integral a) => a -> a -> a
  stirling1 n k | even n    = s*(-1)^k
                | otherwise = s*(-1)^(k+1)
    where P cs = facpoly n
          i = fromIntegral k
          s = head (drop i cs)

  -------------------------------------------------------------------------
  -- compute the power x^n using a sum of scaled x^(k)
  -- the scaling factors are stirling numbers of the second kind
  -------------------------------------------------------------------------
  stirpow :: Integer -> Poly Integer
  stirpow n = sump [scale (Perm.stirling2 n k) (facpoly k) | k <- [1..n]]

  -------------------------------------------------------------------------
  -- create the terms of the power x^n 
  -- as pairs (stirling2 n k, x^(k))
  -------------------------------------------------------------------------
  fpPowTerms :: Integer -> [(Integer, Poly Integer)]
  fpPowTerms n =  [(Perm.stirling2 n k, facpoly k) | k <- [1..n]]

  -------------------------------------------------------------------------
  -- create the facpoly terms of polynomial
  -- a_1x^n +  a_2x^(n-1) + ...
  -- [scale (a_1 * stirling2 n k) (facpoly k), ...]
  -------------------------------------------------------------------------
  fpPolyTerms :: Poly Integer -> [Poly Integer]
  fpPolyTerms (P cs) = first ++ concat (go 1 $ tail cs)
    where s c (n,p) = scale (c*n) p
          go _ [] = []
          go k (x:xs) | x == 0    = go (k+1) xs
                      | otherwise = map (s x) (fpPowTerms k) :
                                    go (k+1) xs
          first = case cs of
                    [] -> []
                    (x:xs) -> [scale x (facpoly 0)]
