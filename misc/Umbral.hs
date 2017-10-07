---------------------------------------------------------------------------
-- Factorial Polynomials and the Umbral Calculus
---------------------------------------------------------------------------
module Umbral
where

  import qualified Perm as Perm
  import           Poly
  import           Data.List (sortOn, groupBy, zipWith)
  import           Data.Function (on)

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
  stir1poly n = poly [Perm.stirling1s n k | k <- [0..n]]

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

  stirling1s :: (Integral a) => a -> a -> a
  stirling1s n k | k > n     = 0
                 | k == n    = 1
                 | otherwise =  hdcf n
    where hdcf = head . drop i . coeffs . facpoly
          i    = fromIntegral k

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
  fpPowTerms 0 =  [(1,P[1])]
  fpPowTerms n =  [(Perm.stirling2 n k, facpoly k) | k <- [1..n]]

  -------------------------------------------------------------------------
  -- create the facpoly terms of polynomials
  -- a_1x^n +  a_2x^(n-1) + ...
  -- [scale (a_1 * stirling2 n k) (facpoly k), ...]
  -------------------------------------------------------------------------
  fpPolyScaled :: Poly Integer -> [Poly Integer]
  fpPolyScaled (P cs) = first ++ concat (go 1 $ tail cs)
    where s c (n,p) = scale (c*n) p
          go _ [] = []
          go k (x:xs) | x == 0    = go (k+1) xs
                      | otherwise = map (s x) (fpPowTerms k) :
                                    go (k+1) xs
          first = case cs of
                    [] -> []
                    (x:_) -> [scale x (facpoly 0)]

  -------------------------------------------------------------------------
  -- create the facpoly terms of polynomials 
  -- as pairs of coefficient and polynomial
  -------------------------------------------------------------------------
  fpPolyTerms :: Poly Integer -> [(Integer,Poly Integer)]
  fpPolyTerms (P cs) = [foldl ab p0 p | p <- p2]
    where p0 = (0,P[0])
          p1 = concat [map (s c) (fpPowTerms k) | (c,k) <- zip cs [0..]]
          p2 = groupBy ((==) `on` snd) $ sortOn  (degree . snd) p1
          ab a b = (fst a + fst b, snd b)
          s c (n,p) = (c*n, p)

  -------------------------------------------------------------------------
  -- inverse of the previous
  -------------------------------------------------------------------------
  sumFpPolyTerms :: [(Integer, Poly Integer)] -> Poly Integer
  sumFpPolyTerms = sump . map (uncurry scale)
