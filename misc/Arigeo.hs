module AriGeo
where

---------------------------------------------------------------------------
-- The Little Gauss
---------------------------------------------------------------------------
ltGauss :: (Num a, Fractional a) => a -> a
ltGauss n = n*(n+1)/2

---------------------------------------------------------------------------
-- Little Gauss with coefficient and arbitrary sequence
---------------------------------------------------------------------------
arith :: (Num a, Fractional a) => [a] -> a
arith [] = 0
arith ns = n*(h+l)/2
  where h = head   ns
        l = last   ns
        n = fromIntegral (length ns)

---------------------------------------------------------------------------
-- Compute function to compute arithmetic series (sum) from given sequence
---------------------------------------------------------------------------
arif :: (Num a, Fractional a) => [a] -> (a -> a)
arif [] = \_ -> 0
arif ns = (*) ((h+l)/2)
  where h = head   ns
        l = last   ns

---------------------------------------------------------------------------
-- Geometric sequence
---------------------------------------------------------------------------
geo :: (Num a, Fractional a) => [a] -> a
geo [] = 0
geo ns = (h-h*r^n)/(1-r)
  where h = head  ns
        s = head (tail ns)
        r = s/h
        n = fromIntegral (length ns)

---------------------------------------------------------------------------
-- Comput function to compute geometric series from given sequence
---------------------------------------------------------------------------
geof :: (Num a, Fractional a, Floating a, Num b, Integral b) => 
        [a] -> (b -> a)
geof [] = \_ -> 0
geof ns = \n -> (1-h*r^n)/(1-r)
  where h = head  ns
        s = head (tail ns)
        r = s/h
  
