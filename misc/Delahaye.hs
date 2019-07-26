---------------------------------------------------------------------------
-- Delahaye: pi, die Story (Le fascinant nombre pi)
---------------------------------------------------------------------------
module Delahaye
where

  import Data.List (foldl',nub,group,filter)
  import Data.Char (intToDigit)
  import Data.Ratio
  import Perm 
  import Binom
  import Debug.Trace (trace)

  -- very slow, very demanding
  pDigitsOfPi :: Integer -> Double
  pDigitsOfPi p = 4*(k/a)
    where xes = [(x,y) | x <- [-n..n], y <- [-n..n]]
          a   = fromIntegral((2*n + 1)^2)
          k   = fromIntegral(length (filter f xes))
          f (x,y) = x^2 + y^2 < n^2
          n   = 10^p

  -- A simple formula to compute 10 digits of pi (Ramanujan)
  ramaPi :: Double
  ramaPi = p
    where p' = (102-(2222/22^2))**(1/4) :: Double
          p0 = 100000000*p' :: Double
          p  = fromIntegral (round(p0)::Integer) / 100000000

  -- A fast convergence series to compute approximations to pi (Ramanujan)
  ramaPi2 :: Integer -> Double
  ramaPi2 n = (9801/(8**(1/2))) * (1/s)
    where s = sum [(nu i) / (de i) | i <- [0..n]]
          nu x = fromIntegral ((fac (4*x)) * (1103 + 26390*x))
          de x = fromIntegral ((fac x)^4 * 396^(4*x))

  -- Fast convergence and even more important:
  -- computation of individual digits of pi (Simon Plouffe)
  plouffe :: Integer -> Double
  plouffe n = sum [1/(16**i) * (4/(8*i+1) - 2/(8*i+4) - 1/(8*i+5) - 1/(8*i+6)) | i <- is]
    where is = map fromIntegral [0..n] 

  -- Double too limited for this :-(
  liouville :: Integer -> Double
  liouville n = sum [1/10^(fac i) | i <- [1..n]]

  -- We show it as a list of 0 and 1
  liou :: Integer -> [Int]
  liou 0 = []
  liou n = go 1 1
     where bit i x | fac i == x = 1
                   | otherwise  = 0
           go i x | i > n = []
                  | otherwise = let a = bit i x
                                 in if a == 0 then a : go i (x+1)
                                              else a : go (i+1) (x+1)
