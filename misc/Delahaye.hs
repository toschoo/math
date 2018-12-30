module Delahaye
where

  import Data.List (foldl',nub,group,filter)
  import Data.Char (intToDigit)
  import Data.Ratio
  import Perm 
  import Debug.Trace (trace)

  pDigitsOfPi :: Integer -> Double
  pDigitsOfPi p = 4*(k/a)
    where xes = [(x,y) | x <- [-n..n], y <- [-n..n]]
          a   = fromIntegral((2*n + 1)^2)
          k   = fromIntegral(length (filter f xes))
          f (x,y) = x^2 + y^2 < n^2
          n   = 10^p

