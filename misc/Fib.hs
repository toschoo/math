module Fib
where

  import           Data.List (intercalate,sort)
  import           Debug.Trace (trace)

  fib :: Integer -> Integer
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1)

  phi :: Double
  phi = 0.5 * (1 + sqrt 5)

  phi' :: Double
  phi' = 1 - phi

  golden :: Double -> Double
  golden a = phi*a

  fibApx :: Integer -> (Integer, Integer)
  fibApx n = let n' = fromIntegral n 
              in (floor   $ phi^(n'-2), 
                  ceiling $ phi^(n'-1))

  fi :: Integer -> Integer
  fi n = round (phi^n'/sqrt 5)
    where n' = fromIntegral n

  fi2 :: Integer -> Integer
  fi2 n = round ((phi^n' - phi'^n')/sqrt 5)
    where n' = fromIntegral n

  fir :: Integer -> Double 
  fir n = (phi^n - phi'^n)/sqrt 5


  g :: Double -> Double
  g x = x / (1 - x - x^2)

  g' :: Double -> Double
  g' x = (1/(sqrt 5)) * (1/(1-x*phi) - 1/(1-x*phi'))
