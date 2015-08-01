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

  golden :: Double -> Double
  golden a = phi*a

  fibApx :: Integer -> (Integer, Integer)
  fibApx n = let n' = fromIntegral n 
              in (floor   $ phi^(n'-2), 
                  ceiling $ phi^(n'-1))

  fi :: Integer -> Integer
  fi n = round (phi^n'/sqrt 5)
    where n' = fromIntegral n
