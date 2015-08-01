module Axler
where

  import Complex
  
  import Debug.Trace (trace)

  ------------------------------------------------------------------------
  -- Exercises 1
  ------------------------------------------------------------------------
  -- 1 / (a + bi) = c + di
  -- c + di is the multiplicative inverse of a + bi
  -- thus: (a+bi)(c+di) = unity
  --       (c+di) = unity / (a+bi) 
  ex1_1 :: Double -> Double -> Bool
  ex1_1 x y = let a  = Complex x y
                  a' = unity / a
               in trace (show a') $ a' == a' -- tbc
  
  -------------------------------------------------------------------------
  -- (-1 + sqrt(3)i)^3 = 8, but this is not easy to show with Double
  -- I need a CAS!!!
  -- (1 - sqrt(3)i - sqrt(3)i -3) (-1 + sqrt(3)i)
  -- = (-1 + sqrt(3)i + sqrt(3)i + 3 + sqrt(3)i + 3 + 3 - 3sqrt(3)i)
  -- = 8 + 3sqrt(3)i - 3sqrt(3)i
  -- = 8
  -------------------------------------------------------------------------
  ex1_2 :: (Complex Double,Complex Double)
  ex1_2 = let n = Complex (-1) (sqrt 3)
              d = Complex 2.0 0.0
           in (n^3,d^3)
