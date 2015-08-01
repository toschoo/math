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
  --       (c+di) = (1+0i) / (a+bi)
  -- division of 2 number (a + bi) / (c + di) = 
  --                      (ac + bd / c^2 + d^2) + ((bc -ad)/(c^2 + d^2))i
  -- hence:
  --       (1a + 0b) / (a^2 + b^2) + ((0a - 1b)/(a^2 + b^2))i
  --     = a / (a^2+b^2) - (b/(a^2 + b^2))i 
  --
  -- This exercise is a nice test for cdiv!
  ex1_1 :: Double -> Double -> Bool
  ex1_1 x y = let a  = Complex x y
                  a' = unity / a
               in a' == Complex (x / (x^2+y^2)) (-y/(x^2+y^2))
  
  -------------------------------------------------------------------------
  -- (-1 + sqrt(3)i)^3 = 8, but this is not easy to show with Double
  -- I need a CAS!!!
  --
  -- (1 - sqrt(3)i - sqrt(3)i -3) (-1 + sqrt(3)i)
  -- = (-1 + sqrt(3)i + sqrt(3)i + 3 + sqrt(3)i + 3 + 3 - 3sqrt(3)i)
  -- = 8 + 3sqrt(3)i - 3sqrt(3)i
  -- = 8
  -------------------------------------------------------------------------
  ex1_2 :: (Complex Double,Complex Double)
  ex1_2 = let n = Complex (-1) (sqrt 3)
              d = Complex 2.0 0.0
           in (n^3,d^3)
