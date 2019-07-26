module Calculus
where

  -- integral of a function f in the interval a..b with n iterations
  integralOfF :: (Double -> Double) -> Double -> Double -> Double -> Double
  integralOfF f a b n = sum [(f x_1) * (x_2 - x_1)  | (x_1, x_2) <- zip [a,a+s..b-s] [a+s,a+2*s..b]]
    where s = step a b n

  -- integral a list of points
  integralByValues :: [(Double,Double)] -> Double
  integralByValues ps = sum [y * d | (d, y) <- zip ds ys]
    where ds = [x_2 - x_1 | (x_1, x_2) <- zip (init (map fst ps)) (tail (map fst ps))]
          ys = map snd ps 

  step :: Double -> Double -> Double -> Double
  step a b n = (b - a) / n

  -- derivative of a function at a given point

  -- derivative(s) of values at each given point

  -- derivative of a function
  derivativeOfF :: (Double -> Double) -> Double -> (Double -> Double)
  derivativeOfF f d = f'
    where f' x = (f(x+d)-(f x))/d
