module Line
where

  slope :: (Double,Double) -> (Double,Double) -> Double
  slope (x1,y1) (x2,y2) | x1 == x2 = 0
                        | otherwise = (y2-y1)/(x2-x1)

  intercept :: Double -> (Double,Double) -> Double
  intercept l (x,y) = y - (l*x)

  extend2X :: Double -> (Double,Double) -> (Double, Double) -> Double
  extend2X x p@(x1,y1) q@(x2,y2) = let l = slope p q
                                       b = intercept l p
                                    in l*x + b
