module Line
where

  data Line = L Double Double
    deriving (Show,Eq)

  p2l :: (Double,Double) -> (Double,Double) -> Line
  p2l p q = let s = slope p q
                c = intercept s p
             in L s c

  line :: Double -> Double -> Line
  line = L

  slp :: Line -> Double
  slp (L s _) = s

  icpt :: Line -> Double
  icpt (L _ c) = c

  slope :: (Double,Double) -> (Double,Double) -> Double
  slope (x1,y1) (x2,y2) | x1 == x2 = 0
                        | otherwise = (y2-y1)/(x2-x1)

  intercept :: Double -> (Double,Double) -> Double
  intercept l (x,y) = y - (l*x)

  intersect :: Line -> Line -> Double
  intersect l1 l2 = let a  = slp  l1 
                        c  = slp  l2
                        b  = icpt l1 
                        d  = icpt l2
                     in (d-b) / (a-c)

  getY :: Double -> Line -> Double
  getY x l = let s =  slp l
                 c = icpt l
              in s*x + c

  getX :: Double -> Line -> Double
  getX y l = let s = slp  l
                 c = icpt l
              in (y-c)/s
