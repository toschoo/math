module Poly
where

  import Data.List (nub)

  data Poly a = P [a]
    deriving (Eq,Show)

  coeffs :: Poly a -> [a]
  coeffs (P as) = as

  pretty :: (Num a, Show a, Eq a) => Poly a -> String
  pretty p = go (weigh p)
    where go [] = ""
          go ((i,c):cs) = let x | i == 0    = ""
                                | i == 1    = "x"
                                | otherwise = "x^" ++ show i
                              t | c == 0    = "0"
                                | c == 1    = x
                                | otherwise = show c ++ x
                              o | null cs   = ""
                                | otherwise = " + "
                           in if c == 0 then go cs else t++o++go cs

  apply :: Num a => Poly a -> a -> a
  apply (P []) _ = 0
  apply (P as) x = go x $ zip [0..] (reverse as)
    where go z [] = 0
          go z ((i,c):cs) = c*z^i + go z cs

  weigh :: (Num a) => Poly a -> [(Integer,a)]
  weigh (P []) = []
  weigh (P as) = reverse (zip [0..] (reverse as))

  degree :: Poly a -> Int
  degree (P as) = length as - 1

  solve :: Poly Double -> [Double]
  solve p = case degree p of
              0 -> coeffs p
              1 -> solvel p
              2 -> solve2 p
              3 -> solve3 p
              _ -> error "I don't know how to solve this polynomial"

  solvel :: (Num a,Fractional a) => Poly a -> [a]
  solvel (P [a,b]) = [-b/a]
  solvel _         = error "oops!"

  solve2 :: Poly Double -> [Double]
  solve2 (P [a,b,c]) | det < 0   = []
                     | otherwise = let d  = sqrt det
                                       x1 = (-b + d) / 2*a
                                       x2 = (-b - d) / 2*a
                        in if x1 /= x2 then [x1,x2] else [x1]
    where det = b^2 - 4*a*c
  solve2 _           = error "oops!"

  -- not correct...
  solve3 :: Poly Double -> [Double]
  solve3 (P [a,0,c,d]) | a /= 1    = solve3 (P [1,0,c/a,d/a])
                       | otherwise =
                         let disc = d^2/4 + c^3/9
                             u13  = -d/2 + sqrt disc
                             u23  = -d/2 - sqrt disc
                             v13  = -d/2 + sqrt disc
                             v23  = -d/2 - sqrt disc
                             u1   | u13 < 0    = -(-u13)**(1/3)
                                  | otherwise  = u13**(1/3)
                             u2   | u23 < 0    = -(-u23)**(1/3)
                                  | otherwise  = u23**(1/3)
                             v1   | v13 < 0    = -(-v13)**(1/3)
                                  | otherwise = v13**(1/3)
                             v2   | v23 < 0    = -(-v23)**(1/3)
                                  | otherwise = v23**(1/3)
                             (u,v) = finduv (-d/3) u1 u2 v1 v2
                          in if disc < 0 then [] else [u+v]
  solve3 (P [a,b,c,0]) | a /= 1    = solve3 (P [1,b/a,c/a,0])
                       | otherwise = 
                           let xs = solve2 (P [1,b,c])
                            in nub (0:xs)
  solve3 (P [a,b,c,d]) | a /= 1    = solve3 (P [1,b/a,c/a,d/a])
                       | otherwise = 
                         let p  = -(b^2)/3 + c
                             q  = (2*b^3)/27 - (b*c)/3 + d
                          in [y-a/3 | y <- solve3 (P [1,0,p,q])]
  solve3 _           = error "oops!"

  finduv :: Double -> Double -> Double -> Double -> Double -> (Double,Double)
  finduv d u1 u2 v1 v2 = let one   = (u1,v1)
                             two   = (u1,v2) 
                             three = (u2,v1) 
                             four  = (u2,v2) 
                          in mymin [(x,abs (d-(fst x * snd x))) | 
                                    x <- [one,two,three,four]]
    where mymin (x:xs) = go x xs
          go x [] = fst x
          go x (z:zs) | snd x < snd z = go x zs
                      | otherwise     = go z zs
    
                          
  -- add
  -- subtract
  -- multiply
  -- divide
  -- factor
