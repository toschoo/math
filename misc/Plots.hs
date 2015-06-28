module Plots
where

  import Data.List (sort)

  poly :: Int -> [Double] -> Double -> Double
  poly deg cs x | length cs < deg = 
                    let n = deg - length cs
                     in poly deg (cs ++ take n (repeat 0.0)) x
                | otherwise       = go deg cs
    where go k []     = go k [0.0]
          go 0 (c:cc) = c 
          go k (c:cc) = c * x^k + go (k-1) cc


  weierstrass :: Int -> Double -> Double -> Double -> Double
  weierstrass 0 _ _ _ = 0.0
  weierstrass k a b x = a^k * cos(b^k*pi*x) + weierstrass (k-1) a b x

  weierstrass2 :: Int -> Double -> Double
  weierstrass2 0 _ = 0.0
  weierstrass2 k x = 1.0/(2.0^k) * sin(2^k * x) + weierstrass2 (k-1) x

  summation :: (Int -> Double) -> (Int -> Int) -> Int -> Int -> Double
  summation f add i k | i == k    = f i
                      | otherwise = f i + summation f add (add i) k

  form2poly :: String -> (Double -> Double)
  form2poly s = let ts = getTerms s
                    k  = head $ reverse $ sort $ map fst ts
                    co = map snd ts
                 in poly k co

  getTerms :: String -> [(Int, Double)]
  getTerms [] = []
  getTerms s  = let (k,co,s') = getTerm s
                 in (k,co) : getTerms s'

  getTerm :: String -> (Int,Double, String)
  getTerm s = let s1 = dropWhile (== ' ') s
                  s2 = takeWhile (\x -> not $ x `elem` [' ', '-', '+']) s1
                  co' = takeWhile (/= 'x') s2
                  co  = if null co' then 1.0 else read co'
                  x   = takeWhile (== 'x') s2
                  s3 = drop (length co' + length x) s2
                  k' = if length x > 0 then s3 else "0"
                  k  = if null k' then 1 else read k'
               in (k,co,drop (length s + length s2) s)
                  

  -- s = "ax2 + bx1 + c"
