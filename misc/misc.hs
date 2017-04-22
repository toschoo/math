import Data.Ratio

---------------------------------------------------------------------------
-- Zeno's paradox
---------------------------------------------------------------------------
zeno :: Integer -> Integer -> Ratio Integer
zeno n k = go 1
  where go i | i > k     = (0 % 1)
             | otherwise = (1 % (n^i)) + go (i+1)

zenos :: Integer -> Integer -> [Ratio Integer]
zenos n k = map (zeno n) [1..k]

zeno2 :: Integer -> Integer -> Ratio Integer
zeno2 n k = ((n^k - 1) `div` (n - 1)) % n^k

testZeno2 :: Integer -> Integer -> Bool
testZeno2 l k = go 2 1
  where go i j | i == l    = True
               | j == k    = go (i+1) 1
               | zeno  i j == 
                 zeno2 i j = go i (j+1)

---------------------------------------------------------------------------
-- Heron's method
---------------------------------------------------------------------------
heron :: Integer -> Int -> Double
heron n s = let a = fromIntegral n
             in go s a (a/2)
  where go 0 _ x = x
        go i a x = go (i-1) a ((x + a/x)/2)

heron2 :: Double -> Double -> Double
heron2 n x = (x + n/x) / 2
