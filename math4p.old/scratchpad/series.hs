module Binom
where

import Data.List (intercalate, nub)

toTheKFalling :: Integer -> Integer -> Integer
toTheKFalling = toTheK (-)

toTheKRising :: Integer -> Integer -> Integer
toTheKRising = toTheK (+)

toTheK :: (Integer -> Integer -> Integer) -> 
           Integer -> Integer -> Integer
toTheK add n k = go 0
  where go i | i == k    = 1
             | otherwise = (n `add` i) * go (i + 1)

fac :: Integer -> Integer
fac 0 = 1
fac 1 = 1
fac n = n * fac (n - 1)

{- ????
   Accoding to TAOCP, p. 47
fac2 :: Integer -> Integer
fac2 0 = 1
fac2 1 = 1
fac2 n = foldr (*) 1 $ go [] primes
  where go fs (p:ps) | p >= n    = fs
                     | otherwise = go (p^(facts 1 p []) : fs) ps
        facts i p ns | p >= n = sum ns
                     | otherwise = 
                         let k = n `div` p 
                          in if k > 0 then facts (i+1) (p^i) (k:ns)
                                      else sum ns
-}

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose n 1 = n
choose n 2 = n * (n-1) `div` 2
choose n k = (n >| k) `div` fac k 

infixr >| 
(>|) :: Integer -> Integer -> Integer
(>|) = toTheKFalling

infixr |> 
(|>) :: Integer -> Integer -> Integer
(|>) = toTheKRising

infixr >|>
(>|>) :: Integer -> Integer -> Integer
(>|>) = choose

pascalRow :: Integer -> Integer -> [Integer]
pascalRow n k = map (choose n) [0..k]

pascalCol :: Integer -> Integer -> [Integer]
pascalCol n k = map (`choose` k) [0..n]

pascal :: Integer -> Integer -> [[Integer]]
pascal n k = map (`pascalRow` k) [0..n]

pascalTab :: Integer -> Integer -> String
pascalTab n k = header ++ (
                intercalate "\n" $ rindex 0 $ tab2Str $ pascal n k)
  where tab2Str = map row2Str
        rindex _ [] = []
        rindex i (x:xs) = (n2s i ++ x) : rindex (i+1) xs
        cindex i = concatMap n2s [0..i]
        header   = "      " ++ cindex k ++ "\n" 

row2Str :: [Integer] -> String
row2Str = concatMap n2s

n2s :: Integer -> String
n2s n = let x = show n
            l = length x
            d = 5 - l
            s = map (\_ -> ' ') [0..d]
         in s ++ x

pfact :: Integer -> [Integer]
pfact n | n  < 1    = error "not a positive integer"
        | n == 1    = [n]
        | prime n   = [n]
        | otherwise = let p = ldp n
                          k = n `div` p
                       in if prime k then [p,k]
                                     else p : pfact k

squarefree :: Integer -> Bool
squarefree n = let ps = pfact n in ps == nub ps

mu :: Integer -> Integer
mu 1 = 1
mu n = let ps = pfact n
        in if ps == nub ps then (-1)^(length ps)
                           else 0

mertens :: Double -> Double -- Integer -> Integer
mertens 1 = 1
mertens n = fromIntegral $ sum $ map mu [1..ceiling n]

ldp :: Integer -> Integer
ldp  = ldpf primes 

ldpf :: [Integer] -> Integer -> Integer
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes :: [Integer]
primes = 2:filter prime [3..]

prime :: Integer -> Bool
prime n | n < 1     = error "Not a positive integer!"
        | n == 1    = False
        | otherwise = ldp n == n

prime2 :: Integer -> Integer -> Bool
prime2 2 _ = True
prime2 p n | gcd p n /= 1         = False
           | n^(p-1) `rem` p /= 1 = False
           | otherwise            = prime p

countP :: Integer -> Integer
countP n = n `div` (fromIntegral $ length (2:filter prime [3..n])) 

harmonic :: Integer -> Double
harmonic n = go 1.0
  where go i | i >  n'   = 0.0
             | otherwise = 1.0 / i + go (i+1.0)
        n' = fromIntegral n

phi :: Double
phi = 0.5 * (1 + sqrt 5)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fibApx :: Integer -> (Integer, Integer)
fibApx n = let n' = fromIntegral n 
            in (floor   $ phi^(n'-2), 
                ceiling $ phi^(n'-1))

fi :: Integer -> Integer
fi n = let n' = fromIntegral n
        in round (phi^n'/sqrt 5)

myseries :: Integer -> Integer -> (Integer,Integer)
myseries n 0 = (1,1)
myseries n k = ((n^k - 1) `div` (n - 1), n^k)

poly :: Integer -> Integer -> Integer
poly n 0 = 0
poly n k = n^(k-1) + poly n (k-1)

testseries :: Integer -> (Integer,Integer)
testseries n = go n 2
  where go :: Integer -> Integer -> (Integer,Integer)
        go n k = if fst (myseries n k) /= poly n k 
                   then (n,k)
                   else go n (k+1)

pow :: Integer -> Integer -> Integer
pow b 0 = 1
pow b 1 = b
pow 0 _ = 0
pow b n = b * pow b (n-1)

powBySqr :: Integer -> Integer -> Integer
powBySqr b 0 = 1
powBySqr b 1 = b
powBySqr 0 _ = 0
powBySqr b n | even n    =     powBySqr (b*b) (n     `div` 2)
             | otherwise = b * powBySqr (b*b) ((n-1) `div` 2)

root1 :: Integer -> Integer -> (Integer,Integer)
root1 p 0 = undefined
root1 p 1 = (p,0)
root1 p n = go p (p `div` 2) (-)
  where go x y f | y == 0    = (x,p-pow x n)
                 | otherwise = 
                   let q = x `f` y
                    in case compare p (pow q n) of
                         EQ -> (q, 0)
                         LT -> if y <= 1 then (x, p - pow x n)
                                         else go q (y `div` 2) (-)
                         GT -> go q (y `div` 2) (+)

root1IO :: Integer -> Integer -> IO (Integer,Integer)
root1IO p 0 = undefined
root1IO p 1 = return (p,0)
root1IO p n = go p (p `div` 2) (-)
  where go x y f | x == y    = return (x,p-pow x n)
                 | y == 0    = return (x,p-pow x n)
                 | otherwise = 
                   let q = x `f` y
                    in case compare p (pow q n) of
                         EQ -> return (q, 0)
                         LT -> do print ('m', q, y `div` 2) 
                                  go q (y `div` 2) (-)
                         GT -> do print ('p', q, y `div` 2)
                                  go q (y `div` 2) (+)


