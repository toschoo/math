{-# Language BangPatterns #-}
import Data.List (intercalate, (\\), partition, sortBy, nub, delete)
import Prelude hiding (gcd)
import Natural
import Numbers
import Control.Monad (filterM)
import qualified Data.Vector as V
import Data.Vector ((!))

-- more number operations
-- basic sums: 1 + 2 + ... + n
--             1^2 + ... n^2
--
pow :: (Eq a, Num a) => a -> a -> a
pow n 0 = 1
pow n 1 = n
pow 0 _ = 0
pow n k = n * pow n (k-1) 

root :: (Eq a, Num a, Integral a) => a -> a -> a
root = undefined

-- fibonacci
fib :: (Eq a, Num a) => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- sums and products
-- ...

-- permutations and factorials
-- ... 

fac :: (Eq a, Num a) => a -> a
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

-- binomial coefficients

toTheKFalling :: (Ord a, Eq a, Num a) => a -> a -> a
toTheKFalling n k | n < k     = 0
                  | otherwise = toTheK (-) n k

toTheKRising :: (Ord a, Eq a, Num a) => a -> a -> a
toTheKRising = toTheK (+)

toTheK :: (Ord a, Eq a, Num a) => (a -> a -> a) -> a -> a -> a
toTheK add n k = go 0
  where go i | i == k    = 1
             | otherwise = (n `add` i) * go (i + 1)


choose :: (Ord a, Eq a, Num a, Integral a) => a -> a -> a
choose n 0 = 1
choose 0 k = 0
choose n 1 = n
choose n 2 = n * (n-1) `div` 2
choose n k | n == k    =  1
           | otherwise = (n >| k) `div` fac k 

infixr >| 
(>|) :: (Ord a, Eq a, Integral a, Num a) => a -> a -> a
(>|) = toTheKFalling

infixr |> 
(|>) :: (Ord a, Eq a, Integral a, Num a) => a -> a -> a
(|>) = toTheKRising

infixr >|>
(>|>) :: (Ord a, Eq a, Integral a, Num a) => a -> a -> a
(>|>) = choose

pascalRow :: (Ord a, Eq a, Integral a, Num a) => a -> a -> [a]
pascalRow n k = map (choose n) [0..k]

pascalCol :: (Ord a, Eq a, Integral a, Num a) => a -> a -> [a]
pascalCol n k = map (`choose` k) [0..n]

pascal :: (Ord a, Eq a, Integral a, Num a) => a -> a -> [[a]]
pascal n k = map (`pascalRow` k) [0..n]

pascalTab :: (Ord a, Eq a, Integral a, Num a, Show a) => a -> a -> String
pascalTab n k = header ++ (
                intercalate "\n" $ rindex 0 $ tab2Str $ pascal n k)
  where tab2Str = map row2Str
        rindex _ [] = []
        rindex i (x:xs) = (n2s i ++ x) : rindex (i+1) xs
        cindex i = concatMap n2s [0..i]
        header   = "      " ++ cindex k ++ "\n" 

row2Str :: (Ord a, Eq a, Integral a, Num a, Show a) => [a] -> String
row2Str = concatMap n2s

n2s :: (Ord a, Eq a, Integral a, Num a, Show a) => a -> String
n2s n = let x = show n
            l = length x
            d = if l > 5 then 5 else 5 - l
            s = map (\_ -> ' ') [0..d]
         in s ++ x

-- primes
-- modulo, etc.
pfact :: (Ord a, Eq a, Num a, Integral a) => a -> [a]
pfact n | n  < 1    = error "not a positive integer"
        | n == 1    = [n]
        | prime n   = [n]
        | otherwise = let p = ldp n
                          k = n `div` p
                       in if prime k then [p,k]
                                     else p : pfact k

ldp :: (Ord a, Eq a, Num a, Integral a) => a -> a
ldp  = ldpf primes 

ldpf :: (Ord a, Eq a, Num a, Integral a) => [a] -> a -> a
ldpf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = ldpf ps n

primes :: (Ord a, Eq a, Num a, Integral a) => [a]
primes = 2:filter prime [3..]

prime :: (Ord a, Eq a, Num a, Integral a) => a -> Bool
prime n | n < 1     = error "Not a positive integer!"
        | n == 1    = False
        | otherwise = ldp n == n

countP :: (Ord a, Eq a, Num a, Integral a) => a -> a
countP n = n `div` (fromIntegral $ length (2:filter prime [3..n])) 

-- not here
harmonic :: Integer -> Double
harmonic n = go 1.0
  where go i | i >  n'   = 0.0
             | otherwise = 1.0 / i + go (i+1.0)
        n' = fromIntegral n

-- not here
phi :: Double
phi = 0.5 * (1 + sqrt 5)

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

--------------------------------------------------------------------------
-- set cover problem
--------------------------------------------------------------------------
greedySetCover2 :: (Eq a) => [a] -> [[a]] -> [[a]]
greedySetCover2 u s = let u' = nub u
                          s' = nub $ map nub s
                       in joinSets u' $ sizeParts u' s'

joinSets :: (Eq a) => [a] -> [[[a]]] -> [[a]]
joinSets _ [] = []
joinSets u (x:xs) | null x    = joinSets u xs
                  | otherwise = go [head x] (head x) xs -- we just snatch the first
                                                        -- that can be certainly improved
  where go :: (Eq a) => [[a]] -> [a] -> [[[a]]] -> [[a]]
        go res _ [] = res
        go res p (y:ys) | length p >= length u = res
                        | null   y  = go res p ys
                        | not (null ys) && 
                          length (p ++ head y) > length u = go res p  ys
                        | otherwise = let y' = distOrd2 p y -- sort by highest distance to p
                                       in go ((head y'):res) (p ++ head y') ys

sizeParts :: (Eq a) => [a] -> [[a]] -> [[[a]]]
sizeParts _ [] = []
sizeParts u xs =
  let xs'      = sizeOrd xs
      p        = head xs'
      (r,rs)   = partition (\x -> length x == length p) xs'
      rr       = sizeParts u rs 
   in (distOrd u r):rr 

sizeOrd:: [[a]] -> [[a]]
sizeOrd  = sortBy (\x y -> length y `compare` length x)

distOrd, distOrd2 :: (Eq a) => [a] -> [[a]] -> [[a]]
distOrd  u = sortBy (\x y -> (dist u x) `compare` (dist u y))
distOrd2 u = sortBy (\x y -> (dist u y) `compare` (dist u x))

dist :: (Eq a) => [a] -> [a] -> Int
dist a b = length (a \\ b)

union :: (Eq a) => [a] -> [a] -> [a]
union a b = nub (a ++ b)

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] _ = []
intersect _ [] = []
intersect (a:as) bs | a `elem` bs = a : intersect as bs
                    | otherwise   =     intersect as bs

greedySetCover :: (Eq a) => [a] -> [[a]] -> [[a]]
greedySetCover u = loop (length u) [] 
  where loop _ _  []  = []
        loop m rs xs  = let (m',p) = best m rs [] xs
                         in if m' < m 
                             then p : loop m' (p `union` rs) (delete p xs)
                             else []
        best m r p []     = (m, p)
        best m r p (x:xs) = let m' = dist u (x `union` r)
                             in if m' < m then best m' r x xs
                                          else best m  r p xs

powerset1 :: (Eq a) => [a] -> [[a]]
powerset1 [] = [[]]
powerset1 (x:xs) = powerset1 xs ++ map (x:) (powerset1 xs)

powerset2 :: (Eq a) => [a] -> [[a]]
powerset2 [] = [[]]
powerset2 xs = let c = map (\_ -> False) xs in fill c []
  where fill c r = let s = addXth c xs
                    in if and c then s:r
                                else fill (inc c) (s:r)
        addXth [] _ = []
        addXth _ [] = []
        addXth (b:bs) (y:ys) | b = [y] ++ addXth bs ys
                             | otherwise = addXth bs ys

testinc :: [a] -> [Bool]
testinc xs = let c = map (\_ -> False) xs in go c
  where go c | and c     = c
             | otherwise = go (inc c)

inc :: [Bool] -> [Bool]
inc [] = []
inc [False] = [True]
inc [True]  = [True, False]
inc bs = let h = last bs
          in case inc [h] of
               [h1]   -> init bs       ++ [h1]
               [_,h2] -> inc (init bs) ++ [h2]

powerset3 :: (Eq a) => [a] -> [[a]]
powerset3 xs = go (length xs) 0
  where go l i | l == i    = [[]]
               | otherwise = let !x  = xs!!i 
                                 !is = map (x:) (go i 0)
                              in is ++ go l (i+1)

powerset4 :: (Eq a) => [a] -> [[a]]
powerset4 = filterM (const [False,True])

natLog :: Integer -> Integer -> (Integer,Integer)
natLog b n = let e = floor $ logBase (fromIntegral b) 
                                     (fromIntegral n)
              in (e, n - b^e)

binExp :: Integer -> [Integer] 
binExp 0 = []
binExp 1 = [0]
binExp n = let (!e, !r) = natLog 2 n
            in e : binExp r

powerset5 :: (Eq a) => [a] -> [[a]]
powerset5 [] = [[]]
powerset5 xs = go (2^(length xs)-1) 0
  where go n i | i == n    = [xs]
               | otherwise = let xx = binExp i
                                 s  = map (\x -> xs!!(fromIntegral x)) xx
                              in s:go n (i+1)

powerset6 :: (Eq a) => [a] -> [[a]]
powerset6 [] = [[]]
powerset6 xs = let v = V.fromList xs
                in go v (2^(length xs)-1) 0
  where go v n i | i == n    = [xs]
                 | otherwise = let !xx = binExp i
                                   !s  = map (\x -> v!(fromIntegral x)) xx
                                in s:go v n (i+1)
