{-# LANGUAGE GADTs #-}
module Perm
where

  import           Data.List (delete, nub)
  import qualified Data.Vector.Mutable as V
  import           System.Random
  import           Control.Applicative ((<$>))
  import           Control.Monad (when)
  import           Debug.Trace (trace)

  import Binom

  -------------------------------------------------------------------------
  -- insert an element in all possible positions of a given sequence
  -------------------------------------------------------------------------
  insall :: a -> [a] -> [[a]]
  insall p  []      =  [[p]]
  insall p  (x:xs)  =  (p:x:xs):[x:as | as <- insall p xs] 

  -------------------------------------------------------------------------
  -- create all possible permutations of a sequence
  -------------------------------------------------------------------------
  perms :: [a] -> [[a]]
  perms []  = [[]]
  perms (x:xs) = concatMap (insall x) $ perms xs

  -------------------------------------------------------------------------
  -- replace one element by another
  -------------------------------------------------------------------------
  replace :: (Eq a) => a -> a -> [a] -> [a] -> [a]
  replace _ _ [] _ = []
  replace p s (y:ys) (z:zs)  |  y == p     =  s:zs
                             |  otherwise  =  z:replace p s ys zs

  type Perm a = [[a]]

  -------------------------------------------------------------------------
  -- apply permutation given in cycle notation
  -------------------------------------------------------------------------
  permute :: (Eq a) => Perm a -> [a] -> [a]
  permute  []       xs   =  xs
  permute ([]:ps)   xs   =  permute ps xs
  permute ([p]:ps)  xs   =  permute ps xs
  permute (p:ps)    xs   =  permute ps $ orbit (head p) xs p
    where  orbit _ rs  []         =  rs
           orbit x rs [u]         =  replace u x xs rs
           orbit x rs (p1:p2:pp)  =  orbit x (replace p1 p2 xs rs) (p2:pp) 

  -------------------------------------------------------------------------
  -- create all possible permutations given in cycle notation
  -------------------------------------------------------------------------
  permsOfCycle :: (Eq a) => Perm a -> [a] -> [[a]]
  permsOfCycle os xs = [permute o xs | o <- permOrbits os] 

  -------------------------------------------------------------------------
  -- create all possible permutations with k cycles
  -------------------------------------------------------------------------
  permsWithCycles :: (Eq a) => Int -> [a] -> [[a]]
  permsWithCycles k xs = concat [
    permsOfCycle x xs | x <- nPartitions k xs]

  -------------------------------------------------------------------------
  -- permute an orbit
  -------------------------------------------------------------------------
  permOrbits :: (Eq a) => Perm a -> [Perm a]
  permOrbits [] = [[]]
  permOrbits (o:oo) = concat [map (:x) (oPerms o) | x <- permOrbits oo] 
    where oPerms [] = [] -- head keeps in front 
          oPerms (x:xs) = [x:ps | ps <- perms xs] 

  -------------------------------------------------------------------------
  -- partition set in 2 subsets                     
  -------------------------------------------------------------------------
  partIn2 :: (Eq a) => [a] -> [([a],[a])]
  partIn2 = fltr . p2 
    where p2 [] = [([],[])]
          p2 (x:xs) = [(x:a,b) | (a,b) <- p2 xs] ++ 
                 fltr [(a,x:b) | (a,b) <- p2 xs]
          fltr = filter (\p -> not (null (fst p) || null (snd p)))

  partX2 :: (Eq a) => [a] -> [[[a]]]
  partX2 = fltr . p2
    where p2 [] = [[[],[]]]
          p2 (x:xs) = [[x:a,b] | [a,b] <- p2 xs] ++
                 fltr [[a,x:b] | [a,b] <- p2 xs]
          fltr = filter (\[a,b] -> not (null a || null b))

  -------------------------------------------------------------------------
  -- partition set in k subsets                     
  -------------------------------------------------------------------------
  partIn :: (Eq a) => Int -> [a] -> [[[a]]]
  partIn n = fltr . p
    where p [] = [take n $ repeat []]
          p (x:xs) = condense $ concatMap (insone x) $ p xs 
          fltr = filter (all (not . null))

  condense :: (Eq a) => [[[a]]] -> [[[a]]] 
  condense [] = []
  condense (x:xs) = x : condense (delPerms (perms x) xs)
    where delPerms [] xs = xs
          delPerms _  [] = []
          delPerms (p:ps) xs = delete p $ delPerms ps xs

  insone :: (Eq a) => a -> [[a]] -> [[[a]]]
  insone _ [] = []
  insone p (x:xs) = [(p:x):xs] ++ map (x:) (insone p xs)

  -------------------------------------------------------------------------
  -- Restricted Growth String
  -------------------------------------------------------------------------
  nPartitions :: (Eq a) => Int -> [a] -> [[[a]]]
  nPartitions _ [] = []
  nPartitions 1 xs = [[xs]]
  nPartitions k xs  | k >= length xs = [[[x] | x <- xs]]
                    | otherwise      = go $ countRgs k $ length xs
    where go []     = []
          go (r:rs) = let ps = take k $ repeat [] in rgs2set r xs ps : go rs
 
  rgs2set :: (Eq a) => [Int] -> [a] -> [[a]] -> [[a]]
  rgs2set [] _  ps         = ps
  rgs2set _  [] ps         = ps
  rgs2set (r:rs) (x:xs) ps = rgs2set rs xs $ ins r x ps
    where ins _ _ []       = undefined
          ins 0 p (z:zs)   = (p:z) : zs
          ins i p (z:zs)   = z : ins (i-1) p zs 
  
  -------------------------------------------------------------------------
  -- count number 1..k^n as restricted growth strings
  -------------------------------------------------------------------------
  countRgs :: Int -> Int -> [[Int]]
  countRgs 1 n                =  [rgs 1 n 1]
  countRgs b n  |  b == n     =  [[0..b-1]]
                |  b >  n     =  []
                |  otherwise  =  go 1
    where go i = let (j,r) = toRgsN b n i 
                  in if head r /= 0 then []
                       else r : go (j+1)

  toRgs :: ([Int] -> Bool) -> Int -> Int -> Int -> (Int, [Int])
  toRgs rst b n i = go $ rgs b n i
    where go r | not (rst r) = toRgs rst b n (i + 1)
               | otherwise   = (i,r)

  toRgsN :: Int -> Int -> Int -> (Int, [Int])
  toRgsN b = toRgs rst b
    where rst r = rGrowth r && hasN  b r
  
  rGrowth :: [Int] -> Bool
  rGrowth [] = True
  rGrowth (x:xs) = go x xs
    where go _ [] = True
          go d (z:zs) = if z - d > 1 then False
                          else let d' = max d z
                                in go d' zs
  
  hasN :: Int -> [Int] -> Bool
  hasN b r = length (nub r) == b

  toN :: Int -> Int -> [Int]
  toN b = reverse . go 
    where go x = case x `quotRem` b of
                  (0,r) -> [r]
                  (q,r) -> r : go q

  rgs :: Int -> Int -> Int -> [Int]
  rgs b m n = let r = toN b n
                  d = m - length r 
                  p = if d > 0 then take d $ repeat 0 else []
               in if d < 0 then [] else p ++ r

  ------------------------------------------------------------------------
  -- Fisher-Yates (or Knuth) Shuffle
  ------------------------------------------------------------------------
  kshuffle :: [a] -> IO [a]
  kshuffle xs = do is <- idxperm (length xs) 0
                   vs <- createVector xs
                   go 0 is vs
                   vector2list vs (length xs) 
    where idxperm n k | k == n     = return [] 
                      | otherwise  = do i <- randomRIO (0,n-1)
                                        (i:) <$> idxperm n (k+1)
          go _ [] _      = return ()
          go k (i:is) vs = when (k /= i) (V.unsafeSwap vs k i)
                           >> go (k+1) is vs

  createVector :: [a] -> IO (V.IOVector a)
  createVector xs = do v <- V.new (length xs)
                       initV v 0 xs
                       return v
    where initV _ _ []     = return ()
          initV v i (z:zs) = V.unsafeWrite v i z
                             >> initV v (i+1) zs

  vector2list :: V.IOVector a -> Int -> IO [a]
  vector2list v n = go 0
    where -- go :: Int -> IO [a]
          go i | i == n    = return []
               | otherwise = do x <- V.unsafeRead v i
                                (x:) <$> go (i+1) 
                  
  -------------------------------------------------------------------------
  -- Stirling Numbers
  -------------------------------------------------------------------------
  stirling1 :: Integer -> Integer -> Integer
  stirling1 0 _ = 0
  stirling1 1 1 = 1
  stirling1 n k | k > n     = 0
                | otherwise = (n-1) * (stirling1 (n-1) k) + 
                                      (stirling1 (n-1) (k-1))

  stirlings1 :: Integer -> [Integer]
  stirlings1 n = map (stirling1 n) [1..n]

  stirling2 :: Integer -> Integer -> Integer
  stirling2 0 _ = 0
  stirling2 1 1 = 1
  stirling2 n k | k > n     = 0
                | otherwise = k * (stirling2 (n-1) k) +
                                  (stirling2 (n-1) (k-1))

  stirlings2 :: Integer -> [Integer]
  stirlings2 n = map (stirling2 n) [1..n]

  stst :: Integer -> Integer -> Integer
  stst n k = sum $ map (\j -> (stirling1 n j) * (stirling2 j k)) [0..n]

  symmetric1 :: Integer -> Integer -> Integer
  symmetric1 = symmetric stirling2 

  symmetric2 :: Integer -> Integer -> Integer
  symmetric2 = symmetric stirling1 

  symmetric :: (Integer -> Integer -> Integer) -> 
                Integer -> Integer -> Integer
  symmetric s n k = sum $ map f [0..n-k]
    where f j = (-1)^j * choose (n-1+j) (n-k+j) 
                       * choose (2*n-k) (n-k-j) 
                       * s (n-k+j) j

  -------------------------------------------------------------------------
  -- Eulerian Numbers
  -------------------------------------------------------------------------
  eulerian1 :: Integer -> Integer -> Integer
  eulerian1 0 _ = 1
  eulerian1 _ 0 = 1
  eulerian1 n m | m == n - 1 = 1
                | otherwise  =   (n-m) * eulerian1 (n-1) (m-1)
                               + (m+1) * eulerian1 (n-1)  m

  eu1closed :: Integer -> Integer -> Integer
  eu1closed n m = go 0
    where go k = let a = (-1)^k
                     b = choose (n+1) k
                     c = (m + 1 - k)^n
                  in (a * b * c) + if k == m then 0 else go (k+1)

  eu1sum :: Integer -> Integer -> [Integer]
  eu1sum n m = go 0
    where go k = let a = (-1)^k
                     b = choose (n+1) k
                     c = (m + 1 - k)^n
                  in (a * b * c) : if k == m then [] else go (k+1)

  eu1Tri :: Integer -> [[Integer]]
  eu1Tri n = map row [1..n]
    where row k = map (eulerian1 k) [0..k-1]

  doublefac :: Integer -> Integer
  doublefac 0 = 1
  doublefac 1 = 1
  doublefac n = n * doublefac (n - 2)

  eulerian2 :: Integer -> Integer -> Integer
  eulerian2 0 0 = 1
  eulerian2 0 _ = 0
  eulerian2 n m = (2*n-m-1) * eulerian2 (n-1) (m-1)
                  +   (m+1) * eulerian2 (n-1)  m

  eu2Tri :: Integer -> [[Integer]]
  eu2Tri n = map row [1..n]
    where row k = map (eulerian2 k) [0..k-1]

  -------------------------------------------------------------------------
  -- Powersets
  -------------------------------------------------------------------------
  powset :: (Eq a) => [a] -> [[a]]
  powset xs = go xs [[]]
    where go [] rs = rs
          go (y:ys) rs = go ys (ins y rs)
          ins y [] = []
          ins y (s:ss) = (y:s):s:ins y ss

  -------------------------------------------------------------------------
  -- all combinations of sets (cartesian product)
  -------------------------------------------------------------------------
  listcombine :: [[a]] -> [[a]]
  listcombine [] = []
  listcombine ([]:_) = []
  listcombine (x:xs) = inshead (head x) (listcombine xs) ++ 
                       listcombine ((tail x):xs)

  inshead :: a -> [[a]] -> [[a]]
  inshead x [] = [[x]]
  inshead x zs = map (x:) zs

  listcombine2 :: [a] -> [a] -> [[a]]
  listcombine2 as bs = concat [listcombine1 a bs | a <- as]

  listcombine1 :: a -> [a] -> [[a]]
  listcombine1 a bs = [[a,b] | b <- bs] 

