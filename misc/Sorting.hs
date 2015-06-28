--------------------------------------------------------------------------
-- Several algorithms
--------------------------------------------------------------------------
import Debug.Trace (trace)
import Data.List (nub)

import Perm

--------------------------------------------------------------------------
-- Mergesort
--------------------------------------------------------------------------
mergesort :: (Ord a) => [a] -> [a]
mergesort l = let rs = runs l 
                  n' = length l
                  n  = if even n' then n' else n'+1
               in if length rs > (n `div` 2) + 1 
                    then multimerge $ runs (reverse l) 
                    else multimerge rs

multimerge :: (Ord a) => [[a]] -> [a]
multimerge []       = []
multimerge [xs]     = xs
multimerge (x:y:zs) = merge (merge x y) 
                            (multimerge zs)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

--------------------------------------------------------------------------
-- Bubblesort
--------------------------------------------------------------------------
bubblesort :: (Ord a) => [a] -> [a]
bubblesort l = case bubble False l of
                 (l',False) -> l'
                 (l',True ) -> bubblesort l'

bubble :: (Ord a) => Bool -> [a] -> ([a],Bool)
bubble t []  = ([],t)
bubble t [x] = ([x],t) 
bubble t (x:y:zs) | x > y     = let (l,t') = bubble True (x:zs) in (y:l,t')
                  | otherwise = let (l,t') = bubble t    (y:zs) in (x:l,t')

--------------------------------------------------------------------------
-- Quicksort
--------------------------------------------------------------------------
quicksort :: (Ord a) => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort lft ++ [x] ++ quicksort rgt
  where lft = [z | z <- xs, z <= x]
        rgt = [z | z <- xs, z >  x]

--------------------------------------------------------------------------
-- run + stepdown
--------------------------------------------------------------------------
runs :: (Ord a) => [a] -> [[a]]
runs [] = []
runs xs = let (r,zs) = run xs in r : runs zs
  where run :: (Ord a) => [a] -> ([a],[a])
        run []  = ([], [])
        run [x] = ([x],[])
        run (x:y:zs) | x > y     = ([x],y:zs)
                     | otherwise = let (r,ys) = run (y:zs) in (x:r,ys)

--------------------------------------------------------------------------
-- Measure of disorder
--------------------------------------------------------------------------
measure :: (Ord a) => [a] -> (Int,Int)
measure l = (10 * go l           `div` length l, 
             10 * go (reverse l) `div` length l)
  where go []  = 0
        go [x] = 0
        go (x:y:zs) | x > y     = 1 + go (y:zs)
                    | otherwise =     go (y:zs)

----------------------------------------------------------------------------
-- Theorem:
-- The sum of the runs forward and backward of a list with n elements is n+1
----------------------------------------------------------------------------
-- holds only for lists with unique elements.
-- With repetitions, we may have, e.g. [1,2,2,2,3]:
-- [2,3,2,1,2] -> 2 3 | 2 | 1 2 and reversed:
-- [2,1,2,3,2] -> 2 | 1 2 3 | 2
----------------------------------------------------------------------------
-- Conjecture:
-- The sum of the runs foward and backward of a list 
-- with n distinct elements >= n+1
----------------------------------------------------------------------------
runs2 :: (Ord a) => [a] -> ([[a]],[[a]])
runs2 xs = (runs xs, runs $ reverse xs)

bothruns :: (Ord a) => [a] -> (Int,Int)
bothruns xs = (length (runs xs), length (runs $ reverse xs))

allboth :: (Ord a) => [a] -> [(Int,Int)]
allboth = nub . map bothruns . perms

occount :: (Eq a) => [a] -> [(Int,a)]
occount [] = []
occount (x:xs) = (1 + count x xs, x) : occount (del x xs)
  where count _ [] = 0
        count p (z:zs) = let k | p == z    = 1 
                               | otherwise = 0
                          in k + count p zs
        del _ [] = []
        del p (x:xs) | p == x    = del p xs
                     | otherwise = x : del p xs

permsPerRuns :: (Ord a) => [a] -> [(Int,Int)]
permsPerRuns = occount . map length . map runs . perms

selBoth :: (Ord a) => (Int,Int) -> [a] -> [[a]]
selBoth (f,b) = map (concat . fst) . filter flt . map runs2 . perms 
  where flt (x,y) = (length x, length y) == (f,b)

---------------------------------------------------------------------------
-- Number of permutations of n elements that have k "descents" 
-- and, hence, k+1 runs
---------------------------------------------------------------------------
euler :: Integer -> Integer -> Integer
euler n 0 = 1
euler n k | k >= n        = 0
          | k > n `div` 2 = euler n (n - 1 - k)
          | otherwise = (k+1) * (euler (n-1) k) +
                        (n-k) * (euler (n-1) (k-1))

