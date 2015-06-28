module Shor
where

  import Control.Applicative ((<$>))
  import Control.Monad (unless)
  import System.Random
  import Prime
  import Modular
  import Debug.Trace (trace)

  ------------------------------------------------------------------------
  -- Shor's factorisation algorithm
  ------------------------------------------------------------------------
  shorfact :: Integer -> IO [Integer]
  shorfact 0 = return []
  shorfact 1 = return []
  shorfact 2 = return [2]
  shorfact n | n < 0     = do fs <- shorfact (-n)
                              return fs
             | even n    = do fs <- shorfact (n `div` 2) 
                              return (2:fs)
             | otherwise = do p <- rabinMiller 64 n
                              if p then return [n] else loop
    where loop = do a <- randomRIO (2,n-2)
                    let f = gcd n a 
                    if f /= 1 then do fs2 <- shorfact (n `div` f)
                                      fs1 <- shorfact f
                                      return (fs1 ++ fs2)
                              else check a (order n a)
          check a t | odd t = loop
                    | (a^(t `div` 2)) `rem` n == n-1 = loop
                    | otherwise = let f1 = gcd n (a^(t `div` 2) - 1) 
                                      f2 = gcd n (a^(t `div` 2) + 1)
                                      f  = if f1 == 1 then f2 else f1
                                   in do fs1 <- shorfact (n `div` f) 
                                         fs2 <- shorfact f
                                         return (fs1 ++ fs2)

  ------------------------------------------------------------------------
  -- the quantum part
  ------------------------------------------------------------------------
  order :: Integer -> Integer -> Integer
  order n a = go 2 (a*a)
    where go r a' | a' `rem` n == 1 = r
                  | r >= n          = error "group exhausted"
                  | otherwise       = go (r+1) (a'*a)
