{-# Language BangPatterns #-}

module Main
where

  powerset3 :: (Eq a) => [a] -> [[a]]
  powerset3 xs = go (length xs) 0
    where go l i | l == i    = [[]]
                 | otherwise = let x  = xs!!i 
                                   is = map (x:) (go i 0)
                                in is ++ go l (i+1)

  powerset1 :: (Eq a) => [a] -> [[a]]
  powerset1 [] = [[]]
  powerset1 (x:xs) = powerset1 xs ++ map (x:) (powerset1 xs)

  main :: IO ()
  main = print $ length $ powerset3 [1..20]
