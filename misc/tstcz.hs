{-# Language BangPatterns #-}
module Main
where 

  import Poly

  main :: IO ()
  main = do 
    !t <- deepCantorZass 1000
    if t then putStrLn "passed" else putStrLn "failed"
