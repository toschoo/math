module Main
where 

  import Poly

  main :: IO ()
  main = do 
    t <- tstCantorZass 100 7
    if t then putStrLn "passed" else putStrLn "failed"
