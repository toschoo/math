import Linear
import Poly
import Control.Monad (forM)

main :: IO ()
main = do
  let m = 100
  t <- forM [1..m] (\i -> do
    p <- randomPoly 9 5 
    q <- randomPoly 9 4
    let s = sylvester p q
    putStrLn (show p)
    putStrLn (show q)
    putStrLn (show s)
    let d = det s
    let x = spgcd p q
    let r = head (coeffs (last x))
    putStrLn (show d ++ " = " ++ show r)
    putStrLn ("gcd " ++ show d ++ ", " ++ show r ++ " = " ++ show (gcd d r))
    if d == r then return True else return False)
  let cnt = length (filter (\x -> x) t)
  putStrLn ("true: " ++ show cnt ++ " | false: " ++ show (m - cnt))
  if and t then putStrLn "PASSED" else putStrLn "FAILED"

