module Fib
where

  import Data.List (foldl')
  import Debug.Trace (trace)
  import Text.Printf

  ------------------------------------------------------------------------
  -- Naive fibonacci
  ------------------------------------------------------------------------
  fib :: Integer -> Integer
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1)

  ------------------------------------------------------------------------
  -- The Golden Ratio
  ------------------------------------------------------------------------
  phi :: Double
  phi = 0.5 * (1 + sqrt 5)

  ------------------------------------------------------------------------
  -- Conjugate of the Golden Ratio
  ------------------------------------------------------------------------
  phi' :: Double
  phi' = 1 - phi

  ------------------------------------------------------------------------
  -- Compute the golden ratio for a
  ------------------------------------------------------------------------
  golden :: Double -> Double
  golden a = phi*a

  ------------------------------------------------------------------------
  -- Integers around the nth fibonacci number
  ------------------------------------------------------------------------
  fibApx :: Integer -> (Integer, Integer)
  fibApx n = let n' = fromIntegral n 
              in (floor   $ phi^(n'-2), 
                  ceiling $ phi^(n'-1))

  ------------------------------------------------------------------------
  -- Smart fibonacci
  ------------------------------------------------------------------------
  fi :: Integer -> Integer
  fi n = round (phi^n/sqrt 5)

  ------------------------------------------------------------------------
  -- Complete formula
  ------------------------------------------------------------------------
  fi2 :: Integer -> Integer
  fi2 n = round ((phi^n - phi'^n)/sqrt 5)

  ------------------------------------------------------------------------
  -- Double
  ------------------------------------------------------------------------
  fir :: Integer -> Double 
  fir n = (phi^n)/sqrt 5

  ------------------------------------------------------------------------
  -- Double
  ------------------------------------------------------------------------
  fir2 :: Integer -> Double 
  fir2 n = (phi^n - phi'^n)/sqrt 5

  ------------------------------------------------------------------------
  -- Double
  ------------------------------------------------------------------------
  fratio :: Integer -> Double
  fratio n = np / nn
    where np = fromInteger (fi (n+1))
          nn = fromInteger (fi n)

  ------------------------------------------------------------------------
  -- Some experiments with the generating function
  ------------------------------------------------------------------------
  g :: Double -> Double
  g x = x / (1 - x - x^2)

  g' :: Double -> Double
  g' x = (1/(sqrt 5)) * (1/(1-x*phi) - 1/(1-x*phi'))

  ------------------------------------------------------------------------
  -- Write a program that shows the coefficients for
  -- phi^n and phi'^n, such that
  -- phi x phi = (3 + b) / 2, etc. for b = sqrt(5)
  ------------------------------------------------------------------------
  data Phi a = Phi a a a
    deriving (Show,Eq)

  one :: Phi Integer
  one = Phi 1 1 2

  one' :: Phi Integer
  one' = Phi 1 (-1) 2

  mkPhi :: (Num a, Integral a) => a -> a -> a -> Phi a
  mkPhi a b c = Phi (a `div` g) (b `div` g) (c `div` g)
    where k = gcd a c
          m = gcd b c
          g = gcd k m

  dbgPhi :: (Num a, Integral a) => a -> a -> a -> Phi a
  dbgPhi a b c = Phi a b c

  mul :: (Num a, Integral a) => Phi a -> Phi a -> Phi a
  mul (Phi a b c) (Phi d e f) =
    mkPhi (a*d + 5*b*e) (a*e + b*d) (c*f)

  pow :: Phi Integer -> Int -> Phi Integer
  pow p n = foldl' mul p $ take (n-1) (repeat p)

  add :: (Num a, Integral a) => Phi a -> Phi a -> Phi a
  add (Phi a b c) (Phi d e f) =
    mkPhi (f*a + c*d) (f*b + c*e) (c*f)

  neg :: (Num a, Integral a) => Phi a -> Phi a
  neg (Phi a b c) = mkPhi (-a) (-b) c

  pair :: Int -> (Phi Integer, Phi Integer)
  pair n = (pow one n, pow one' n)

  triple :: Int -> (Phi Integer, Phi Integer, Phi Integer)
  triple n = (p, q, d)
    where p = pow one n
          q = pow one' n
          d = add p (neg q)

  ------------------------------------------------------------------------
  -- Pretty printers
  ------------------------------------------------------------------------
  pretty :: (PrintfArg a) => Phi a -> String
  pretty (Phi a b c) = 
    printf "(%+06d " a ++
    printf "%+06d "  b ++
    printf "%+06d)"  c

  pretty1 :: (PrintfArg a) => Phi a -> String
  pretty1 (Phi a _ _) = 
    printf "%+06d"  a

  pretty2 :: (PrintfArg a) => Phi a -> String
  pretty2 (Phi _ b _) = 
    printf "%+06d"  b

  pretty3 :: (PrintfArg a) => Phi a -> String
  pretty3 (Phi _ _ c) = 
    printf "%+06d"  c

  printp :: Int -> IO ()
  printp n = mapM_ pr (map triple [1..n])
    where pr = putStrLn . sh
          sh (a,b,c) = pretty a ++ " " ++ 
                       pretty b ++ " " ++
                       pretty c

  print1 :: Int -> IO ()
  print1 n = mapM_ pr (map triple [1..n])
    where pr = putStrLn . sh
          sh (a,b,c) = pretty1 a ++ " " ++
                       pretty1 b ++ " " ++
                       pretty1 c

  print2 :: Int -> IO ()
  print2 n = mapM_ pr (map triple [1..n])
    where pr = putStrLn . sh
          sh (a,b,c) = pretty2 a ++ " " ++
                       pretty2 b ++ " " ++
                       pretty2 c

  print3 :: Int -> IO ()
  print3 n = mapM_ pr (map triple [1..n])
    where pr = putStrLn . sh
          sh (a,b,c) = pretty3 a ++ " " ++
                       pretty3 b ++ " " ++
                       pretty3 c

  printr :: Int -> IO ()
  printr n = mapM_ pr (map triple [1..n])
    where pr = putStrLn . sh
          sh = show . ratio
          ratio (Phi _ a _, _, Phi _ c _ ) 
            | a == c    = 1
            | 2*a == c  = 2
            | otherwise = 0

