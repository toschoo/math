{-# LANGUAGE BangPatterns #-}
module Main
where

  import System.Exit
  import Test.QuickCheck
  import Test.QuickCheck.Monadic
  import Control.Applicative ((<$>))
  import Control.Monad (when)
  import Data.List (nub, sort, delete)
  import Debug.Trace (trace)

  import Natural
  import Zahl

  ------------------------------------------------------------------------------
  -- basic arithmetic operations, Int only!
  ------------------------------------------------------------------------------
  prpPlus  :: Integer -> Integer -> Bool
  prpPlus a b = let si = a + b
                    sn = withInt a b (+)
                 in si == fromIntegral sn

  prpMinus :: Integer -> Integer -> Bool
  prpMinus a b = let di = a - b
                     dn = withInt a b (-)
                  in di == fromIntegral dn

  prpMul :: Integer -> Integer -> Bool
  prpMul a b = let mi = a * b
                   mn = withInt a b (*)
                in mi == fromIntegral mn

  prpDist :: Integer -> Integer -> Integer -> Bool
  prpDist a b c = let r1 = (withInt c a (*)) + 
                           (withInt c b (*))
                      r2 = (withInt c (a+b) (*))
                   in if r1 /= r2 then trace (show r1 ++ ", " ++ show r2) False
                                  else r1 == r2

  prpQR  :: Integer -> Integer -> Bool
  prpQR  a 0 = prpQR a 1
  prpQR  a b = let (qi,ri) = quotRem a b
                   (qn,rn) = withInt a b quotRem 
                in qi == (fromIntegral qn) && ri == (fromIntegral rn)

  prpMulQR  :: Integer -> Integer -> Bool
  prpMulQR  a 0 = prpMulQR a 1
  prpMulQR  a b = let (qn,rn) = withInt a b quotRem 
                      c = qn * (fromIntegral b) + rn
                   in a == fromIntegral c

  prpRem :: Integer -> Integer -> Bool
  prpRem a 0 = prpRem a 1
  prpRem a b = let ri = rem a b
                   rn = withInt a b rem 
                in ri == (fromIntegral rn) 

  prpGcd :: Integer -> Integer -> Bool
  prpGcd a b = let ci = gcd a b
                   cn = withInt a b gcd 
                in ci == (fromIntegral cn) 

  withInt :: Integer -> Integer -> (Zahl -> Zahl -> r) -> r
  withInt x y f = let a | x < 0     = Neg (fromIntegral (-x))
                        | otherwise = Pos (fromIntegral x) 
                      b | y < 0     = Neg (fromIntegral (-y))
                        | otherwise = Pos (fromIntegral y)
                   in f a b

  -------------------------------------------------------------
  -- controlled quickcheck, arbitrary tests
  -------------------------------------------------------------
  deepCheck :: (Testable p) => p -> IO Result
  deepCheck = quickCheckWithResult stdArgs{maxSuccess=10000}

  someCheck :: (Testable p) => Int -> p -> IO Result
  someCheck n = quickCheckWithResult stdArgs{maxSuccess=n}

  -------------------------------------------------------------
  -- do just one test
  -------------------------------------------------------------
  oneCheck :: (Testable p) => p -> IO Result
  oneCheck = quickCheckWithResult stdArgs{maxSuccess=1}

  -------------------------------------------------------------
  -- combinator, could be a monad...
  -------------------------------------------------------------
  applyTest :: IO Result -> IO Result -> IO Result
  applyTest r f = do
    r' <- r
    case r' of
      Success{} -> f
      x         -> return x

  infixr ?>
  (?>) :: IO Result -> IO Result -> IO Result
  (?>) = applyTest

  checkAll :: IO ()
  checkAll = do
    let good = "OK. All Tests passed."
    let bad  = "Bad. Some Tests failed."
    r <- deepCheck prpPlus       ?>
         deepCheck prpMinus      ?>
         deepCheck prpMul        ?> 
         deepCheck prpDist       ?>
         deepCheck prpQR         ?> 
         deepCheck prpMulQR      ?> 
         deepCheck prpRem        ?> 
         deepCheck prpGcd  
    case r of
      Success{} -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
