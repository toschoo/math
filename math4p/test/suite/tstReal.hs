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
  import Realrep
  import Real hiding (rdiv)

  tol,tol5 :: Double
  tol  = 1/10^12
  tol5 = 1/10000

  prpIntDiv :: Integer -> Integer -> Bool
  prpIntDiv = withInts ndiv

  prpDoubleDiv :: Integer -> Integer -> Bool
  prpDoubleDiv = withDoubles ddiv5

  prpRealDoubleDiv :: Double  -> Double  -> Bool
  prpRealDoubleDiv = withRealDoubles ddiv

  -- The rules a / b * b = a and a * b / b = a
  --     do not hold with doubles
  prpJustDoubles :: Double -> Double -> Bool
  prpJustDoubles x y | x < 0.000001 = prpJustDoubles 0.1 y
                     | y < 0.000001 = prpJustDoubles x 0.1
                     | otherwise    =
                       let q = x / y
                           p = x * y
                        in q * y == x && p / y == x
    
  
  ndiv :: Natural -> Natural -> Bool
  ndiv x 0 = ndiv x 1
  ndiv x y = let a = real x 0
                 b = real y 0
                 c = fromIntegral x :: Double
                 d = fromIntegral y :: Double
                 r = r2d (a/b)
                 p = c/d
              in r > p - tol && r < p + tol

  ddiv5 :: Double -> Double -> Bool
  ddiv5 x 0 = ddiv x 1
  ddiv5 x y = let a = real (fromIntegral $ round (10^5 * x)) 5
                  b = real (fromIntegral $ round (10^5 * y)) 5
                  r = r2d (a / b)
               in r > (x/y) - tol5 && r < (x/y) + tol5

  ddiv :: Double -> Double -> Bool
  ddiv x 0 = ddiv x 1
  ddiv x y = let a = real (fromIntegral $ round (10^17 * x)) 17
                 b = real (fromIntegral $ round (10^17 * y)) 17
                 r = r2d (a / b)
                 t = r > (x/y) - tol && r < (x/y) + tol
              in if not t then trace (show r ++ ": " ++ show x ++ "/" ++ show y ++ "=" ++ show (x/y)) t
                          else t
                 
  withInts :: (Natural -> Natural -> Bool) -> Integer -> Integer -> Bool
  withInts f a b = let x = if abs a >= 1000 then 999 else abs a
                       y = if abs b >= 1000 then 999 else abs b
                    in f (fromInteger x) (fromInteger y)
                 
  withDoubles :: (Double -> Double -> Bool) -> Integer -> Integer -> Bool
  withDoubles f 0 b = withDoubles f 123456 b
  withDoubles f a 0 = withDoubles f a 123456
  withDoubles f a b | abs a >= 1000000 = withDoubles f (a `div` 10) b 
                    | abs a <  1       = withDoubles f (a*10)  b
                    | abs b >= 1000000 = withDoubles f a (b `div` 10)
                    | abs b <  1       = withDoubles f a (b*10)
                    | otherwise        = let x = (fromIntegral a) / 10^4
                                             y = (fromIntegral b) / 10^4
                                          in f (abs x) (abs y)
                 
  withRealDoubles :: (Double -> Double -> Bool) -> Double -> Double -> Bool
  withRealDoubles f 0 b = withRealDoubles f 1.23456 b
  withRealDoubles f a 0 = withRealDoubles f a 1.23456
  withRealDoubles f a b | abs a >= 1000000 = withRealDoubles f (a/10) b 
                        | abs a <  0.1     = withRealDoubles f 1 b
                        | abs b >= 1000000 = withRealDoubles f a (b/10)
                        | abs b <  0.1     = withRealDoubles f a 1 
                        | otherwise        = f (abs a) (abs b)

  withIntsP :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Property
  withIntsP f a b = let x = if abs a > 100 then 100 else abs a
                        y = if abs b > 100 then 100 else abs b
                     in collect (x,y) $ f x y

  withAbs :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Bool
  withAbs f a b = f (abs a) (abs b)

  -------------------------------------------------------------
  -- controlled quickcheck, arbitrary tests
  -------------------------------------------------------------
  deepCheck :: (Testable p) => p -> IO Result
  deepCheck = quickCheckWithResult stdArgs{maxSuccess=1000}

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
    r <- -- deepCheck prpJustDoubles
         deepCheck prpIntDiv    ?>
         deepCheck prpDoubleDiv ?>
         deepCheck prpRealDoubleDiv -- ?>
    case r of
      Success{} -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
