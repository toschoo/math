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
  import Quoz hiding ((%))
  import qualified Data.Ratio as R
  import Data.Ratio ((%))

  -- import Numbers (root2)

  ------------------------------------------------------------------------------
  -- basic arithmetic operations, Int only!
  ------------------------------------------------------------------------------
  prpRat :: Integer -> Integer -> Bool
  prpRat a b = let n | a < 0     = abs a
                     | otherwise = a
                   d | b < 0     = abs b
                     | b == 0    = 1
                     | otherwise = b
                   r = n % d
                   q = withInt n d ratio 
                in case q of
                     Q n' d' -> R.numerator   r == (fromIntegral n') &&
                                R.denominator r == (fromIntegral d')
                   
  prpAdd :: Integer -> Integer -> Integer -> Integer -> Bool
  prpAdd a 0 c d  = prpAdd a 1 c d
  prpAdd a b c 0  = prpAdd a b c 1
  prpAdd a b c d  = let xr = a % b
                        yr = c % d
                        r  = xr + yr
                        q  = withRatio a b c d (+)
                     in cmq r q
                   
  prpSub :: Integer -> Integer -> Integer -> Integer -> Bool
  prpSub a 0 c d  = prpSub a 1 c d
  prpSub a b c 0  = prpSub a b c 1
  prpSub a b c d  = let xr = a % b
                        yr = c % d
                        r  = xr - yr
                        q  = withRatio a b c d (-)
                     in cmq r q
 
  prpMul :: Integer -> Integer -> Integer -> Integer -> Bool
  prpMul a 0 c d  = prpMul a 1 c d
  prpMul a b c 0  = prpMul a b c 1
  prpMul a b c d  = let xr = a % b
                        yr = c % d
                        r  = xr * yr
                        q  = withRatio a b c d (*)
                     in cmq r q
                   
  prpDist :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Bool
  prpDist a 0 c d e f = prpDist a 1 c d e f
  prpDist a b c 0 e f = prpDist a b c 1 e f
  prpDist a b c d e 0 = prpDist a b c d e 1
  prpDist a b c d e f = let r1 = withRatio e f a b (*) +
                                 withRatio e f c d (*)
                            r2 = withRatio a b c d (+) * 
                                 toRatio   e f
                         in r1 == r2
                   
  prpDiv :: Integer -> Integer -> Integer -> Integer -> Bool
  prpDiv a 0 c d  = prpDiv a 1 c d
  prpDiv a b 0 d  = prpDiv a b 1 d
  prpDiv a b c 0  = prpDiv a b c 1
  prpDiv a b c d  = let xr = a % b
                        yr = c % d
                        r  = xr / yr
                        q  = withRatio a b c d (/)
                     in cmq r q

  cmq :: R.Rational -> Quoz -> Bool
  cmq r q = case q of
              Pos (Q nu de) -> abs (R.numerator   r) == (fromIntegral nu) &&
                               abs (R.denominator r) == (fromIntegral de) &&
                               ((R.numerator r >= 0 && R.denominator r > 0) ||
                                (R.numerator r <  0 && R.denominator r < 0))
              Neg (Q nu de) -> abs (R.numerator   r) == (fromIntegral nu) &&
                               abs (R.denominator r) == (fromIntegral de) &&
                               ((R.numerator r < 0 || R.denominator r < 0) &&
                                not (R.numerator r < 0 && R.denominator r < 0))


  withInt :: Integer -> Integer -> (Natural -> Natural -> r) -> r
  withInt a b f = f (fromIntegral a) (fromIntegral b)

  toRatio :: Integer -> Integer -> Quoz
  toRatio n d = withRatio n d 1 1 (*)

  withRatio :: Integer -> Integer -> Integer -> Integer -> 
               (Quoz  -> Quoz  -> Quoz ) -> Quoz 
  withRatio n1 d1 n2 d2 f = let cr1 | (n1 > 0 && d1 < 0) ||
                                      (n1 < 0 && d1 > 0) = neg0
                                    | otherwise        = Pos
                                r1 = cr1 ( ratio (fromIntegral (abs n1))
                                                 (fromIntegral (abs d1)))
                                cr2 | (n2 < 0 && d2 > 0) ||
                                      (n2 > 0 && d2 < 0) = neg0
                                    | otherwise        = Pos
                                r2 = cr2 ( ratio (fromIntegral (abs n2))
                                                 (fromIntegral (abs d2)))
                             in f r1 r2 

  {-
  prpSub :: Integer -> Integer -> Bool
  prpSub = withAbs (\a b ->
             let x = max a b
                 y = min a b 
              in x - y == n2Integer (int2n x `sub2` int2n y))

  prpAddSub :: Integer -> Integer -> Bool
  prpAddSub = withInts (\x y ->
                let s = int2n x `add2` int2n y
                 in s `sub` int2n y == int2n x &&
                    s `sub` int2n x == int2n y)

  prpMul :: Integer -> Integer -> Bool
  prpMul = withInts (\x y -> 
             x * y == n2Integer (int2n x `mul2` int2n y)) 
  -}

  withInts :: (Integer -> Integer -> Bool) -> Integer -> Integer -> Bool
  withInts f a b = let x = if abs a >= 1000 then 999 else abs a
                       y = if abs b >= 1000 then 999 else abs b
                    in f x y

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
    r <- deepCheck prpRat      ?> 
         deepCheck prpAdd      ?> 
         deepCheck prpSub      ?> 
         deepCheck prpMul      ?> 
         deepCheck prpDiv      ?> 
         someCheck 100 prpDist  
    case r of
      Success{} -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
