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

  ------------------------------------------------------------------------------
  -- basic arithmetic operations, Int only!
  ------------------------------------------------------------------------------
  prpQR  :: Integer -> Integer -> Bool
  prpQR  a b = let x | a < 0     = abs a
                     | otherwise = a
                   y | b < 0     = abs b
                     | b == 0    = 1
                     | otherwise = b
                   (qi,ri) = quotRem x y
                   (qn,rn) = withInt x y quotRem 
                in qi == (fromIntegral qn) && ri == (fromIntegral rn)

  prpMulQR  :: Integer -> Integer -> Bool
  prpMulQR  a b = let x | a < 0     = abs a
                        | otherwise = a
                      y | b < 0     = abs b
                        | b == 0    = 1
                        | otherwise = b
                      (qn,rn) = withInt x y quotRem 
                      z = qn * (fromIntegral y) + rn
                   in x == fromIntegral z

  prpBigQR :: Integer -> Integer -> Property
  prpBigQR a b = monadicIO $ do
    r <- (pick $ choose (1,3)) :: PropertyM IO Integer
    s <- (pick $ choose (1,3)) :: PropertyM IO Integer 
    let x = (abs a)^r
    let y | b == 0    = 1
          | otherwise = (abs b)^s
    assert (prpQR x y)

  prpDiv :: Integer -> Integer -> Bool
  prpDiv a b = let x | a < 0     = abs a
                     | otherwise = a
                   y | b < 0     = abs b
                     | b == 0    = 1
                     | otherwise = b
                   qi = div x y
                   qn = withInt x y div 
                in qi == (fromIntegral qn) 

  prpDiv0 :: Bool
  prpDiv0 = let x  = 208
                y  = 2
                qi = div x y
                qn = withInt x y div 
              in qi == (fromIntegral qn) 

  prpBigDiv1 :: Bool
  prpBigDiv1 = let x  = 23^7    :: Integer
                   y  =  5^6    :: Integer
                   qi = div x y :: Integer
                   qn = withInt x y div 
                 in qi == (fromIntegral qn) 

  prpRem :: Integer -> Integer -> Bool
  prpRem a b = let x | a < 0     = abs a
                     | otherwise = a
                   y | b < 0     = abs b
                     | b == 0    = 1
                     | otherwise = b
                   ri = rem x y
                   rn = withInt x y rem 
                in ri == (fromIntegral rn) 

  prpGcd :: Integer -> Integer -> Bool
  prpGcd a b = let x | a < 0     = abs a
                     | otherwise = a
                   y | b < 0     = abs b
                     | b == 0    = 1
                     | otherwise = b
                   ci = gcd x y
                   cn = withInt x y gcd 
                in ci == (fromIntegral cn) 

  prpMul :: Integer -> Integer -> Bool
  prpMul a b | a < 0     = prpMul (-a) b
             | b < 0     = prpMul a (-b)
             | otherwise = let mi = a * b
                               mn = withInt a b (*)
                            in mi == fromIntegral mn

  prpDist :: Integer -> Integer -> Integer -> Bool
  prpDist a b c | a < 0     = prpDist (-a) b c
                | b < 0     = prpDist a (-b) c
                | c < 0     = prpDist a b (-c)
                | otherwise = let r1 = (withInt c a (*)) + 
                                       (withInt c b (*))
                                  r2 = (withInt c (a+b) (*))
                               in r1 == r2

  prpBigMul :: Integer -> Integer -> Property
  prpBigMul a b | a < 0     = prpBigMul (-a) b
                | b < 0     = prpBigMul a (-b)
                | otherwise = monadicIO $ do
    r <- (pick $ choose (1,9)) :: PropertyM IO Integer
    s <- (pick $ choose (1,9)) :: PropertyM IO Integer 
    assert (prpMul (a^r) (b^s))

  prpPlus  :: Integer -> Integer -> Bool
  prpPlus a b | a < 0     = prpPlus (-a) b
              | b < 0     = prpPlus a (-b)
              | otherwise = let si = a + b
                                sn = withInt a b (+)
                             in si == fromIntegral sn

  prpMinus :: Integer -> Integer -> Bool
  prpMinus a b | a < 0     = prpMinus (-a) b
               | b < 0     = prpMinus a (-b)
               | otherwise = let (x,y) | a < b     = (b,a)
                                       | otherwise = (a,b)
                                 di = x - y
                                 dn = withInt x y (-)
                              in di == fromIntegral dn

  withInt :: Integer -> Integer -> (Natural -> Natural -> r) -> r
  withInt x y f = let a = fromIntegral x 
                      b = fromIntegral y 
                   in f a b
                 

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
    r <- deepCheck prpPlus       ?>
         deepCheck prpMinus      ?>
         deepCheck prpQR         ?> 
         deepCheck prpMulQR      ?> 
         -- oneCheck  prpBigDiv1    ?> 
         someCheck 1000 prpBigQR ?> 
         deepCheck prpDiv        ?> 
         oneCheck  prpDiv0       ?> 
         deepCheck prpRem        ?> 
         deepCheck prpGcd        ?> 
         deepCheck prpMul        ?> 
         deepCheck prpBigMul     ?> 
         deepCheck prpDist    
    case r of
      Success{} -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
