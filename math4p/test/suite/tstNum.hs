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

  import Types
  import Multi 
  import Div -- hiding (div, quotRem)
  import Prelude hiding (div, quotRem)
  import Log

  -- import Numbers (root2)

  ------------------------------------------------------------------------------
  -- basic arithmetic operations, Int only!
  ------------------------------------------------------------------------------
  prpConv :: Integer -> Bool
  prpConv i = let x = abs i
               in x == n2Integer (int2n x)

  prpAdd :: Integer -> Integer -> Bool
  prpAdd = withAbs (\x y ->
             x + y == n2Integer (int2n x `add2` int2n y))

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

  {-
  prpRoot2 :: Integer -> Bool
  prpRoot2 x = let a = abs x
                   b = sqrt (fromIntegral a)
                   (c,r) = root2 (int2n a)
                   (ic,ir)= (n2Integer c, n2Integer r) 
                in -- trace (show a ++ ": " ++ show ic ++ " " ++ show ir ++ " (" ++ show (floor b)) $ 
                   ic == floor b &&
                   ic*ic + ir == a  && (ic < 3 || ir < ic*ic) 
  -}

  prp2Root :: Integer -> Bool
  prp2Root n = prpRoot n 2

  prp3Root :: Integer -> Bool
  prp3Root n = prpRoot n 3

  prp4Root :: Integer -> Bool
  prp4Root n = prpRoot n 4

  prp5Root :: Integer -> Bool
  prp5Root n = prpRoot n 5

  prp102Root :: Integer -> Bool
  prp102Root n = prp2Root (100 * n) 

  prp103Root :: Integer -> Bool
  prp103Root n = prp3Root (1000 * n) 

  prp104Root :: Integer -> Bool
  prp104Root n = prp4Root (100 * n) 

  prp105Root :: Integer -> Bool
  prp105Root n = prp5Root (100 * n) 

  prp100Root :: Integer -> Integer -> Bool
  prp100Root n = prpRoot (100 * n) 

  prpRootStat :: Integer -> Integer -> Property
  prpRootStat n m = collect (n,m) $ prpRoot n m

  prpRoot :: Integer -> Integer -> Bool
  prpRoot n m = let a = int2n $ abs n
                    x = int2n $ if m == 0 then 2 else abs m
                    (b,r) = root a x
                 in -- trace (show a ++ ": " ++ show ic ++ " " ++ show ir ++ " (" ++ show (floor b)) $ 
                    cmp (add2 (power2 b x) r) a  == EQ &&
                    (elem (x `cmp` unity) [LT,EQ] || 
                     elem (a `cmp` unity) [LT,EQ] || 
                     power2 (next b) x `cmp` a == GT) 

  prpLog :: Integer -> Integer -> Bool
  prpLog p q = let x = abs p
                   y = abs q
                   z = if x >= y then if x == 0 then 100 else 100 * x else y
                   b = int2n $ if x == 0 || x == 1 then 2 else x
                   n = int2n $ if z == 0 || z == 1 then x + 100 else z
                   (e,r) = {- trace ("b: " ++ show b ++ ", n: " ++ show n) -} nLog b n
                in cmp (add2 (power2 b e) r) n == EQ &&
                   power2 b  (next e) `cmp`  n == GT

  prpLog102 :: Integer -> Bool
  prpLog102 n = let x = if n == 0 then 100 else 100 * (abs n)
                 in prpLog 2 x

  prpRootI :: Integer -> Integer -> Bool
  prpRootI n m = let a = fromIntegral $ abs n 
                     x = fromIntegral $ if m < 1 || m > 10 || m >= n then 4 else m
                     b = a**(1/x)
                  in True -- b**x == a 

  {-
  prpDiv :: Integer -> Integer -> Bool
  prpDiv = withInts (\a b ->
             let (x,y) = if b == 0 then (a,1) else (a,b)
                 (q1,r1) = x `quotRem` y 
                 (q2,r2) = (int2n x `quotRem2` int2n y)
              in (q1 == n2Integer q2 && r1 == n2Integer r2))
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

  int2n :: Integer -> Number
  int2n = map fromChar . show
    where fromChar '0' = Zero
          fromChar '1' = One 
          fromChar '2' = Two 
          fromChar '3' = Three 
          fromChar '4' = Four
          fromChar '5' = Five
          fromChar '6' = Six 
          fromChar '7' = Seven
          fromChar '8' = Eight
          fromChar '9' = Nine
          fromChar _   = undefined

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
    r <- deepCheck prpConv     ?>
         deepCheck prpAdd      ?> 
         deepCheck prpSub      ?>
         deepCheck prpAddSub   ?> 
         deepCheck prpMul      ?>
         deepCheck prp2Root    ?> 
         deepCheck prp3Root    ?> 
         deepCheck prp4Root    ?> 
         deepCheck prp5Root    ?> 
         deepCheck prp102Root  ?>
         deepCheck prp103Root  ?>
         deepCheck prp104Root  ?>
         deepCheck prp105Root  ?>
         deepCheck prpRoot     ?>
         deepCheck prp100Root  ?> 
         deepCheck prpLog      ?>
         deepCheck prpLog102 
    case r of
      Success{} -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
