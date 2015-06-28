{-# LANGUAGE BangPatterns #-}
module Main
where

  import System.Exit
  import Test.QuickCheck
  import Test.QuickCheck.Monadic
  import Control.Applicative ((<$>))
  import Control.Monad (when)
  import Data.List (nub, sort, delete)

  import Numbers

  ------------------------------------------------------------------------
  -- Make sure, we always insert!
  ------------------------------------------------------------------------
  data NubList a = NubList [a]
  
  instance (Show a) => Show (NubList a) where
    show (NubList l) = show l

  instance (Arbitrary a, Eq a) => Arbitrary (NubList a) where
    arbitrary = NubList <$> nub <$> arbitrary

  els :: Int -> [Int] -> Gen [Int]
  els _ [] = return [] 
  els n l  = let l' = nub l
              in pickInd (min (abs n) ((length l') `div` 2)) l'
    where pickInd m is 
            | m == 0    = return []
            | otherwise = do
                i   <- choose (0, length is)
                is' <- pickInd (m-1) is
                return (i:is')

  ------------------------------------------------------------------------
  -- Poly
  ------------------------------------------------------------------------
  

  -- tests:
  -- 

  ------------------------------------------------------------------------------
  -- basic arithmetic operations, Int only!
  ------------------------------------------------------------------------------
  prpConv :: Int -> Bool
  prpConv i = let x = abs i
               in x == toInt (fromInt x)

  prpAdd :: Int -> Int -> Bool
  prpAdd = withInts (\x y ->
             x + y == toInt (fromInt x `add` fromInt y))

  prpSub :: Int -> Int -> Bool
  prpSub = withInts (\x y -> if x < y then go y x else go x y)
    where go x y = x - y == toInt (fromInt x `sub` fromInt y)

  prpAddSub :: Int -> Int -> Bool
  prpAddSub = withInts (\x y ->
                let s = fromInt x `add` fromInt y
                 in s `sub` fromInt y == fromInt x &&
                    s `sub` fromInt x == fromInt y)

  prpMul :: Int -> Int -> Bool
  prpMul = withInts (\x y -> 
             x * y == toInt (fromInt x `mul2` fromInt y)) 

  prpDiv :: Int -> Int -> Bool
  prpDiv = withInts (\x y ->
             if y == 0 then True
                       else let (q1,r1) = x `quotRem` y 
                                (q2,r2) = (fromInt x `nQuotRem2` fromInt y)
                             in (q1 == toInt q2 && r1 == toInt r2))

  withInts :: (Int -> Int -> Bool) -> Int -> Int -> Bool
  withInts f a b = let x = if abs a >= 1000 then 999 else abs a
                       y = if abs b >= 1000 then 999 else abs b
                    in f x y

  withIntsP :: (Int -> Int -> Bool) -> Int -> Int -> Property
  withIntsP f a b = let x = if abs a > 100 then 100 else abs a
                        y = if abs b > 100 then 100 else abs b
                     in collect (x,y) $ f x y

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
    r <- {- deepCheck prpConv   ?>
         deepCheck prpAdd    ?>
         deepCheck prpSub    ?>
         deepCheck prpAddSub ?>  
         deepCheck prpMul    ?>  -}
         deepCheck prpDiv
    case r of
      Success{} -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
