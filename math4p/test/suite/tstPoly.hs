{-# LANGUAGE BangPatterns #-}
module Main
where

  import System.Exit
  import Test.QuickCheck
  import Test.QuickCheck.Monadic
  import Control.Applicative ((<$>))
  import Control.Monad (when)
  import Data.List (nub, sort, delete)

  import Poly
  import Math.Polynomial

  ------------------------------------------------------------------------
  -- For debugging it's much nicer to work with digits
  ------------------------------------------------------------------------
  data Digit = Digit Int
    deriving (Read, Eq, Ord)

  instance Show Digit where
    show (Digit d) = show d

  instance Arbitrary Digit where
    arbitrary = Digit <$> elements [0..9]

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
  prpAddSub :: NonEmptyList (Digit, Integer) -> 
               NonEmptyList (Digit, Integer) -> Bool
  prpAddSub (NonEmpty xs) (NonEmpty ys) = {- collect (length $ xs) $ -} 
    let p1 = fromTerms $ dig2Term xs
        p2 = fromTerms $ dig2Term ys
        p3 = p1 `padd` p2
     in p3 `psub` p2 == p1 &&
        p3 `psub` p1 == p2

  prpMulDiv :: NonEmptyList (Digit, Integer) -> 
               NonEmptyList (Digit, Integer) -> Bool
  prpMulDiv (NonEmpty xs) (NonEmpty ys) = {- collect (length xs) $ -} 
    let xx = map (\(i,c) -> (i,fromIntegral c)) xs
        yy = map (\(i,c) -> (i,fromIntegral c)) ys
        p1 = fromTerms $ dig2Term xx
        p2 = fromTerms $ dig2Term yy
        p3 = p1 `pmul` p2
     in (pnull p3 && (pnull p1 || pnull p2)) ||
        fst (p3 `pdiv` p2) == p1 &&
        fst (p3 `pdiv` p1) == p2

  prpEuclide :: NonEmptyList (Digit, Integer) ->
                NonEmptyList (Digit, Integer) -> 
                NonEmptyList (Digit, Integer) -> Property 
  prpEuclide (NonEmpty xs) (NonEmpty ys) (NonEmpty zs) = monadicIO $ do
    let xx = map (\(i,c) -> (i,fromIntegral c)) xs
    let yy = map (\(i,c) -> (i,fromIntegral c)) ys
    let zz = map (\(i,c) -> (i,fromIntegral c)) zs
    let p1 = fromTerms $ dig2Term xx
    let p2 = fromTerms $ dig2Term yy
    let p3 = fromTerms $ dig2Term zz
    let p4 = p1 `pmul` p2
    let p5 = p4 `padd` p3
    when (not (pnull p4)) $ run $ do
      let (q,r) = p5 `pdiv` p1
      putStrLn $ (show p1) ++ " * " ++ (show p2) ++ " + " ++
                 (show p3) ++ " = " ++ (show p5)
      putStrLn $ (show p5) ++ " / " ++ (show p1) ++ " = " ++
                 (show  q) ++ " + " ++ (show  r)
    assert((pnull p4 && (pnull p1 || pnull p2)) ||
           (let q = fst (p5 `pdiv` p1)
                r = snd (p5 `pdiv` p1)
             in q == p2 && r == p3))

  prpEuclide2 :: NonEmptyList Integer ->
                 NonEmptyList Integer -> 
                 NonEmptyList Integer -> Property 
  prpEuclide2 (NonEmpty xs) (NonEmpty ys) (NonEmpty zs) = monadicIO $ do
    let p1 = poly BE (map fromIntegral xs)
    let p2 = poly BE (map fromIntegral ys)
    let p3 = poly BE (map fromIntegral zs)
    let p4 = p1 `multPoly` p2
    let p5 = p4 `addPoly`  p3
    when (not (polyIsZero p4)) $ run $ do
      let (q,r) = p5 `quotRemPoly` p1
      putStrLn $ (show p1) ++ " * " ++ (show p2) ++ " + " ++
                 (show p3) ++ " = " ++ (show p5)
      putStrLn $ (show p5) ++ " / " ++ (show p1) ++ " = " ++
                 (show  q) ++ " + " ++ (show  r)
    assert((polyIsZero p4 && (polyIsZero p1 || polyIsZero p2)) ||
           (p1 == one || p2 == one) ||
           (let (q,r) = p5 `quotRemPoly` p1
             in q == p2 && r == p3))

  dig2Term :: (Num c) => [(Digit, c)] -> [Term c]
  dig2Term xs = map conv xs
    where conv (Digit k, c) = (k, c)

  -------------------------------------------------------------
  -- controlled quickcheck, arbitrary tests
  -------------------------------------------------------------
  deepCheck :: (Testable p) => p -> IO Result
  deepCheck = quickCheckWithResult stdArgs{maxSuccess=10000}

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
    r <- {-deepCheck prpAddSub   ?>
         deepCheck prpMulDiv   ?>
         deepCheck prpEuclide  ?> -}
         deepCheck prpEuclide2 -- ?>
    case r of
      Success{} -> do
        putStrLn good
        exitSuccess
      _ -> do
        putStrLn bad
        exitFailure

  main :: IO ()
  main = checkAll
