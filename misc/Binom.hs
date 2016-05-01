module Binom
where

  import           Data.List (intercalate,sort)
  import           Debug.Trace (trace)

  toTheKFalling :: Integer -> Integer -> Integer
  toTheKFalling = toTheK (-)

  toTheKRising :: Integer -> Integer -> Integer
  toTheKRising = toTheK (+)

  toTheK :: (Integer -> Integer -> Integer) -> 
             Integer -> Integer -> Integer
  toTheK add n k = go 0
    where go i | i == k    = 1
               | otherwise = (n `add` i) * go (i + 1)

  fac :: Integer -> Integer
  fac 0 = 1
  fac 1 = 1
  fac n = n * fac (n - 1)

  facfac :: Integer -> Integer
  facfac 0 = 1
  facfac 1 = 1
  facfac n = n * facfac (n-2)

  choose2 :: Integer -> Integer -> Integer
  choose2 n 0 = 1
  choose2 n 1 = n
  choose2 n k | 2*k > n   = choose2 n (n-k)
              | otherwise = (n >| k) `div` fac k 
  
  choose :: Integer -> Integer -> Integer
  choose n 0 = 1
  choose n 1 = n 
  choose n k | k > n     = 0
             | 2*k > n   = choose n (n-k)
             | otherwise = go 1 1 
    where go m i | i > k     = m 
                 | otherwise = go (m * (n - k + i) `div` i) (i+1)

  choosePlusOne :: Integer -> Integer -> Integer
  choosePlusOne n k = let m = n * (choose n k) - k * (choose n k)
                       in m `div` (k+1)

  testPlusOne :: Integer -> Integer -> Bool
  testPlusOne n k = choosePlusOne n (k-1) == choose n k

  testKplusDiv :: Integer -> Integer -> Bool
  testKplusDiv n k = let b = choose n k
                         d = n * b - k * b
                      in d `rem` (k+1) == 0

  testRangeKplus :: Integer -> Bool
  testRangeKplus n = all (testKplusDiv n) [1..n-1]

  infixr >| 
  (>|) :: Integer -> Integer -> Integer
  (>|) = toTheKFalling

  infixr |> 
  (|>) :: Integer -> Integer -> Integer
  (|>) = toTheKRising

  infixr >|>
  (>|>) :: Integer -> Integer -> Integer
  (>|>) = choose

  pascalRow :: Integer -> Integer -> [Integer]
  pascalRow n k = map (choose n) [0..k]

  pascalCol :: Integer -> Integer -> [Integer]
  pascalCol n k = map (`choose` k) [0..n]

  pascal :: Integer -> Integer -> [[Integer]]
  pascal n k = map (`pascalRow` k) [0..n]

  pascalTab :: Integer -> Integer -> String
  pascalTab n k = header ++ (
                  intercalate "\n" $ rindex 0 $ tab2Str $ pascal n k)
    where tab2Str = map row2Str
          rindex _ [] = []
          rindex i (x:xs) = (n2s i ++ x) : rindex (i+1) xs
          cindex i = concatMap n2s [0..i]
          header   = "      " ++ cindex k ++ "\n" 

  row2Str :: [Integer] -> String
  row2Str = concatMap n2s

  n2s :: Integer -> String
  n2s n = let x = show n
              l = length x
              d = 5 - l
              s = map (\_ -> ' ') [0..d]
           in s ++ x

  pascalRule1 :: Integer -> Integer -> Integer
  pascalRule1 n k = choose n k + choose n (k+1)

  harmonic :: Integer -> Double
  harmonic 1 = 1
  harmonic n = 1/d + harmonic (n-1)
    where d = fromIntegral n

  phi :: Double
  phi = 0.5 * (1 + sqrt 5)

  fib :: Integer -> Integer
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-2) + fib (n-1)

  fibApx :: Integer -> (Integer, Integer)
  fibApx n = let n' = fromIntegral n 
              in (floor   $ phi^(n'-2), 
                  ceiling $ phi^(n'-1))

  fi :: Integer -> Integer
  fi n = let n' = fromIntegral n
          in round (phi^n'/sqrt 5)

  myseries :: Integer -> Integer -> (Integer,Integer)
  myseries n 0 = (1,1)
  myseries n k = ((n^k - 1) `div` (n - 1), n^k)

  poly :: Integer -> Integer -> Integer
  poly n 0 = 0
  poly n k = n^(k-1) + poly n (k-1)

  testseries :: Integer -> (Integer,Integer)
  testseries n = go n 2
    where go :: Integer -> Integer -> (Integer,Integer)
          go n k = if fst (myseries n k) /= poly n k 
                     then (n,k)
                     else go n (k+1)

  ------------------------------------------------------------------------
  -- Pascal's Rule backward
  ------------------------------------------------------------------------
  pascalBack :: Integer -> Integer -> Integer
  pascalBack n 0 = 1
  pascalBack n k | k == n    = 1
                 | n == 0    = 0
                 | n >  0 && k < 0  = 0
                 | n >  0 && k >= 0 = choose n k -- (n+1) k - choose n (k-1)
                 | k <  0    = pascalBack (n+1) (k+1) - pascalBack n (k+1)
                 | otherwise = pascalBack (n+1) k - pascalBack n (k-1)

  chooseNeg :: Integer -> Integer -> Integer
  chooseNeg n k | n >= 0 && k >= 0 = choose n k
                | n == k           = 1
                | k == 0           = 1
                | k == 1           = n
                | otherwise        = (n >| k) `div` (fac k) 

  chooseNeg2 :: Integer -> Integer -> Integer
  chooseNeg2 n k | n >= 0 && k >= 0 = choose n k
                 | n == k           = 1
                 | k == 0           = 1
                 | k == 1           = n
                 | otherwise        = (n |> k) `div` (fac k) 

  chooseNeg3 :: Integer -> Integer -> Integer
  chooseNeg3 n k | n >= 0 && k >= 0 = choose n k
                 | n == k           = 1
                 | k == 0           = 1
                 | k == 1           = n
                 | n>=0 && k<=0     = (n >| (-k)) `div` (facNeg k) 
                 | n<=0 && k<=0     = (n |> (-k)) `div` (facNeg k) 
                 | otherwise        = (n |> k) `div` (fac k) 

  facNeg :: Integer -> Integer
  facNeg n | n >= 0 = fac n
           | otherwise = n * facNeg (n+1)

  ------------------------------------------------------------------------
  -- Conjecture:
  -- chooseNeg (-n) 1     = chooseNeg n (n-1)
  -- chooseNeg (-n) (k+1) = chooseNeg (n+k) (n-1)
  ------------------------------------------------------------------------
  testChoNeg :: Integer -> Integer -> Bool
  testChoNeg n t | n < 0     = testChoNeg (-n) t
                 | otherwise = go t 1
    where absbinom a k = abs (chooseNeg (-a) k)
          go 0 _ = True
          go t k | absbinom n k == choose (n+k-1) (n-1) = go (t-1) (k+1)
                 | otherwise = error $ "failed on " ++ show k

  ------------------------------------------------------------------------
  -- Combinator for negative and positive binomial coefficients
  ------------------------------------------------------------------------
  data Sym = P String | N String 
    deriving (Eq,Show)

  instance Ord Sym where
    compare (P a) (P b) = compare a b
    compare (P a) (N b) = compare a b
    compare (N a) (P b) = compare a b
    compare (N a) (N b) = compare a b

  sortStr :: Sym -> Sym
  sortStr (P a) = P (sort a)
  sortStr (N a) = N (sort a)

  simplify :: [Sym] -> [(Integer, Sym)]
  simplify xs = go (sort $ map sortStr xs) 1
    where go []  _ = []
          go [x] n = [(n,x)]
          go ((P x):(P y):zs) n | x == y = go ((P y):zs) (n+1)
                                | otherwise = (n,P x) : go ((P y) : zs) 1
          go ((N x):(N y):zs) n | x == y = go ((N y):zs) (n+1)
                                | otherwise = (n,N x) : go ((N y) : zs) 1
          go ((N x):(P y):zs) n | x == y = go zs 1
                                | otherwise = (n,N x) : go ((P y) : zs) 1
          go ((P x):(N y):zs) n | x == y = go zs 1
                                | otherwise = (n,P x) : go ((N y) : zs) 1

  powsum :: [Sym] -> Int -> [(Integer,Sym)]
  powsum xs = simplify . combinator xs

  coeffs :: [Sym] -> Int -> [Integer]
  coeffs xs = map getCoeff . powsum xs

  getCoeff :: (Integer,Sym) -> Integer
  getCoeff (n,P _) = n
  getCoeff (n,N _) = -n

  combinator :: [Sym] -> Int -> [Sym]
  combinator _  0 = []
  combinator xs n = go xs (n-1)
    where go ys 0 = ys
          go ys n = let zs = concat [combN x ys | x <- xs]
                     in go zs (n-1)

  combine :: [Sym] -> [Sym]
  combine xs = concat [combN x xs | x <- xs]

  combN :: Sym -> [Sym] -> [Sym]
  combN x zs = map (comb x) zs

  comb :: Sym -> Sym -> Sym
  comb (P a) (P b) = P (a++b)
  comb (P a) (N b) = N (a++b)
  comb (N a) (P b) = N (a++b)
  comb (N a) (N b) = P (a++b)

  combinator_ :: [String] -> Int -> [String]
  combinator_ _  0 = []
  combinator_ xs 1 = xs
  combinator_ xs n = go xs (n-1)
    where go ys 0 = ys
          go ys n = let zs = concat [combine1 x ys | x <- xs]
                     in go zs (n-1)

  combine_ :: [String] -> [String]
  combine_ xs = concat [combine1 x xs | x <- xs]

  combine1 :: String -> [String] -> [String]
  combine1 x zs = map (x++) zs

  binomials :: [String] -> Int -> [(Integer,String)]
  binomials xs n = let zs = sort $ map sort $ combinator_ xs n
                    in go zs 1
    where go [] _  = []
          go [x] n = [(n,x)]
          go (x:y:zs) n | x == y    = go (y:zs) (n+1)
                        | otherwise = (n,x):go (y:zs) 1

  coeffs_ :: [String] -> Int -> [Integer]
  coeffs_ xs = map fst . binomials xs
