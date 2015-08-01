module Cantor
where

  import Data.Ratio
  import Data.List (nub,sort)
  import Debug.Trace (trace)
  import Binom
  import Prime
  import Perm
  -- import Fib

  ------------------------------------------------------------------------
  -- Convert from dec to any
  ------------------------------------------------------------------------
  toBaseN :: Integer -> Integer -> [Int]
  toBaseN b = reverse . go
    where go x = case x `quotRem` b of
                   (0, r) -> [fromIntegral r]
                   (q, r) -> (fromIntegral r) : go q

  ------------------------------------------------------------------------
  -- Convert from any to dec
  ------------------------------------------------------------------------
  fromBaseN :: Integer -> [Int] -> Integer
  fromBaseN b = go 0 . map fromIntegral . reverse
    where go _ []     = 0
          go x (r:rs) = r*(b^x) + go (x+1) rs 

  ------------------------------------------------------------------------
  -- Convert from dec to binary
  ------------------------------------------------------------------------
  toBinary :: Integer -> [Int]
  toBinary = toBaseN 2

  ------------------------------------------------------------------------
  -- Convert from binary to dec
  ------------------------------------------------------------------------
  fromBinary :: [Int] -> Integer
  fromBinary = fromBaseN 2

  ------------------------------------------------------------------------
  -- Invert the interior bits
  ------------------------------------------------------------------------
  binverse :: [Int] -> [Int]
  binverse [] = []
  binverse bs = head bs : go (tail bs)
    where go []  = []
          go [x] = [x]
          go (0:xs) = 1 : go xs
          go (1:xs) = 0 : go xs

  ------------------------------------------------------------------------
  -- Helper for working with numerator and denominator
  ------------------------------------------------------------------------
  withRS :: Rational -> ((Integer,Integer) -> Rational) -> Rational
  withRS x f = f (numerator x, denominator x)

  ------------------------------------------------------------------------
  -- Counting
  ------------------------------------------------------------------------
  count :: [a] -> [(a, Integer)]
  count xs = zip xs [1..]

  ------------------------------------------------------------------------
  -- Create the sequence of Cantor's first diagonal argument
  ------------------------------------------------------------------------
  cantor1 :: [Rational]
  cantor1 = (1%1): go 2 1
    where go n 1   = up   n 1 ++ go 1 (n+1) 
          go 1 d   = down 1 d ++ go (d+1) 1
          down n 1 = [n%1]
          down n d = (n%d) : down (n+1) (d-1)
          up   1 d = [1%d]
          up   n d = (n%d) : up (n-1) (d+1)

  ------------------------------------------------------------------------
  -- Calwi Tree
  ------------------------------------------------------------------------
  data Tree a = Node a [Tree a]
    deriving (Eq,Show)

  type CalwiTree = Tree Rational

  ------------------------------------------------------------------------
  -- Create tree
  ------------------------------------------------------------------------
  calwiTree :: Int -> Rational -> CalwiTree
  calwiTree i r | i == 0    = Node r []
                | otherwise = let n = numerator r
                                  d = denominator r 
                               in Node r [calwiTree (i-1) (n % (n+d)),
                                          calwiTree (i-1) ((n+d) % d)]

  ------------------------------------------------------------------------
  -- Tree to Sequence
  ------------------------------------------------------------------------
  calwiTree2Seq :: CalwiTree -> [Rational]
  calwiTree2Seq t = go 1
    where go n = case getKids n t of
                   [] -> []
                   sq -> sq ++ go (n+1)

  ------------------------------------------------------------------------
  -- Get one generation
  ------------------------------------------------------------------------
  getKids :: Int -> Tree a -> [a]
  getKids 1 (Node r _)      = [r]
  getKids n (Node r [])     = []
  getKids n (Node r (x:xs)) = getKids (n-1) x ++ getKids n (Node r xs) 

  ------------------------------------------------------------------------
  -- Product of one generation is 1
  ------------------------------------------------------------------------
  genprod :: Int -> CalwiTree -> Rational
  genprod n t = product $ getKids n t
                   
  ------------------------------------------------------------------------
  -- The next number in the Calkin-Wilf listing 
  -- according to Moshe Newman
  -- See Proofs from the Book, p. 107
  ------------------------------------------------------------------------
  next :: Rational -> Rational
  next x = one / (i + one - q)
    where i = (floor x) % 1
          q = x - i 
          one = 1 % 1

  ------------------------------------------------------------------------
  -- Give me the n-th fraction in the Calkin-Wilf listing
  ------------------------------------------------------------------------
  calwiR :: Integer -> Rational
  calwiR = go (0 % 1) . toBinary 
    where go y [] = y
          go y (0:xs) = withRS y $ \(r,s) -> go (r % (r+s)) xs
          go y (1:xs) = withRS y $ \(r,s) -> go ((r+s) % s) xs

  ------------------------------------------------------------------------
  -- Give me the position x of fraction n in the Calkin-Wilf listing
  ------------------------------------------------------------------------
  calwiP :: Rational -> Integer
  calwiP = fromBinary . reverse . go 
    where go r = let n = numerator   r
                     d = denominator r
                  in if n == 0 then []
                               else if n >= d then 1 : go ((n - d) % d)
                                              else 0 : go (n % (d-n))

  ------------------------------------------------------------------------
  -- Calkin-Wilf Sequence
  ------------------------------------------------------------------------
  enumQ :: [Rational]
  enumQ = map calwiR [1..]

  ------------------------------------------------------------------------
  -- Stern Sequence
  ------------------------------------------------------------------------
  stern :: [Integer]
  stern = map numerator enumQ

  ------------------------------------------------------------------------
  -- Fusc (see http://en.wikipedia.org/wiki/Calkin-Wilf tree):
  -- - The number of odd binomial coefficients of the form 
  --   choose (n-r) r for 0 <= 2r <= n
  -- - The number of ways of writing n as a sum of powers of 2
  --    in which each power appears at most twice
  ------------------------------------------------------------------------
  fusc :: Integer -> Integer
  fusc 0 = 0
  fusc 1 = 1
  fusc n | even n    = fusc (n `div` 2)
         | otherwise = let k = (n-1) `div` 2 
                        in fusc k + fusc (k+1)

  fuscd1 :: Integer -> Integer
  fuscd1 n = ewd n 1 0
    where ewd 0 _ b = b
          ewd m a b | even m    = ewd (m `div` 2) (a+b) b
                    | otherwise = ewd ((m-1) `div` 2) a (b+a)

  fuscd2 :: Integer -> Integer
  fuscd2 n = ewd n 0
    where ewd 0 b = b
          ewd a b | even a    = ewd (a `div` 2) (2*b) 
                  | otherwise = ewd ((a-1) `div` 2) (2*b+1)

  ------------------------------------------------------------------------
  -- fusc with alternating 1s and 0s is
  --      fib (number of bits)
  ------------------------------------------------------------------------
  fibfusc :: Int -> Bool
  fibfusc i = let b = reverse (1 : concat (take i $ repeat [0,1]))
                  n = fromBinary b
                  l = fromIntegral (length b) + 1
               in fi l == fusc n


  bfusc :: [Int] -> [Int]
  bfusc = reverse . go . reverse
    where go []  = []
          go [0] = [0]
          go [1] = [1]
          go (0:bs) = go bs
          go (1:bs) = badd (go bs) (go (inc bs))

  inc :: [Int] -> [Int]
  inc = go 
    where go [] = [1]
          go (0:bs) = 1:bs
          go (1:bs) = 0:inc bs

  badd :: [Int] -> [Int] -> [Int]
  badd a [] = a
  badd [] b = b
  badd (0:as) (0:bs) = 0:badd as bs
  badd (0:as) (1:bs) = 1:badd as bs 
  badd (1:as) (0:bs) = 1:badd as bs
  badd (1:as) (1:bs) = 0:(badd (inc as) bs)

  ------------------------------------------------------------------------
  -- Find first generation of occurence of numerator n
  ------------------------------------------------------------------------
  oriGen :: CalwiTree -> Integer -> Int
  oriGen t n = go 1
    where go i | chk i     = i
               | otherwise = go (i+1)
          chk i = case filter (==n) $ map numerator (getKids i t) of
                    [] -> False
                    _  -> True

  ------------------------------------------------------------------------
  -- Find all positions of numerator n in binary, zeros dropped
  ------------------------------------------------------------------------
  binsOf :: Integer -> [[Int]]
  binsOf n = map fst $ filter isN [(toB x, calwiR x) | x <- [1..]]
    where isN x = n == numerator (snd x)
          toB  = reverse . dropWhile (==0) . reverse . toBinary
               
  
  ------------------------------------------------------------------------
  -- Odd binary coefficients
  ------------------------------------------------------------------------
  oddCos :: Integer -> [Integer]
  oddCos n = filter odd [choose (n-r) r | r <- [0..(n `div` 2)]]

  ------------------------------------------------------------------------
  -- Hyperbinary systems
  ------------------------------------------------------------------------
  hyperbin :: Integer -> [[Integer]]
  hyperbin n = let p1   = takeWhile (<=n) powers2
                   pool = perms p1
                in nub (go pool)
    where go pss = filter (\k -> sum k == n) [sort (sums n 0 ps ps) | ps <- pss]

  sums :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer]
  sums _ _ [] [] = []
  sums n s [] ds = sums n s ds []
  sums n s (p:ps) ds | s + p > n  = sums n s ds []
                     | s + p == n = [p]
                     | otherwise  = p : sums n (s+p) ps ds
  
  powers2 :: [Integer]
  powers2 = map (2^) [0..]

