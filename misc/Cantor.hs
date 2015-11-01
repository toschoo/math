module Cantor
where

  import Data.Ratio
  import Data.List (nub,sort,sortBy,(\\))
  import Data.Tree 
  import Data.Tree.Pretty (drawVerticalTree)
  import Debug.Trace (trace)
  import Binom
  import Prime
  import Perm
  import ConwayGuy
  import Real

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
  -- Calwi to Pretty Tree
  ------------------------------------------------------------------------
  pretty :: CalwiTree -> Tree String
  pretty (Node r ks) = Node (show r) (map pretty ks)

  ------------------------------------------------------------------------
  -- Draw Calwi subtree
  ------------------------------------------------------------------------
  drawCalwi :: Int -> Rational -> String
  drawCalwi g = drawVerticalTree . pretty . calwiTree g

  ------------------------------------------------------------------------
  -- Print Calwi subtree
  ------------------------------------------------------------------------
  printCalwi :: Int -> Rational -> IO ()
  printCalwi g = putStrLn . drawCalwi g

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
  -- Extract transformation group from list
  ------------------------------------------------------------------------
  exGroup :: [[Int]] -> [[Int]]
  exGroup [] = []
  exGroup xs = let h = head xs
                   b = binverse h
                in [h,reverse h, b, reverse b]

  ------------------------------------------------------------------------
  -- Find positions of all fractions with n as numerator
  ------------------------------------------------------------------------
  unfusc :: Integer -> [Integer]
  unfusc n = [calwiP (n%d) | d <- [1..n], gcd n d == 1] 

  ------------------------------------------------------------------------
  -- Find positions of all fractions with n as numerator in one generation
  ------------------------------------------------------------------------
  unfuscGen :: Integer -> Int -> [Integer]
  unfuscGen n g = filter (\p -> length (toBinary p) == g) $ unfusc n 

  ------------------------------------------------------------------------
  -- Example: 10th generation of 55
  ------------------------------------------------------------------------
  ten55 :: [[Int]]
  ten55 = [[1,1,0,0,1,0,1,1,1,1],
           [1,0,1,1,1,0,0,1,1,1],
           [1,1,1,1,0,1,0,0,1,1],
           [1,0,0,1,1,0,0,0,1,1],
           [1,1,1,0,0,1,1,1,0,1],
           [1,0,0,0,1,0,1,1,0,1],
           [1,1,0,0,0,1,1,0,0,1],
           [1,0,1,1,0,1,0,0,0,1]]

  ------------------------------------------------------------------------
  -- list all fractions in a trajectory
  ------------------------------------------------------------------------
  trajectory :: [Int] -> [(Integer,Integer)]
  trajectory = go (0,1) 
    where go n [] = [n]
          go (n,d) (1:bs) = (n,d) : go (n+d,d) bs
          go (n,d) (0:bs) = (n,d) : go (n,d+n) bs

  ------------------------------------------------------------------------
  -- greatest common vertex (is it always 2?)
  ------------------------------------------------------------------------
  gcv :: [[Int]] -> Int
  gcv = go (-1) 
    where go _ []  = 0
          go m [x] = m
          go m (h:x:xs) = let m0 = cmp 0 h x 
                              m' | m == -1 || m0 < m = m0
                                 | otherwise         = m
                           in go m' (x:xs)
          cmp l [] [] = l
          cmp _ [] _  = error "not the same generation" 
          cmp _ _ []  = error "not the same generation" 
          cmp l (a:as) (b:bs) | a == b    = cmp (l+1) as bs
                              | otherwise = l
                           
  ------------------------------------------------------------------------
  -- Try to bridge 2 groups
  ------------------------------------------------------------------------
  bridge :: ([Int] -> [Int]) -> [[Int]] -> [[Int]] -> Bool
  bridge t g1 g2 = let g1' = map t g1
                    in sort g1' == sort g2

  ------------------------------------------------------------------------
  -- Some tests
  ------------------------------------------------------------------------
  swapbits :: [Int] -> [Int] -- nope
  swapbits [] = []
  swapbits [x] = [x]
  swapbits (x:y:zs) = y:swapbits (x:zs)

  swapInner :: [Int] -> [Int]
  swapInner [] = []
  swapInner (x:zs) = case reverse zs of
                       []     -> [x]
                       (y:zs') -> x:reverse (y:swapbits zs')
  
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
  -- Number of groups for n in fusc(n)
  ------------------------------------------------------------------------
  noFuscGroups :: Integer -> Int
  noFuscGroups n = length $ nub $  map length $ 
                      nub $ take (fromIntegral n*2) $ binsOf n

  ------------------------------------------------------------------------
  -- ks in no of groups = phi(n)/k
  ------------------------------------------------------------------------
  kFuscGroups :: Integer -> Integer -- (Integer,Integer)
  kFuscGroups n = let g = fromIntegral $ noFuscGroups n
                   in (tot n) `div` g -- (tot n,g) 
  
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

  --------------------------------------------------------------------------
  -- Stern-Brocot
  --------------------------------------------------------------------------
  type SterBroc = Tree [Integer]

  ------------------------------------------------------------------------
  -- Create tree: sterbroc n [0,1]
  ------------------------------------------------------------------------
  sterbroc  :: Int -> [Integer] -> SterBroc
  sterbroc  i r | i == 0    = Node r []
                | otherwise =  let (k1,k2) = sterbrockids r
                                in Node r [sterbroc (i-1) k1,
                                           sterbroc (i-1) k2]

  sterbrockids :: [Integer] -> ([Integer],[Integer])
  sterbrockids r  = let h  = init r
                        l  = last r
                        s  = length h
                        k1 = h++[l+1]
                        k2 = h++[l-1,2]
                      in if odd s then (k1,k2) else (k2,k1)

  ------------------------------------------------------------------------
  -- Invert a rational 
  ------------------------------------------------------------------------
  invert :: Rational -> Rational
  invert r = denominator r % numerator r

  ------------------------------------------------------------------------
  -- Continued fractions to Real
  ------------------------------------------------------------------------
  ints2rs :: [Integer] -> [Rational]
  ints2rs = map (\i -> i%1)

  sterbroc2r :: SterBroc -> Tree [Rational]
  sterbroc2r = fmap ints2rs

  sterbroc2d :: SterBroc -> Tree RealN
  sterbroc2d = fmap (contfrac2 . ints2rs)

  contfrac2 :: [Rational] -> RealN
  contfrac2 []  = 1
  contfrac2 [i] = fromRational i 
  contfrac2 (i:is) = n + 1 / (contfrac2 is)
    where n = fromRational i

  contfracr :: [Rational] -> Rational
  contfracr []  = 1
  contfracr [i] = i 
  contfracr (i:is) = i + (invert $ contfracr is)

  contfraci :: [Integer] -> Rational
  contfraci = contfracr . ints2rs
  
  sterbrocTree :: Int -> Tree Rational
  sterbrocTree i = fmap contfraci $ sterbroc i [0,1]

  -------------------------------------------------------------------------
  -- Approximate a real number
  -------------------------------------------------------------------------
  approx :: Int -> Double -> [Rational]
  approx i d = go i [0,1] 
    where go 0 _ = []
          go n r = let r' = contfracr (map toRational r) 
                       d' = (fromIntegral $ numerator r') / 
                            (fromIntegral $ denominator r')
                       (k1,k2) = sterbrockids r
                    in if d' == d then [r']
                                  else if d' < d then r':go (n-1) k2
                                                 else r':go (n-1) k1
                                 
  -------------------------------------------------------------------------
  -- Calkin Wilf <-> Stern Brocot
  -------------------------------------------------------------------------
  getSterbrocKids :: Int -> CalwiTree -> [Rational]
  getSterbrocKids = tree2tree

  getCalwiKids :: Int -> Tree Rational -> [Rational]
  getCalwiKids = tree2tree

  tree2tree :: Int -> Tree a -> [a]
  tree2tree n = bitreverse . getKids n

  bitreverse :: [a] -> [a]
  bitreverse xs = go xs $ idxbitrev xs
    where go _ []      = []
          go zs (p:ps) = zs!!p : go zs ps

  idxbitrev :: [a] -> [Int]
  idxbitrev xs =  let l  = length xs
                      l' = fromIntegral l
                      x  = round $ logBase 2 (fromIntegral l)
                   in [fromInteger (brev x i) | i <- [0..l'-1]]
    where brev x = fromBinary . cleanz . reverse . fillup x 0 . toBinary

  fillup :: Int -> Int -> [Int] -> [Int]
  fillup i z is | length is == i = is
                | otherwise      = fillup i z (z:is)

  cleanz :: [Int] -> [Int]
  cleanz []  = []
  cleanz [0] = [0]
  cleanz (0:is) = cleanz is
  cleanz is     = is

  -------------------------------------------------------------------------
  -- Stern Brocot Sequence
  -------------------------------------------------------------------------
  enumQSB :: [Rational]
  enumQSB = go 1 $ sterbrocTree (-1) 
    where go i t = getKids i t ++ go (i+1) t

  -------------------------------------------------------------------------
  -- Farey Sequence
  -------------------------------------------------------------------------
  fareyNumerators :: [Integer]
  fareyNumerators = map numerator enumQSB

  farey2 :: Integer -> [Rational]
  farey2 n = sort (nub $ filter (<= 1) $ 
                         concatMap (\x -> map (x%) [1..n]) [0..n])

  farey :: Integer -> [Rational]
  farey n = 0%1 : sort (go 1 $ sterbrocTree (-1))
    where go k t = let g = getKids k t
                       l = filter fltr g
                    in if null l then l else l++go (k+1) t
          fltr k = k <= 1%1 && n >= denominator k 

  nxtFarey :: Integer -> [Rational] -> [Rational]
  nxtFarey n []  = []
  nxtFarey n [r] = [r]
  nxtFarey n (a:b:rs) | denominator a + denominator b == n = 
                        nxtFarey n (a:x:b:rs)
                      | otherwise = a:nxtFarey n (b:rs)
    where x = let n1 = numerator a
                  n2 = numerator b
                  d1 = denominator a
                  d2 = denominator b
               in (n1+n2) % (d1+d2)

  -------------------------------------------------------------------------
  -- Stern-Brocot with mediants
  -------------------------------------------------------------------------
  mSterbroctree :: Int -> Integer -> Integer -> Integer -> 
                                     Integer -> Rational -> Tree Rational
  mSterbroctree 0 _ _ _ _ r = Node r []
  mSterbroctree n a b c d r = let rn = numerator r
                                  rd = denominator r
                                  k1 = (a+rn)%(b+rd)
                                  k2 = (c+rn)%(d+rd)
                               in if k1 < k2
                                    then Node r [mSterbroctree (n-1) a b rn rd k1,
                                                 mSterbroctree (n-1) c d rn rd k2]
                                    else Node r [mSterbroctree (n-1) c d rn rd k2,
                                                 mSterbroctree (n-1) a b rn rd k1]
