module Numbers
where

  import Data.List (foldl',nub,group)
  import Data.Char (intToDigit)
  import Data.Ratio
  import Binom
  import Prime
  import Perm 
  import Debug.Trace (trace)

  -------------------------------------------------------------------------
  -- Nexus
  -------------------------------------------------------------------------
  triangle :: Integer -> Integer
  triangle n = withRatio n $ \x -> (x / 2) * (x + 1) 

  square :: Integer -> Integer
  square = ngon 2

  pentagon :: Integer -> Integer
  pentagon = ngon 3

  hexagon :: Integer -> Integer
  hexagon = ngon 4

  heptagon :: Integer -> Integer
  heptagon = ngon 5

  octagon :: Integer -> Integer
  octagon = ngon 6

  hex :: Integer -> Integer
  hex n = 1 + 6 * triangle (n - 1)

  ------------------------------------------------------------------------
  -- Nexus numbers:
  -- 0: sum of ones = n
  -- 1: sum of odds = n^2
  -- 2: sum of hex  = n^3
  -- 3: sum of rho  = n^4 -- rhombic dodecahedral
  -- 4: ...
  ------------------------------------------------------------------------
  hexsum :: Integer -> Integer
  hexsum 1 = 1
  hexsum n = hex n + hexsum (n-1)

  tetra :: Integer -> Integer
  tetra n = withRatio n $ \x -> (x^3 + 3 * x^2 + 2 * x) / 6

  slowtetra :: Integer -> Integer
  slowtetra 1 = 1
  slowtetra n = triangle n + slowtetra (n - 1)

  pyr :: Integer -> Integer
  pyr n = withRatio n $ \x -> (2*x^3 + x^2 + 2*x^2 + x) / 6

  pyr2 :: Integer -> Integer
  pyr2 n = tetra n + tetra (n - 1)

  slowpyr :: Integer -> Integer
  slowpyr 1 = 1
  slowpyr n = n^2 + slowpyr (n-1)

  ------------------------------------------------------------------------
  -- Platonic Solids
  ------------------------------------------------------------------------
  pyr3 :: Integer -> Integer -- tetrahedal
  pyr3 n = choose (n+2) 3

  pyr3b :: Integer -> Integer
  pyr3b 1 = 1
  pyr3b n = pyr3b (n-1) + triangle n

  octa :: Integer -> Integer -- octahedral
  octa n = withRatio n $ \x -> (2*x^3 + x) / 3

  octabin :: Integer -> Integer
  octabin n = 4 * (choose n 3) + 4 * (choose n 2) + n 

  ico :: Integer -> Integer -- icosahedral
  ico n = withRatio n $ \x -> (5 * x^3 - 5 * x^2 + 2*x) / 2

  dode :: Integer -> Integer -- dodecahedral
  dode n = choose (3*n) 3

  ------------------------------------------------------------------------
  -- Note that 
  -- n + triangle (n-1) is the same as
  -- triangle n (that is the trivial case i = 1 in ngon).
  -- Here's the proof:
  -- (1) n + ((n-1)/2) * (n - 1 + 1) = n/2 (n + 1)
  -- (2) n + ((n-1)/2) * n           = n/2 (n + 1)
  -- (3) n + (n^2 - n / 2)           = n^2 + n / 2
  -- (4) 2n/2 + (n^2 - n / 2)        = n^2 + n / 2
  -- (5) n^2 - n + 2n / 2            = n^2 + n / 2
  -- (6) n^2 + n / 2                 = n^2 + n / 2 qed
  ------------------------------------------------------------------------
  ngon :: Integer -> Integer -> Integer
  ngon i n = n + i * triangle (n-1)

  withRatio :: Integer -> (Ratio Integer -> Ratio Integer) -> Integer
  withRatio n f = floor $ f (fromIntegral n) 

  withRatios :: Integer -> Integer -> 
                (Ratio Integer -> Ratio Integer -> Ratio Integer) 
                -> Integer
  withRatios n m f = floor $ f (fromIntegral n) (fromIntegral m)

  triangles :: [Integer]
  triangles = map triangle [1..]

  squares :: [Integer]
  squares = map square [1..]

  pentagons :: [Integer]
  pentagons = map pentagon [1..]

  hexagons :: [Integer]
  hexagons = map hexagon [1..]

  heptagons :: [Integer]
  heptagons = map heptagon [1..]

  octagons :: [Integer]
  octagons = map octagon [1..]

  hexes :: [Integer] -- congruent 1 modulo 6
  hexes = map hex [1..]

  hexsums :: [Integer]
  hexsums = map hexsum [1..] -- surprise!

  tetras :: [Integer]
  tetras = map tetra [1..]

  slowtetras :: [Integer]
  slowtetras = map slowtetra [1..]

  pyrs :: [Integer]
  pyrs = map pyr [1..]

  pyrs2 :: [Integer]
  pyrs2 = map pyr [1..]

  slowpyrs :: [Integer]
  slowpyrs = map slowpyr [1..]

  sqmods :: Integer -> [Integer]
  sqmods n = map (`rem` n) squares

  oddsqmods :: Integer -> [Integer]
  oddsqmods n = map (`rem` n) $ filter odd squares

  odds :: [Integer]
  odds = [1,3..]

  evens :: [Integer]
  evens = [2,4..]

  oddsum :: Integer -> Integer
  oddsum x = let n = fromIntegral x in sum $ take n odds

  oddsum2 :: Integer -> Integer
  oddsum2 n = n^2

  seqsum :: [Integer] -> Integer 
  seqsum ns = let a = fromIntegral $ head   ns
                  z = fromIntegral $ last   ns
                  n = fromIntegral $ length ns
                  r = n * ((a + z) / 2) :: (Ratio Integer)
               in floor r

  -------------------------------------------------------------------------
  -- Handshake
  -------------------------------------------------------------------------
  handshake :: Integer -> Integer
  handshake n = triangle (n-1)

  handshake2 :: Integer -> Integer
  handshake2 n = choose n 2

  triangle2 :: Integer -> Integer
  triangle2 n = choose (n+1) 2

  -------------------------------------------------------------------------
  -- Newton
  -------------------------------------------------------------------------
  regions :: Integer -> Integer
  regions n = sum $ map (choose (n-1)) [0..4]

  genreg :: Integer -> Integer
  genreg x = withRatio x $ \n -> (n^4 - 6*n^3 + 23*n^2 - 18*n + 24)/24

  diff :: Integer -> Integer -> Integer
  diff a b = abs (b - a)

  diffstep :: [Integer] -> [Integer]
  diffstep []  = []
  diffstep [x] = []
  diffstep (a:b:bs) = diff a b : diffstep (b:bs)

  diffmachine :: [Integer] -> [[Integer]]
  diffmachine xs = let ds = diffstep xs
                    in if constant ds then [ds]
                                      else ds : diffmachine ds
    where constant []     = True
          constant (x:xs) = all (==x) xs
  
  -- give me:
  --   - the x coordinate of the starting point
  --   - the x coordinate I want to calculate
  --   - the sequence (starting from starting point)
  -- and I give you the y
  newton :: Integer -> Integer -> [Integer] -> Integer
  newton t n xs = let hs = heads $ xs:diffmachine xs
                   in trace (show hs) $ cos 0 hs
    where heads xs = map go xs
          go []    = 0
          go (z:_) = z
          cos _ [] = 0
          cos i (z:zs) = z * (choose (n - t) i) + cos (i+1) zs

  -------------------------------------------------------------------------
  -- Cataln Numbers
  -------------------------------------------------------------------------
  catalan :: Integer -> Integer
  catalan n = let m = 2*n + 1 in choose m n `div` m

  -------------------------------------------------------------------------
  -- Collatz Conjecture
  -------------------------------------------------------------------------
  hailstone :: Integer -> [Integer]
  hailstone 1 = [1]
  hailstone n | even n    = n : hailstone (n `div` 2)
              | otherwise = n : hailstone (3 * n + 1) 

  ------------------------------------------------------------------------
  -- Description of strings
  ------------------------------------------------------------------------
  desc :: Integer -> Integer
  desc 1 = 1
  desc n = read $ say (show $ desc (n-1))

  say :: String -> String
  say xs = concat [[intToDigit (length x), head x] | x <- group xs]

  dlen :: Integer -> Int
  dlen = length . show . desc

  ------------------------------------------------------------------------
  -- e
  ------------------------------------------------------------------------
  e :: Double
  e = e_ 17

  e_ :: Integer -> Double
  e_ p = 1 + sum [1/(dfac n) | n <- [1..p]]
    where dfac = fromInteger . fac

  e__ :: Double -> Double
  e__ n = (1 + 1/n)**n

  stirfac :: Integer -> Integer
  stirfac i = ceiling $ (sqrt (2*pi*n)) * (n/e)^i
    where n = fromIntegral i

  devfac :: Integer -> Double
  devfac i = (sum [100 * (d x) / dfac x  | x <- [1..i]]) / (fromIntegral i)
    where d x  = fromIntegral (fac x - stirfac x)
          dfac = fromIntegral . fac

  contfrac :: [Rational] -> Double
  contfrac [] = 0
  contfrac (i:is) = n + 1 / (contfrac is)
    where n = fromRational i

  engelexp :: [Integer]
  engelexp = 2:1:go 1
    where go n = (2*n):1:1:go(n+1)

  fastexp :: [Rational]
  fastexp = 1:(1%2):go 1
    where go n = (16*n-4):(4*n+1):go (n+1)


  ------------------------------------------------------------------------
  -- Euler-Mascheroni
  ------------------------------------------------------------------------
  gamma_ :: Integer -> Double
  gamma_ n = let d = fromIntegral n
              in sum [1/k | k <- [1.0..d]] - log d

  pquoz :: Integer -> [Double]
  pquoz n = let ps = map fromIntegral (takeWhile (<n) allprimes)
                d  = fromIntegral n
             in [d/p | p <- ps]

  pquozavg :: Integer -> Double
  pquozavg n = let qs = {- filter (\q -> ceiling q /= floor q) $ -} pquoz n
                   ns = map (fromIntegral . ceiling) qs
                   ds = [n - q | (n,q) <- zip ns qs]
                in (sum ds) / (fromIntegral $ length ds)

  gamma :: Double
  gamma = gamma_ 250000

  harmonatural :: Integer -> Double
  harmonatural n = harmonic n - ln n
    where ln = log . fromIntegral
 
  ------------------------------------------------------------------------
  -- Poussin
  ------------------------------------------------------------------------
  poussin :: Integer -> Double
  poussin n = let ps = map fromIntegral (takeWhile (< n) allprimes)
                  d  = fromIntegral n
                  qs = map ((/) d) ps
                  ns = map (fromIntegral . ceiling) qs
                  ds = [x - y | x <- ns, y <- qs]
               in sum ds / fromIntegral (length ds) 
 
  ------------------------------------------------------------------------
  -- Dirichlet
  ------------------------------------------------------------------------
  divsupn :: Integer -> [[Integer]]
  divsupn n = map divs [1..n]

  ndivs :: Integer -> [Int]
  ndivs = map length . divsupn

  sumndivs :: Integer -> Int
  sumndivs = sum . ndivs

  dirichlet :: Integer -> Double
  dirichlet n = s / l
    where ds = ndivs n
          l  = fromIntegral $ length ds
          s  = fromIntegral $ sum ds

  ------------------------------------------------------------------------
  -- Leibniz Series
  ------------------------------------------------------------------------
  leibniz :: [Double]
  leibniz = go 0 1
    where go x d = ((-1.0)**x) * (1/d) : go (x+1) (d+2)

  leipi :: Int -> Double
  leipi i = 4 * go 0 1
    where go n d | n == i = 0
                 | otherwise = let x | even n    = 1
                                     | otherwise = -1 
                                in x/d + go (n+1) (d+2)

  ------------------------------------------------------------------------
  -- Euler Product
  ------------------------------------------------------------------------
  eup :: Int -> Double
  eup i = 4 * (foldl' eufac 1.0 $ take i $ drop 1 allprimes)
    where eufac :: Double -> Integer -> Double
          eufac r p = let k | p `rem` 4 == 1 = p-1
                            | p `rem` 4 == 3 = p+1
                          n = fromIntegral p
                          d = fromIntegral k
                       in r * (n/d)
  
  ------------------------------------------------------------------------
  -- Another nice formula: sum of fractions of squares
  ------------------------------------------------------------------------
  pisq6 :: Int -> Double
  pisq6 i = sqrt (6 * (go 1))
    where go n | n == i    = 0
               | otherwise = let d = fromIntegral n
                              in 1/(d^2) + go (n+1)
  
  ------------------------------------------------------------------------
  -- Viète Product, converges after 10
  ------------------------------------------------------------------------
  vietep :: Int -> Double
  vietep i = 2 / (go 0 (sqrt 2))
    where go n t | n == i = 1
                 | otherwise = (t/2) * go (n+1) (sqrt (2+t))
  
  ------------------------------------------------------------------------
  -- Nilakantha (converges after ~ 35 to 3.14159)
  ------------------------------------------------------------------------
  nilak :: Int -> Double
  nilak i | even i    = go (i+1) 2 3 4
          | otherwise = go i     2 3 4
    where go 0 _ _ _ = 3
          go n a b c = let k | even n    = -4
                             | otherwise =  4
                        in (k/(a*b*c)) + go (n-1) c (c+1) (c+2)

