{-# Language BangPatterns #-}
module Prime
where

  import System.Random (randomRIO)
  import System.Environment
  import Control.Applicative ((<$>))
  import Data.List (nub,(\\),delete,sort,foldl',group)
  import Data.Ratio
  import Debug.Trace (trace)
  import Binom

  -------------------------------------------------------------------------
  -- Sieve of Eratosthenes
  -------------------------------------------------------------------------
  -- drawbacks:
  -- starts always with 2 (we cannot search primes 
  --                       starting with a large number)
  -- no parallelism (because the next prime depends on the previous numbers)
  -- expensive in general
  -------------------------------------------------------------------------
  erato :: [Integer]
  erato = sieve 2 [2..]
    where sieve x xs = case filter (\p -> p `rem` x /= 0) xs of 
                         [] -> [x]
                         ps -> x : sieve (head ps) ps 

  -------------------------------------------------------------------------
  -- This is a more elegant and efficient implementation of eratosthenes
  -------------------------------------------------------------------------
  allprimes :: [Integer]
  allprimes = 2 : primes 3 0

  ldp :: Integer -> Integer
  ldp  = ldpf allprimes

  ldpf :: [Integer] -> Integer -> Integer
  ldpf (p:ps) n | rem n p == 0 = p
                | p^2 > n      = n
                | otherwise    = ldpf ps n

  primes :: Integer -> Integer -> [Integer]
  primes i1 0  = filter prime [i1..]
  primes i1 i2 = filter prime [i1..i2]         

  -------------------------------------------------------------------------
  -- Sundaram
  -------------------------------------------------------------------------
  sundaram :: Integer -> [Integer]
  sundaram n = let rs = [1..k] \\ nub (concatMap sund1 [1..k])
                in 2: map (\x -> 2*x + 1) rs
    where sund1 i = filter (<= n) $ map (\j -> i + j + 2*i*j) [1..k]
          k       = n `div` 2

  {-
  sund :: Integer -> [Integer]
  sund n = 2 : map (\x -> 2*x + 1) (go [1..l] [1..m] [1..k])
    where go [] _      rs     = rs
          go (i:is) [] rs     = go is [1..m] rs
          go (i:is) (j:js) rs = go (i:is) js (delete (i+j+2*i*j) rs)
          k = n `div` 2
          l = k `div` 2
          m = l `div` 2
  -}

  sund :: Integer -> [Integer]
  sund n = 2 : [2*x+1 | x <- [1..n] \\ [i+j+2*i*j | 
                                        i <- [1..li], j <- [i..(lj i)]]]
    where li   = floor $ sqrt (fromIntegral n / 2)
          lj i = floor $ fromIntegral (n-i) / fromIntegral (2*i+1)
          

  -------------------------------------------------------------------------
  -- Prime candidates
  -------------------------------------------------------------------------
  mersenne :: Integer -> Integer
  mersenne p = 2^p - 1

  mersennes :: [Integer]
  mersennes = map mersenne erato

  fermat :: Integer -> Integer
  fermat n = 2^(2^n) + 1

  fermats :: [Integer]
  fermats = map fermat [1..] -- [fermat n | n <- [1..]]

  ------------------------------------------------------------------------
  -- Primality Tests
  ------------------------------------------------------------------------
  prime :: Integer -> Bool
  prime n | n < 1     = error "Not a positive integer!"
          | n == 1    = False
          | otherwise = ldp n == n

  ------------------------------------------------------------------------
  -- Fermat's Primality Test
  ------------------------------------------------------------------------
  fprime :: Integer -> Bool
  fprime 1 = False
  fprime 2 = True
  fprime p = rem (2^(p - 1)) p == 1

  fprimes :: [Integer]
  fprimes = 2:filter fprime [3..] 

  ------------------------------------------------------------------------
  -- Wilson's Primality Test
  ------------------------------------------------------------------------
  wprime :: Integer -> Bool
  wprime 1 = False
  wprime 2 = True
  wprime p = mod (fac (p-1)) p == mod (-1) p

  wprimes :: [Integer]
  wprimes = 2:filter wprime [3..]

  ------------------------------------------------------------------------
  -- Rabin-Miller Primality Test
  ------------------------------------------------------------------------
  rabinMiller :: Int -> Integer -> IO Bool
  rabinMiller 0 _ = return True
  rabinMiller _ 0 = return False
  rabinMiller _ 1 = return False
  rabinMiller _ 2 = return True
  rabinMiller k p | even p    = return False
                  | otherwise = 
    let (s,t) = odd2t (p-1) 0
     in do a <- randomRIO (2,p-1)
           if isPrime p a s t then rabinMiller (k-1) p
                              else return False

  odd2t :: Integer -> Integer -> (Integer,Integer)
  odd2t s t | even s    = odd2t (s `div` 2) (t+1)
            | otherwise = (s,t)


  isPrime :: Integer -> Integer -> Integer -> Integer -> Bool
  isPrime p a s t = case (a^s) `rem` p of
                      1 -> True
                      v -> if v == p-1 then True
                                       else go t v
    where go 1 _ = False
          go t v = case (v^2) `rem` p of
                     1  -> False -- can only occur if v = p - 1 
                                 -- (there are only 2 numbers 
                                 --  whose inverse it that itself: 1 and p-1)
                     v' -> if v' == p - 1 then True
                                          else go (t-1) v' 

  rest :: Integer -> [(Integer,Integer)]
  rest n = zip rs $ map (\a -> (a^(n-1)) `rem` n) rs
    where rs = [2..n-1]

  liars :: Integer -> Integer
  liars = fromIntegral . length . filter (==1) . map snd . rest 

  strongLiars :: Integer -> Integer
  strongLiars n | even n    = 0
                | otherwise = 
    let (s,t) = odd2t (n-1) 0
        sl    = foldr (\a l -> detector l a s t) [] [2..n-1]
     in fromIntegral $ length sl
    where detector l a s t | isPrime n a s t = a:l
                           | otherwise       =   l

  -------------------------------------------------------------------------
  -- Prime factorisation
  -------------------------------------------------------------------------
  trialfact :: Integer -> [Integer]
  trialfact 1 = []
  trialfact n = let l = floor $ sqrt (fromIntegral n)
                 in case  findf l allprimes of
                       (1,_) -> [n]
                       (p,q) -> if prime q then [p,q]
                                           else p : trialfact q
    where findf _ [] = (1,n)
          findf l (p:ps) | p >  l    = (1,n)
                         | otherwise = case n `quotRem` p of
                                         (q,0) -> (p,q)
                                         (_,r) -> findf l ps

  allfacts :: [[Integer]]
  allfacts = map trialfact [1..]

  pfact :: Integer -> [Integer]
  pfact n | n  < 1    = error "not a positive integer"
          | n == 1    = [n]
          | prime n   = [n]
          | otherwise = let p = ldp n
                            k = n `div` p
                         in if prime k then [p,k]
                                       else p : pfact k

  -------------------------------------------------------------------------
  -- Pollard's Rho Algorithm
  -- Look at Schnorr-Seysen-Lenstra Algorithm!
  -------------------------------------------------------------------------
  rho :: Integer -> Integer
  rho p = go 2 
    where go k | k*k >= p  = p
               | otherwise = 
                 let x = g k
                     y = g x
                  in case gcd p (abs (x-y)) of
                       1 -> go (k+1)
                       f -> if f == p then go (k+1)
                                      else f
          g x = (x^2 + 1) `rem` p

  rhofact :: Integer -> [Integer]
  rhofact p | even p    = 2 : rhofact (p `div` 2)
            | otherwise = case rho p of
                            1 -> []
                            f -> if f == p then [p]
                                           else f : rhofact (p `div` f)

  -------------------------------------------------------------------------
  -- prime factorisation of factorials
  -- Accoding to TAOCP, p. 47
  -------------------------------------------------------------------------
  pInFac :: Integer -> Integer -> Integer
  pInFac n p = p^(go p 1)
    where go q e = let t = n `div` q
                    in if t <= 1 then t
                                 else let e' = e+1 in t + go (p^e') e'

  facfac :: Integer -> [Integer]
  facfac n = go allprimes
    where go (p:ps) = let x = pInFac n p
                       in if x == 1 then [] else x : go ps 

  -------------------------------------------------------------------------
  -- compute binomial coefficients
  -------------------------------------------------------------------------
  powOfp :: Integer -> Integer -> Integer -> Integer
  powOfp n k p | p <= n - k && p > n `div` 2 = 0
               | p > k && p > n - k = 1
               | p*p > n && n `rem` p < k `rem` p = 1
               | otherwise = borrows p (n `div` p) (n `rem` p)
                                       (k `div` p) (k `rem` p)

  borrows :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
  borrows _ 0 _ _ _ = 0
  borrows p u v s t | v < t = 1 + borrows p (u `div` p) (u `rem` p)
                                            (s `div` p) ((s `rem` p) + 1)
                    | otherwise = borrows p (u `div` p) (u `rem` p)
                                            (s `div` p) (s `rem` p)

  choose3 :: Integer -> Integer -> Integer
  choose3 n k = product (foldl' f [] ps)
    where ps  = takeWhile (<= n) allprimes
          {-
          f p = let !e = powOfp n k p
                    !r = p^e
                 in r 
          -}
          f rs p = let !e = powOfp n k p
                       !r = p^e
                    in if e > 0 then r:rs else rs
                 

  testChoose3 :: Integer -> Bool
  testChoose3 m = all go [1..m]
    where go  n   = all (chs n) [0..n]
          chs n k = choose3 n k == choose n k

  ------------------------------------------------------------------------
  -- Goldbach
  ------------------------------------------------------------------------
  goldbach :: Integer -> (Integer, Integer)
  goldbach n | odd n     = error "not even"
             | n == 2    = error "not greater 2"
             | otherwise = go allprimes
    where go [] = error "ouch!"
          go (p:ps) | p > n - 1 = error ("disproved: " ++ show n)
                    | otherwise = let q = n - p 
                       in if prime q then (p,q)
                                     else go ps

  ------------------------------------------------------------------------
  -- Count Primes
  ------------------------------------------------------------------------
  countp :: Integer -> Integer
  countp n = (fromIntegral . length) $ takeWhile (< n) allprimes

  ratiop :: Integer -> Double
  ratiop n = let d = fromIntegral n
                 c = fromIntegral $ countp n
              in d / c

  steps :: [(Integer, Integer)]
  steps = go $ map (\x -> (x, floor $ ratiop x)) [10,20..]
    where go [] = []
          go [x] = [x]
          go ((n1,r1):(n2,r2):xs) | r1 == r2  = go ((n2,r2):xs)
                                  | otherwise = (n2,r2) : go ((n2,r2):xs)

  ------------------------------------------------------------------------
  -- Totient and Carmichael
  ------------------------------------------------------------------------
  fracs1 :: Integer -> [Rational]
  fracs1 n = [x % n | x <- [1..n-1]]

  fracs2 :: Integer -> [Rational]
  fracs2 n = filter (\r -> denominator r == n) [x % n | x <- [1..n-1]]

  tot :: Integer -> Integer
  tot = fromIntegral . length . coprimes

  cotot :: Integer -> Integer
  cotot n = n - tot n

  divtot :: Integer -> [Integer]
  divtot 1 = [1]
  divtot 2 = [1]
  divtot n = [tot x | x <- [1..n-1], rem n x == 0]

  sumtot :: Integer -> Integer
  sumtot 1 = 1
  sumtot 2 = 1
  sumtot n = n - sum [sumtot d | d <- divs n, d < n]

  coprimes :: Integer -> [Integer]
  coprimes n = [x | x <- [0..n-1], gcd n x == 1]

  divs :: Integer -> [Integer]
  divs n = [d | d <- [1..n], rem n d == 0]

  -- partition numbers 1..n according to gcd with n
  gcdpar :: Integer -> [[Integer]]
  gcdpar n = map par (divs n)
    where par d = [m | m <- [1..n], gcd n m == d]

  euprod :: Integer -> Double
  euprod n = (fromIntegral n) * product (
             nub [1 - 1 / (fromIntegral p) | p <- trialfact n])

  ptot :: Integer -> Integer 
  ptot n =  let  r   = n % 1 
                 t   = r * product [(p-1) % p | p <- nub (trialfact n)] 
             in if denominator t == 1 then numerator t
                                      else error "totient not an integer"

  mutot2 :: Integer -> Double 
  mutot2 n = (fromIntegral n) * sum [
              (fromIntegral $ moebius x) / (fromIntegral x) 
              | x <- [1..n], rem n x == 0]

  mutot :: Integer -> Integer
  mutot n = sum [x * moebius (n `div` x) | x <- [1..n], rem n x == 0]

  eutheorem :: Integer -> Integer -> Bool
  eutheorem a n = (a^tot n) `rem` n == 1

  tstEuTheorem :: Integer -> Bool
  tstEuTheorem n = and [eutheorem a n | a <- [1..n], gcd n a == 1]

  lambda :: Integer -> Integer
  lambda 2 = tot 2
  lambda 4 = tot 4
  lambda n | twopower   n = (tot n) `div` 2 
           | primepower n = tot n
           | even n && primepower (n `div` 2) = tot n
           | otherwise    = let ps = map lambda (simplify $ trialfact n)
                             in foldl' lcm 1 ps
    where simplify = map product . group . sort

  carfilter :: Integer -> Bool
  carfilter 2 = False
  carfilter 4 = False
  carfilter n | even n && length (nub $ trialfact n) == 1 = True
              | otherwise                                 = False

  pows2 :: [Integer]
  pows2 = go 2
    where go n = n : go (2*n)

  pows2n :: Integer -> [Integer]
  pows2n n = takeWhile (<=n) pows2

  primepower :: Integer -> Bool
  primepower n = length (nub $ trialfact n) == 1

  twopower :: Integer -> Bool
  twopower 2 = True
  twopower n | even n    = twopower (n `div` 2)
             | otherwise = False

  perfectSquare :: Integer -> Bool
  perfectSquare n = let x = floor (sqrt (fromIntegral n)) 
                     in n == x*x

  intRoot :: Integer -> Integer
  intRoot n = let x = floor $ sqrt (fromIntegral n)
               in if n == x*x then x
                              else undefined
  
  tstLambda :: Integer -> IO Bool
  tstLambda n = do
    a <- randomCoprime n
    let l = lambda n
    let x = (a^l) `rem` n
    return (x == 1)

  tstLambda2 :: Integer -> Bool
  tstLambda2 n = let cs = [c | c <- [2..n-2], gcd n c == 1]
                     l  = lambda n
                     rs = go l cs
                  in and (map fst rs) &&
                     not (and $ map snd rs)
    where go _ [] = [(True, True)]
          go l (c:cs) = let x  = (c^l) `rem` n
                            sc = [(c^s) `rem` n | s <- [2..l-1]]
                         in (x == 1, 1 `elem` sc) : go l cs

  failedL :: [Integer] -> IO [Integer]
  failedL [] = return []
  failedL (n:ns) = do x <- tstLambda n
                      if x then failedL ns
                           else (n:) <$> failedL ns

  failedL2 :: [Integer] -> [Integer]
  failedL2 [] = []
  failedL2 (n:ns) | tstLambda2 n  = failedL2 ns
                  | otherwise = n : failedL2 ns

  randomCoprime :: Integer -> IO Integer
  randomCoprime n = go n
    where go 0 = error "no coprime found!"
          go i = do
            x <- randomRIO (2,n-2) -- for n-1, trivially n^2 = 1
            if gcd n x == 1 then return x else go (i-1)

  ------------------------------------------------------------------------
  -- Riemann
  ------------------------------------------------------------------------
  moebius :: Integer -> Integer
  moebius = chk . trialfact
    where chk f | f /= nub f =  0
                | otherwise  = (-1)^(length f)

  mu :: [Integer]
  mu = map moebius [1..]

  mertens :: Integer -> Integer
  mertens n = sum $ map moebius [1..n]

  leg :: Integer -> Double
  leg n = r / (log r)
    where r = fromInteger n

  -- should be the logarithmic integral
  li :: Double -> Double
  li n = 1 / log n 

  dleg :: Integer -> Double
  dleg n = (fromIntegral $ countp n) - (leg n)

  dri :: Integer -> Double
  dri n = (fromIntegral $ countp n) - (ri3 n)

  ri :: Integer -> Double
  ri n = sum $ map (rif n) [1..n]

  ri2 :: Integer -> Integer -> Double
  ri2 n m = go 1
    where go k | k == m    = 0
               | otherwise = rif n k + go (k+1)

  ri3 :: Integer -> Double
  ri3 n = ri2 n n

  rif :: Integer -> Integer -> Double
  rif n k = let rk = fromIntegral k
                rm = fromIntegral $ moebius k
                c  = rm / rk
             in c * li (r**(1/rk))
    where r   = fromIntegral n

  -------------------------------------------------------------------------
  -- Find Prime Ideals (first ist duplicated)
  -------------------------------------------------------------------------
  allideals :: Integer -> Integer -> [Integer]
  allideals b s = s : ideals b s 0

  ldi :: Integer -> Integer -> Integer -> Integer
  ldi b s = ldpf (allideals b s)

  ideals :: Integer -> Integer -> Integer -> [Integer]
  ideals b i1 0  = filter (ideal b i1) (follow b i1 0)
  ideals b i1 i2 = filter (ideal b i1) (follow b i1 i2)

  ------------------------------------------------------------------------
  -- Ideal Tests
  ------------------------------------------------------------------------
  ideal :: Integer -> Integer -> Integer -> Bool
  ideal b s n | n < 1     = error "Not a positive integer!"
              | n == 1    = False
              | otherwise = ldi b s n == n

  ------------------------------------------------------------------------
  -- Some way to build a sequence
  ------------------------------------------------------------------------
  follow :: Integer -> Integer -> Integer -> [Integer]
  follow b s e | e /= 0 && s >= e = []
               | otherwise        = s : follow b (s+b) e

  -------------------------------------------------------------------------
  -- computing
  -------------------------------------------------------------------------
  raise :: Integer -> Integer -> Integer -> Integer
  raise a b n = go (a `rem` n) (b `rem` n)
    where go x 1 = x
          go x y = go ((x * x) `rem` n) (y-1)

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [o] -> let p = read o
              in putStrLn (show p ++ 
                    if prime p then " is prime"
                               else " is not prime")
      (o:os) -> let p1 = read o
                    p2 = read (head os)
                 in print (primes p1 p2)
      _      ->     print allprimes

