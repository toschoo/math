{-# Language BangPatterns #-}
---------------------------------------------------------------------------
-- Polynomials
---------------------------------------------------------------------------
module Poly
where

  import           Data.List (nub,foldl')
  import           Data.Ratio
  import           Control.Applicative ((<$>))
  import           Control.Concurrent
  import           Control.Monad (when,void)
  import           Debug.Trace (trace)
  import           System.Random (randomRIO)

  import qualified Binom   as B
  import qualified Perm
  import qualified Modular as M
  import qualified Prime   as P
  import qualified Linear  as L
  import qualified Real    as R

  data Poly a = P [a]
    deriving (Eq,Show)

  -------------------------------------------------------------------------
  -- Clean constructor 
  -------------------------------------------------------------------------
  poly :: (Num a, Eq a) => [a] -> Poly a
  poly = P . cleanz

  -------------------------------------------------------------------------
  -- Get the coefficients
  -------------------------------------------------------------------------
  coeffs :: Poly a -> [a]
  coeffs (P as) = as

  -------------------------------------------------------------------------
  -- Print in math notation
  -------------------------------------------------------------------------
  pretty :: (Num a, Show a, Eq a) => Poly a -> String
  pretty p = go (reverse $ weigh p)
    where go [] = ""
          go ((i,c):cs) = let x | i == 0    = ""
                                | i == 1    = "x"
                                | otherwise = "x^" ++ show i
                              t | c == 0    = "0"
                                | c == 1    = x
                                | otherwise = show c ++ x
                              o | null cs   = ""
                                | otherwise = " + "
                           in if c == 0 then go cs else t++o++go cs

  -------------------------------------------------------------------------
  -- Apply the polynomial (substitute x for a number)
  -------------------------------------------------------------------------
  apply :: Num a => Poly a -> a -> a
  apply (P cs) x = sum [c*x^i | (i,c) <- zip [0..] cs]

  -------------------------------------------------------------------------
  -- Map apply 
  -------------------------------------------------------------------------
  mapply :: Num a => Poly a -> [a] -> [a]
  mapply p = map (apply p)

  -------------------------------------------------------------------------
  -- Apply a weight to each term 
  -------------------------------------------------------------------------
  weigh :: (Num a) => Poly a -> [(Integer,a)]
  weigh (P []) = []
  weigh (P as) = (zip [0..] as)

  -------------------------------------------------------------------------
  -- Degree of a polynomial is the number of elements minus 1
  -- note that the degree is the greatest exponent 
  --      appearing in the polynomial. (P [a]), hence, has degree 0.
  -------------------------------------------------------------------------
  degree :: Poly a -> Int
  degree (P as) = length as - 1

  -------------------------------------------------------------------------
  -- Addition
  -------------------------------------------------------------------------
  add :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
  add = strich (+)

  -------------------------------------------------------------------------
  -- Addition over finite field
  -------------------------------------------------------------------------
  addp :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  addp p a b = modp p (add a b)

  -------------------------------------------------------------------------
  -- Subtraction
  -------------------------------------------------------------------------
  sub :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
  sub = strich (-)

  -------------------------------------------------------------------------
  -- Subtraction over finite field
  -------------------------------------------------------------------------
  subp :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  subp p a b = modp p (sub a b)

  -------------------------------------------------------------------------
  -- Sum
  -------------------------------------------------------------------------
  sump :: (Num a, Eq a) => [Poly a] -> Poly a
  sump = foldl' add (P [0])

  -------------------------------------------------------------------------
  -- Generic Strichrechnung
  -------------------------------------------------------------------------
  strich :: (Num a, Eq a) => (a -> a -> a) -> Poly a -> Poly a -> Poly a
  strich o (P x) (P y)     = P $ strichlist o x y

  -------------------------------------------------------------------------
  -- Generic Strichrechnung on lists of coefficients
  -------------------------------------------------------------------------
  strichlist :: (Num a, Eq a) => (a -> a -> a) -> [a] -> [a] -> [a]
  strichlist o xs ys = cleanz (go xs ys)
    where go [] bs         = bs
          go as []         = as
          go (a:as) (b:bs) = a `o` b : go as bs

  -------------------------------------------------------------------------
  -- Folding a list of lists of coefficients using strichrechnung
  -------------------------------------------------------------------------
  strichf :: (Num a, Eq a) => (a -> a -> a) -> [[a]] -> [a]
  strichf o = foldl' (strichlist o) []

  -------------------------------------------------------------------------
  -- Multiplication over an infinite field
  -------------------------------------------------------------------------
  mul :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
  mul p1 p2 | d2 > d1   =  mul p2 p1
            | otherwise =  P (strichf (+) ms)
    where d1 = degree p1
          d2 = degree p2
          ms = [mul1 (*) i (coeffs p1) p | (i,p) <- zip [0..] (coeffs p2)]

  -------------------------------------------------------------------------
  -- Multiplication over a finite field (mod p)
  -------------------------------------------------------------------------
  mulmp :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  mulmp p p1 p2 | d2 > d1   =  mulmp p p2 p1
                | otherwise =  P [m `mmod` p | m <- strichf (+) ms]
    where ms = [mul1 o i (coeffs p1) c | (i,c) <- zip [0..] (coeffs p2)]
          d1 = degree p1
          d2 = degree p2
          o  = modmul p

  -------------------------------------------------------------------------
  -- Mapping (a*) on a list of coefficients
  -------------------------------------------------------------------------
  mul1 :: Num a => (a -> a -> a) -> Int -> [a] -> a -> [a]
  mul1 o i as a = zeros i ++ go as a
    where go [] _     = []
          go (c:cs) x = c `o` x : go cs x 

  -------------------------------------------------------------------------
  -- Multiply a list of coefficients (infinite field)
  -------------------------------------------------------------------------
  mulist :: (Show a, Num a, Eq a) => [a] -> [a] -> [a]
  mulist c1 c2 = coeffs $ mul (P c1) (P c2)

  -------------------------------------------------------------------------
  -- Multiply a list of coefficients (mod p)
  -------------------------------------------------------------------------
  mulmlist :: Integer -> [Integer] -> [Integer] -> [Integer]
  mulmlist p c1 c2 = coeffs $ mulmp p (P c1) (P c2)
          
  -------------------------------------------------------------------------
  -- Scale a polynomial
  -------------------------------------------------------------------------
  scale :: (Num a) => a -> Poly a -> Poly a 
  scale n (P cs) = P (map (n*) cs)

  -------------------------------------------------------------------------
  -- Division (infinite field)
  -------------------------------------------------------------------------
  divp :: (Show a, Num a, Eq a, Fractional a, Ord a) => 
          Poly a -> Poly a -> (Poly a,Poly a)
  divp (P as) (P bs) = let (q,r) = go [] as in (P q, P r)
    where go q r | degree (P r) < db  = (q,r)
                 | null r || r == [0] = (q,r)
                 | otherwise          = -- trace (show (q,r)) $
                     let t  = last r / last bs
                         d  = degree (P r) - db
                         ts = zeros d ++ [t]
                         m  = mulist ts bs
                      in go (cleanz $ strichlist (+) q ts)
                            (cleanz $ strichlist (-) r m)
          db = degree (P bs)

  -------------------------------------------------------------------------
  -- Division (mod p)
  -------------------------------------------------------------------------
  divmp :: Integer -> 
           Poly Integer -> Poly Integer -> (Poly Integer,Poly Integer)
  divmp p (P as) (P bs) = let (q,r) = go [0] as in (P q, P r)
    where go q r | degree (P r) < db  = (q,r)
                 | null r || r == [0] = (q,r)
                 | otherwise          = 
                     let t  = modiv p (last r) (last bs)
                         d  = degree (P r) - db
                         ts = zeros d ++ [t]
                         m  = mulmlist p ts bs
                      in go (cleanz [c `mmod` p | c <- strichlist (+) q ts])
                            (cleanz [c `mmod` p | c <- strichlist (-) r m ])
          db = degree (P bs)

  -------------------------------------------------------------------------
  -- Creating a trail of zeros
  -------------------------------------------------------------------------
  zeros :: Num a => Int -> [a]
  zeros i = take i $ repeat 0

  -------------------------------------------------------------------------
  -- Remove leading zeros
  -------------------------------------------------------------------------
  cleanz :: (Eq a, Num a) => [a] -> [a]
  cleanz xs = reverse $ go (reverse xs)
    where go []  = []
          go [0] = [0]
          go (0:zs) = go zs
          go zs     = zs

  -------------------------------------------------------------------------
  -- Divides (generic)
  -------------------------------------------------------------------------
  divides :: (Show a, Num a, Eq a, Ord a) => 
             (Poly a -> Poly a -> (Poly a, Poly a)) ->
             Poly a -> Poly a -> Bool
  divides d a b = case b `d` a of
                  (_,P [0]) -> True
                  _         -> False

  -------------------------------------------------------------------------
  -- GCD (infinite field)
  -------------------------------------------------------------------------
  gcdp :: (Show a, Num a, Eq a, Fractional a, Ord a) => 
          Poly a -> Poly a -> Poly a
  gcdp a b | degree b > degree a = gcdp b a
           | zerop b = a
           | otherwise = let (_,r) = divp a b in gcdp b r

  -------------------------------------------------------------------------
  -- GCD (mod p)
  -------------------------------------------------------------------------
  gcdmp :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  gcdmp p a b | degree b > degree a = gcdmp p b a
              | zerop b = a
              | otherwise = let (_,r) = divmp p a b in gcdmp p b r
 
  -------------------------------------------------------------------------
  -- Null
  -------------------------------------------------------------------------
  zerop :: (Num a, Eq a) => Poly a -> Bool
  zerop (P [0]) = True
  zerop _       = False

  -------------------------------------------------------------------------
  -- unity
  -------------------------------------------------------------------------
  unityp :: Integer -> Poly Integer -> Bool
  unityp _ (P [1]) = True
  unityp p (P [x]) = x `mmod` p `elem` [1,p-1]
  unityp _ _       = False      

  -------------------------------------------------------------------------
  -- Derivatives (generic)
  -------------------------------------------------------------------------
  derivative :: (Eq a, Num a) => (a -> a -> a) -> Poly a -> Poly a
  derivative o (P as) = P (cleanz (go $ zip [1..] (drop 1 as)))
    where go []         = []
          go ((x,c):cs) = (z `o` c) : go cs where z = fromIntegral x

  -------------------------------------------------------------------------
  -- Squarefree
  -- ----------
  -- usually the test is gcd u (derivative u) == 1
  -- however, in computing the gcd, one would usually factor
  --          the remainder, for instance:
  -- rem (x^2 + 7x + 6) (x^2 - 5x - 6) = 12(x+1)
  --     and then continue with
  -- rem (x^2-5x-6) (x+1) = 0 (since (x+1)(x-6) = (x^2-5x-6))
  --     resulting in the gcd x+1, otherwise,
  --     the gcd would have been 12x+12
  --     This is called content-and-primitive-part factorisation
  --     and should always be applied 
  --     before searching for non-trivial factors.
  -------------------------------------------------------------------------
  squarefree :: Integer -> Poly Integer -> Bool
  squarefree p u = degree (gcdmp p u (derivative (modmul p) u)) == 0

  -------------------------------------------------------------------------
  -- Squared factor
  -------------------------------------------------------------------------
  squarefactor :: (Show a, Num a, Eq a, Enum a, Fractional a, Ord a) => 
                  Poly a -> [Poly a]
  squarefactor u | degree sq  > 0 = [sq]
                 | otherwise      = []
    where dv = derivative (*) u
          sq = gcdp u dv

  -------------------------------------------------------------------------
  -- Powers of...
  -------------------------------------------------------------------------
  powers :: Integer -> Poly Integer -> [Poly Integer]
  powers p u = go u
    where go sq = pow sq : go (pow sq) 
          pow   = mulmp p u
  
  -------------------------------------------------------------------------
  -- Factoring: Kronecker
  -- receives the polynomial to be factored
  -- a list of integers which are results of applying the polynomial
  -- (the length of the list determines the degree of the factor,
  --  where degree = length - 1)
  -------------------------------------------------------------------------
  kronecker :: Poly Integer -> [Integer] -> [Poly Rational]
  kronecker (P cs) is = nub [a | a <- as, snd (r `divp` a) == P [0]]
    where ds = map divs is
          ps = concatMap Perm.perms (Perm.listcombine ds)
          as = map (P . map fromInteger) ps
          r  = P [c%1 | c <- cs]

  divs :: Integer -> [Integer]
  divs i | i < 0     = divs (-i) 
         | otherwise = ds ++ map negate ds
    where ds = [d | d <- [1..i], rem i d == 0] 

  -------------------------------------------------------------------------
  -- Irreducible mod p
  -------------------------------------------------------------------------
  irreducible :: Integer -> Poly Integer -> Bool
  irreducible p u | d < 2     = True
                  | otherwise = go 1 x
    where d      = degree u
          x      = P [0,1]
          go i z = let z' = powmp p p z
                    in case pmmod p (subp p z' x) u of
                     P [0] -> d == i
                     _     -> if i < d then go (i+1) (pmmod p z' u)
                                       else False
          
  -------------------------------------------------------------------------
  -- Factoring: Cantor-Zassenhaus
  -------------------------------------------------------------------------
  cantorzassenhaus :: Integer -> Poly Integer -> IO [(Integer, Poly Integer)] 
  cantorzassenhaus p u | irreducible p m = return [(1,m)]
                       | otherwise       = 
                           concat <$> mapM mexpcz [(e, ddfac p f) | 
                                      (e,f) <- squarefactormod p m]
    where m = monicp p u
          expcz e (d,v)   = map (\f -> (e,f)) <$> cz p d v
          mexpcz (e,dds)  = concat <$> mapM (expcz e) dds

  -------------------------------------------------------------------------
  -- Squared factor (mod p)
  -------------------------------------------------------------------------
  squarefactormod :: Integer -> Poly Integer -> [(Integer, Poly Integer)]
  squarefactormod p = sqmp p 0 

  -------------------------------------------------------------------------
  -- Squared factor (mod p) per exponent
  -------------------------------------------------------------------------
  sqmp :: Integer -> Integer -> Poly Integer -> [(Integer, Poly Integer)]
  sqmp p e u | degree u < 1 = []
             | otherwise    = let u' = derivative (modmul p) u
                                  t  = gcdmp p u u'
                                  v  = fst (divmp p u t)
                               in go 1 t v
    where go k tk vk = let vk' | k `rem` p /= 0 = gcdmp p tk vk
                               | otherwise      = vk
                           tk' = fst (divmp p tk vk')
                           k'  = k + 1
                        in case divmp p vk vk' of
                             (P [_],_) ->             nextStep k' tk' vk'
                             (f,_)     -> (k*p^e,f) : nextStep k' tk' vk'
          nextStep k tk vk | degree vk > 0 = go k tk vk
                           | degree tk > 0 = sqmp p (e+1) (mkNextTk tk)
                           | otherwise     = []
          mkNextTk tk = poly (nexT 0 (coeffs tk))
          nexT _ [] = []
          nexT i (c:cs) | i `rem` p == 0 = c : nexT (i+1) cs
                        | otherwise      =     nexT (i+1) cs

  -------------------------------------------------------------------------
  -- Discrete degree factorisation
  -------------------------------------------------------------------------
  ddfac :: Integer -> Poly Integer -> [(Int, Poly Integer)]
  ddfac p u = go 1 u (P [0,1])
    where go d v x | degree v <= 0 = []
                   | otherwise = 
                     let x'     = powmp p p x 
                         t      = addp p x' (P [0,p-1])
                         g      = gcdmp p t v
                         (v',_) = divmp p v g
                         r      = (d,monicp p g)
                      in case g of
                           P [_] ->     go (d+1) v' (pmmod p x' u) 
                           _     -> r : go (d+1) v' (pmmod p x' u) 

  -------------------------------------------------------------------------
  -- Cantor-Zassenhaus factor splitting
  -------------------------------------------------------------------------
  cz :: Integer -> Int -> Poly Integer -> IO [Poly Integer]
  cz p d u | n <= d    = return [monicp p u]
           | otherwise = do 
    x <- monicp p <$> randomPoly p (2*d) -- 2*d-1
    let t | p == 2    = addsquares (d-1) p x u
          | otherwise = addp p (powmodp p m x u) (P [p-1])
    let r = gcdmp p u t
    if degree r <= 0 || degree r >= n then cz p d u
      else do r1 <- cz p d r 
              r2 <- cz p d (fst $ divmp p u r) 
              return (r1 ++ r2)
    where n = degree u
          m = (p^d-1) `div` 2

  -------------------------------------------------------------------------
  -- Power for even primes (i.e. 2)
  -------------------------------------------------------------------------
  addsquares :: Int -> Integer -> Poly Integer -> Poly Integer -> Poly Integer
  addsquares i p x u = go i x x
    where go 0 w _ = w
          go k w t = let t' = pmmod p (powmp p p t) u
                         w' = addp p w t'
                      in go (k-1) w' t'

  -------------------------------------------------------------------------
  -- Make monic, i.e. divide by leading coefficient
  -------------------------------------------------------------------------
  monicp :: Integer -> Poly Integer -> Poly Integer
  monicp p u = let cs  = coeffs u
                   k   = last cs `M.inverse` p
                in P (cleanz $ map (modmul p k) cs)

  -------------------------------------------------------------------------
  -- Produce a random polynomial (modulo p)
  -- note: d indicates the number of coefficients (not the degree!)
  --       is that correct? check cantor-zassenhaus!
  -------------------------------------------------------------------------
  randomPoly :: Integer -> Int -> IO (Poly Integer)
  randomPoly p d = do
    cs <- cleanz <$> mapM (\_ -> randomCoeff p) [1..d]
    if length cs < d then randomPoly p d
                     else return (P cs)

  -------------------------------------------------------------------------
  -- Produce a random monic squarefree polynomial (modulo p) 
  -------------------------------------------------------------------------
  msRandomPoly :: Integer -> Int -> IO (Poly Integer)
  msRandomPoly p d = do
    r <- monicp p <$> randomPoly p d
    if squarefree p r then return r else msRandomPoly p d

  -------------------------------------------------------------------------
  -- Produce a random coefficient (modulo p)
  -------------------------------------------------------------------------
  randomCoeff :: Integer -> IO Integer
  randomCoeff p = randomRIO (0,p-1)

  -------------------------------------------------------------------------
  -- Produce a random prime
  -------------------------------------------------------------------------
  randomPrime :: Integer -> IO Integer
  randomPrime k = do
    n <- randomRIO (2^(k-1),2^k-1)
    t <- P.rabinMiller 64 n
    if t then return n else randomPrime k

  -------------------------------------------------------------------------
  -- Deep Test Cantor-Zassenhaus
  -------------------------------------------------------------------------
  deepCantorZass :: Int -> IO Bool
  deepCantorZass 0 = return True
  deepCantorZass i = do
    k <- randomRIO(0,4)
    let p = ps!!k
    let x | k == 4    = "" 
          | otherwise = " "
    putStr (x ++ show p ++ " - ")
    t <- tstCantorZass 1 p
    if t then deepCantorZass (i-1)
         else return False
    where ps = [2,3,5,7,11]
    

  -------------------------------------------------------------------------
  -- Test Cantor-Zassenhaus
  -------------------------------------------------------------------------
  tstCantorZass :: Int -> Integer -> IO Bool
  tstCantorZass 0 _ = return True
  tstCantorZass i p = do
    d  <- randomRIO (3,8)
    x  <- monicp p <$> randomPoly p d
    putStr (showp x ++ ": ")
    fs <- cantorzassenhaus p x
    putStrLn (show fs)
    if null fs then return False
               else if checkFactors p x fs then tstCantorZass (i-1) p
                                           else return False

  showp :: Poly Integer -> String
  showp p | a < 0     = show p
          | otherwise = show p ++ sp
    where l  = length(show p)
          a  = 20 - l
          sp = take a (repeat ' ')

  -------------------------------------------------------------------------
  -- Check Factors
  -------------------------------------------------------------------------
  checkFactors :: Integer -> Poly Integer -> [(Integer,Poly Integer)] -> Bool
  checkFactors p x fs = prodp (mulmp p) [powmp p i u | (i,u) <- fs] == x

  -------------------------------------------------------------------------
  -- product
  -------------------------------------------------------------------------
  prodp :: Num a => (Poly a -> Poly a -> Poly a) -> [Poly a] -> Poly a
  prodp o ps = foldl' o (P [1]) ps

  -------------------------------------------------------------------------
  -- pow (square-and-multiply)
  -------------------------------------------------------------------------
  powp :: (Eq a, Show a, Num a) => Integer -> Poly a -> Poly a 
  powp f u = go f (P [1]) u
    where go 0 y _  = y
          go 1 y x  = mul y x
          go n y x | even n    = go (n `div` 2) y   (mul x x) 
                   | otherwise = go ((n-1) `div` 2) (mul y x) 
                                                    (mul x x)

  -------------------------------------------------------------------------
  -- pow (naive)
  -------------------------------------------------------------------------
  powmp2 :: Integer -> Integer -> Poly Integer -> Poly Integer
  powmp2 p f u = go f u 
    where go 0 _ = P [1]
          go 1 x = x
          go n x = go (n-1) (mulmp p u x) -- better: double+add

  -------------------------------------------------------------------------
  -- pow (square-and-multiply)
  -------------------------------------------------------------------------
  powmp :: Integer -> Integer -> Poly Integer -> Poly Integer
  powmp p f u = go f (P [1]) u
    where go 0 y _ = y
          go 1 y x = mulmp p y x
          go n y x | even n    = go (n `div` 2) y   (mulmp p x x) 
                   | otherwise = go ((n-1) `div` 2) (mulmp p y x) 
                                                    (mulmp p x x)

  -------------------------------------------------------------------------
  -- pow (square-and-multiply) modulo a polynomial
  -------------------------------------------------------------------------
  powmodp :: Integer -> Integer -> Poly Integer -> Poly Integer -> Poly Integer
  powmodp p f v u = go f (P [1]) v
    where go 0 y _ = y
          go 1 y x = mulmp p y x
          go n y x | even n    = go (n `div` 2) y   (pmmod p (mulmp p x x) u)
                   | otherwise = go ((n-1) `div` 2) (pmmod p (mulmp p y x) u)
                                                    (pmmod p (mulmp p x x) u)

  -------------------------------------------------------------------------
  -- Test function
  -------------------------------------------------------------------------
  tstPow2 :: Int -> Integer -> IO Bool
  tstPow2 0 _ = return True
  tstPow2 i p = do
    d <- randomRIO (2,5)
    n <- randomRIO (0,25)
    x <- randomPoly p d
    let r  = powmp p n x
    let r' = powmp2 p n x
    if r == r' then tstPow2 (i-1) p
               else return False

  -------------------------------------------------------------------------
  -- Integer Multiplication mod p
  -------------------------------------------------------------------------
  modmul :: Integer -> Integer -> Integer -> Integer
  modmul p f1 f2 = (f1 * f2) `mmod` p

  -------------------------------------------------------------------------
  -- Integer Division mod p
  -------------------------------------------------------------------------
  modiv :: Integer -> Integer -> Integer -> Integer
  modiv p n d = modmul p n d'
    where d' = M.inverse d p

  -------------------------------------------------------------------------
  -- n mod p
  -------------------------------------------------------------------------
  mmod :: Integer -> Integer -> Integer
  mmod n p | n < 0 && (-n) > p = mmod (-(mmod (-n)) p) p
           | n < 0             = mmod (p + n) p
           | otherwise         = n `rem` p

  -------------------------------------------------------------------------
  -- Make polynomial mod p
  -------------------------------------------------------------------------
  modp :: Integer -> Poly Integer -> Poly Integer
  modp p (P as) = P (cleanz [a `mmod` p | a <- as])

  -------------------------------------------------------------------------
  -- Polynomial mod Polynomial
  -------------------------------------------------------------------------
  pmmod :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  pmmod p u m = snd (divmp p u m) 

  -------------------------------------------------------------------------
  -- Comparison
  -------------------------------------------------------------------------
  cmp :: (Num a, Eq a, Ord a) => [a] -> [a] -> Ordering
  cmp a  b  | length a < length b = LT
            | length b > length b = GT
            | otherwise           = go a b
    where go [] [] = EQ
          go [] _  = LT
          go _  [] = GT
          go (x:xs) (y:ys) | x < y     = LT
                           | y < x     = GT
                           | otherwise = go xs ys

  -------------------------------------------------------------------------
  -- Differences
  -------------------------------------------------------------------------
  diffs :: [Integer] -> [Integer]
  diffs []  = []
  diffs [_] = []
  diffs (a:b:cs) = (b-a):diffs (b:cs)

  -------------------------------------------------------------------------
  -- diffs lists
  -------------------------------------------------------------------------
  dengine :: [Integer] -> [[Integer]]
  dengine cs  | constant cs = []
              | otherwise   = ds : dengine ds
    where ds = diffs cs
          constant []     = True
          constant [_]    = True
          constant (x:xs) = all (==x) xs

  -------------------------------------------------------------------------
  -- predict
  -------------------------------------------------------------------------
  predict :: [[Integer]] -> [Integer] -> Maybe Integer
  predict ds xs = case go (reverse ds) of
                    0  -> Nothing
                    d  -> Just (d + (last xs))
    where go []   = 0
          go [[]] = 0
          go [a]  = last a
          go (a:cs) = last a + go cs

  -------------------------------------------------------------------------
  -- Predict Degree
  -------------------------------------------------------------------------
  dpredict :: [[Integer]] -> Int
  dpredict [] = -1
  dpredict ds = length ds

  -------------------------------------------------------------------------
  -- Newton
  -------------------------------------------------------------------------
  newton :: Integer -> Integer -> [[Integer]] -> [Integer] -> Integer
  newton s n ds sq = sum ts
    where hs = getHeads sq ds
          ts = [h * (B.choose (n-s) k) | (h,k) <- zip hs [0..n]]

  -------------------------------------------------------------------------
  -- Get Heads
  -------------------------------------------------------------------------
  getHeads :: [Integer] -> [[Integer]] -> [Integer]
  getHeads sq ds = map head (sq:ds)

  -------------------------------------------------------------------------
  -- Newton Polynomial
  -------------------------------------------------------------------------
  newtonp :: [[Integer]] -> [Integer] -> Poly Rational
  newtonp ds sq = sump ts
    where hs = getHeads sq ds
          n  = fromIntegral $ dpredict ds
          ts = [bin2poly h k | (h,k) <- zip hs [0..n]]

  -------------------------------------------------------------------------
  -- Express binomials as polynomials 
  -------------------------------------------------------------------------
  bin2poly :: Integer -> Integer -> Poly Rational
  bin2poly f 0 = P [f%1]
  bin2poly f 1 = P [0,f%1]
  bin2poly f k = P [f%(B.fac k)] `mul` go (k%1)
    where go 1 = P [0,1]
          go i = P [-(i-1),1] `mul` (go (i-1))

  -------------------------------------------------------------------------
  -- Find generating polynomial
  -- NOTE: number of columns must be greater than number of rows!
  -------------------------------------------------------------------------
  findGen :: [[Integer]] -> [Integer] -> [Rational]
  findGen ds = L.backsub . L.echelon . findCoeffs ds 

  findCoeffs :: [[Integer]] -> [Integer] -> L.Matrix Integer
  findCoeffs ds sq = L.M (go 0 sq)
    where d = fromIntegral (length ds)
          go _ []  = []
          go n (x:xs) | n > d     = []
                      | otherwise = genCoeff d n x : go (n+1) xs 

  genCoeff :: Integer -> Integer -> Integer -> [Integer]
  genCoeff m n x = go 0 x
    where go i z | i >  m    = [z]
                 | otherwise = n^i : go (i+1) z

  testGauss :: Poly Integer -> L.Matrix Integer
  testGauss p = L.echelon $ findCoeffs ds sq
    where ds = dengine sq
          sq = map (apply p) [0..15]

  -------------------------------------------------------------------------
  -- Newton Demonstrator (degree 3 model)
  -------------------------------------------------------------------------
  data Newton = H | X | Y | Z
    deriving (Show,Eq)

  cn :: Newton -> Integer -> [Newton]
  cn H 0 = [H]
  cn H n = cn H (n-1) ++ cn X (n-1)
  cn X 0 = [X]
  cn X n = cn X (n-1) ++ cn Y (n-1)
  cn Y 0 = [Y]
  cn Y n = Z : cn Y (n-1)
  cn Z _ = undefined

  ccn :: [Newton] -> (Int,Int,Int,Int)
  ccn ls = (length $ filter (== H) ls,
            length $ filter (== X) ls,
            length $ filter (== Y) ls,
            length $ filter (== Z) ls)

  new2a :: (a,a,a,a) -> Newton -> a
  new2a (h,x,y,z) n = case n of 
                        H -> h
                        X -> x
                        Y -> y
                        Z -> z

  subst :: (a,a,a,a) -> [Newton] -> [a]
  subst as = map (new2a as)

  -------------------------------------------------------------------------
  -- Talyor Series (real numbers)
  -------------------------------------------------------------------------
  taylor :: Integer -> Double -> Poly Double  -> Poly Double 
  taylor i a = go 0
    where go n f | n == i = P [0]
                 | otherwise = 
                   let k = apply f a 
                       d = fromIntegral (B.fac n)
                       q = k / d
                       x = scale q (powp n (P [-a,1]))
                    in x `add` go (n+1) (derivative (*) f)

  -------------------------------------------------------------------------
  -- Talyor Series (mod p)
  -------------------------------------------------------------------------
  taylormp :: Integer -> Integer -> Integer -> Poly Integer -> Poly Integer
  taylormp i p a = go 0
    where go n f | n == i = P [0]
                 | otherwise = 
                   let k = apply f a 
                       d = B.fac n
                       q = k * (d `M.inverse` p) 
                       x = modp p $ scale q (powmp p n (P [-a,1]))
                    in addp p x $ go (n+1) (derivative (modmul p) f)

  -------------------------------------------------------------------------
  -- Hensel lifting
  -------------------------------------------------------------------------
  hlift :: Integer -> Integer -> Poly Integer -> Integer -> (Bool, Integer)
  hlift p x u r = ((apply u' r) `mmod` p /= 0, s `mmod` p')
    where u' = derivative (modmul p) u
          a  = (apply u' r) `M.inverse` p'
          s  = r - (apply u r) * a 
          p' = p^x

  -------------------------------------------------------------------------
  -- Numerical root finding
  -------------------------------------------------------------------------
  -- a and b must be of opposite signedness
  bisect :: (Num a, Eq a, Ord a, Fractional a, Show a) => 
            Poly a -> a -> a -> a -> a
  bisect p t a b | abs fc < abs t          = c
                 | signum fc == signum fa  = bisect p t c b 
                 | otherwise               = bisect p t a c 
    where fa = apply p a
          fc = apply p c
          c  = (a+b)/2

  -- newton's method
  newguess :: (Num a, Eq a, Ord a, Enum a, Fractional a) =>
              Poly a -> Int -> a -> a -> a
  newguess p m t a | abs pa < t = a
                   | m <= 0     = a
                   | otherwise  = newguess p (m-1) t (a-pa/p'a)
    where p'  = derivative (*) p
          pa  = apply p a
          p'a = apply p' a

  -------------------------------------------------------------------------
  -- Wilkinson's Polynomial
  -------------------------------------------------------------------------
  wilkinson :: (Num a, Enum a, Show a, Eq a) => Poly a
  wilkinson = prodp mul [P [-i,1] | i <- [1..20]]

  -------------------------------------------------------------------------
  -- Solving equations
  -------------------------------------------------------------------------
  solve :: Poly Double -> [Double]
  solve p = case degree p of
              0 -> coeffs p
              1 -> solvel p
              2 -> solve2 p
              -- 3 -> solve3 p
              -- 4 -> solve4 p
              _ -> error "I don't know how to solve this polynomial"

  solvel :: (Num a,Fractional a) => Poly a -> [a]
  solvel (P [b,a]) = [-b/a]
  solvel _         = error "oops!"

  solve2 :: Poly Double -> [Double]
  solve2 p@(P [c,b,a]) | dis p < 0 = []
                       | x1 /= x2  = [x1,x2]
                       | otherwise = [x1]
    where d  = sqrt (dis p)
          x1 = (-b + d) / 2*a
          x2 = (-b - d) / 2*a

  dis :: Poly Double -> Double
  dis (P [c,b,a]) = b^2 - 4*a*c

  countRoots :: Poly Double -> Int
  countRoots p | dis p > 0 = 2
               | dis p < 0 = 0
               | otherwise = 1

  -- not correct...
  {-
  solve3 :: Poly Double -> [Double]
  solve3 (P [a,0,c,d]) | a /= 1    = solve3 (P [1,0,c/a,d/a])
                       | otherwise =
                         let disc = d^2/4 + c^3/9
                             u3   = -d/2 + sqrt disc
                             v3   = -d/2 - sqrt disc
                             u    | u3  < 0    = -(-u3)**(1/3)
                                  | otherwise  = u3**(1/3)
                             v    | v3  < 0    = -(-v3)**(1/3)
                                  | otherwise = v3**(1/3)
                          in if disc < 0 then [] else [u+v]
  solve3 (P [a,b,c,0]) | a /= 1    = solve3 (P [1,b/a,c/a,0])
                       | otherwise = 
                           let xs = solve2 (P [1,b,c])
                            in nub (0:xs)
  solve3 (P [a,b,c,d]) | a /= 1    = solve3 (P [1,b/a,c/a,d/a])
                       | otherwise = 
                         let p  = -(b^2)/3 + c
                             q  = (2*b^3)/27 - (b*c)/3 + d
                          in [y-a/3 | y <- solve3 (P [1,0,p,q])]
  solve3 _           = error "oops!"

  finduv :: Double -> Double -> Double -> Double -> Double -> (Double,Double)
  finduv d u1 u2 v1 v2 = let one   = (u1,v1)
                             two   = (u1,v2) 
                             three = (u2,v1) 
                             four  = (u2,v2) 
                          in mymin [(x,abs (d-(fst x * snd x))) | 
                                    x <- [one,two,three,four]]
    where mymin []     = (0,0)
          mymin (x:xs) = go x xs
          go x [] = fst x
          go x (z:zs) | snd x < snd z = go x zs
                      | otherwise     = go z zs
  -}
    
