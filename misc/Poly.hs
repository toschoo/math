---------------------------------------------------------------------------
-- Polynomials
---------------------------------------------------------------------------
module Poly
where

  import           Data.List (nub,foldl')
  import           Data.Ratio
  import           Control.Applicative ((<$>))
  import           Debug.Trace (trace)
  import           System.Random (randomRIO)

  import qualified Binom   as B
  import qualified Modular as M
  import qualified Prime   as P
  import qualified Linear  as L

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
  -- Subtraction
  -------------------------------------------------------------------------
  sub :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
  sub = strich (-)

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
  mul :: (Show a, Num a, Eq a) => Poly a -> Poly a -> Poly a
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
                 | otherwise          = -- trace (show (q,r)) $
                     let t  = modiv p (last r) (last bs)
                         d  = degree (P r) - db
                         ts = zeros d ++ [t]
                         m  = mulmlist p ts bs
                      in go [c `mmod` p | c <- cleanz $ strichlist (+) q ts]
                            [c `mmod` p | c <- cleanz $ strichlist (-) r m ]
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
          go (0:xs) = go xs
          go xs     = xs

  -------------------------------------------------------------------------
  -- Make polynomial mod p
  -------------------------------------------------------------------------
  modp :: Integer -> Poly Integer -> Poly Integer
  modp p (P as) = P (cleanz [a `mmod` p | a <- as])

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
  derivative :: (Eq a, Num a, Enum a) => (a -> a -> a) -> Poly a -> Poly a
  derivative o (P as) = P (cleanz (go $ zip [1..] (drop 1 as)))
    where go []         = []
          go ((x,c):cs) = (x `o` c) : go cs

  -------------------------------------------------------------------------
  -- Squarefree
  -- ----------
  -- is this test correct?
  -- usually the test is gcd poly (derivative poly) == 1
  -- however, in computing the gcd, one would usually factor
  --          the remainder, for instance:
  -- rem (x^2 + 7x + 6) (x^2 - 5x - 6) = 12(x+1)
  --     and then continue with
  -- rem (x^2-5x-6) (x+1) = 0 (since (x+1)(x-6) = (x^2-5x-6))
  --     resulting in the gcd x+1, otherwise,
  --     the gcd would have been 12x+12
  -------------------------------------------------------------------------
  squarefree :: Integer -> Poly Integer -> Bool
  squarefree p poly = degree (gcdmp p poly (derivative (modmul p) poly)) == 0

  -------------------------------------------------------------------------
  -- Squared factor (mod p)
  -------------------------------------------------------------------------
  squarefactor :: Integer -> Poly Integer -> Maybe (Poly Integer)
  squarefactor p poly | degree sq  > 0 = Just sq
                      | otherwise      = Nothing 
    where sq = gcdmp p poly (derivative (modmul p) poly)

  -------------------------------------------------------------------------
  -- Powers of...
  -------------------------------------------------------------------------
  powers :: Integer -> Poly Integer -> [Poly Integer]
  powers p poly = go poly
    where go sq = pow sq : go (pow sq) 
          pow   = mulmp p poly
  
  -------------------------------------------------------------------------
  -- Factoring: Cantor-Zassenhaus
  -------------------------------------------------------------------------
  cantorzassenhaus :: Integer -> Poly Integer -> IO [Poly Integer]
  cantorzassenhaus = cantorzass 100
  
  -------------------------------------------------------------------------
  -- Cantor-Zassenhaus (with repetition)
  -------------------------------------------------------------------------
  cantorzass :: Int -> Integer -> Poly Integer -> IO [Poly Integer]
  cantorzass 0 _ u = return [u]
  cantorzass i p u | d <= 1    = return [u]
                   | otherwise = 
    case squarefactor p u of
      Nothing -> do 
        x <- randomPoly p (d-1) -- randomPoly receives the number of coeffs
        case zassen 0 p x u of  -- should it be 'd'?
          [] -> cantorzass (i-1) p u
          gs ->  do g1 <- concat <$> mapM (splitG 10 p) gs
                    let g2 = map fst [divmp p u g | g <- g1]
                    g3 <- concat <$> mapM (cantorzassenhaus p) g2
                    return $ nub (g1++g3)
      Just s1 -> do let (s2,_) = divmp p u s1
                    fs <- cantorzassenhaus p s2
                    return (s1:fs)
    where d  = degree u
            
  -------------------------------------------------------------------------
  -- Cantor-Zassenhaus (the heart of the matter)
  -------------------------------------------------------------------------
  zassen :: Int -> Integer -> Poly Integer -> 
                              Poly Integer -> [(Int,Poly Integer)]
  zassen d p w v | d   > (degree v) `div` 2 = []
                 | otherwise                = -- trace (show w ++ ", " ++ show v) $
                     let w' = pmmod p (powmp p p w) v
                      in case gcdmp p ((sub w' (P [0,1])) `pmod` p) v of
                           P [_] -> zassen (d+1) p w' v
                           g     -> let (v',_) = divmp p v g
                                        w''    = pmmod p w' v'
                                     in (d,g) : zassen (d+1) p w'' v'

  -------------------------------------------------------------------------
  -- Factoring: factor product of factors
  -------------------------------------------------------------------------
  splitG :: Int -> Integer -> (Int,Poly Integer) -> IO [Poly Integer]
  splitG _ _ (1,g)    = return [g]
  splitG i p (d,g) = do
    t <- randomPoly p (2*d-1)
    let x  = powmp p (p^d - 1) t
    let x1 = add x (P  [1])
    let x2 = add x (P [-1])
    let r1 = gcdmp p g x1
    let r2 = gcdmp p g x2
    if degree r1 > 1 && 
       degree r2 > 1 then return [r1,r2]
                     else if i == 0 
                          then return [g]
                          else splitG (i-1) p (d,g)

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
  -- Test Cantor-Zassenhaus
  -------------------------------------------------------------------------
  tstCantorZass :: Int -> Integer -> IO Bool
  tstCantorZass 0 _ = return True
  tstCantorZass i p = do
    d  <- randomRIO (3,6)
    x  <- randomPoly p d
    putStr (showp x ++ ": ")
    fs <- cantorzassenhaus p x
    putStrLn (show fs)
    if null fs then tstCantorZass (i-1) p
               else if checkFactors p x fs then tstCantorZass (i-1) p
                                           else return False
    where showp p | a < 0     = show p
                  | otherwise = show p ++ sp
            where d  = degree p 
                  l  = 2*d + 1
                  a  = 13 - l
                  sp = take a (repeat ' ')

  -------------------------------------------------------------------------
  -- Check Factors
  -------------------------------------------------------------------------
  checkFactors :: Integer -> Poly Integer -> [Poly Integer] -> Bool
  checkFactors p x [] = True
  checkFactors p x fs = prodp (mulmp p) fs == x

  -------------------------------------------------------------------------
  -- product
  -------------------------------------------------------------------------
  prodp :: Num a => (Poly a -> Poly a -> Poly a) -> [Poly a] -> Poly a
  prodp o ps = foldl' o (P [1]) ps

  -------------------------------------------------------------------------
  -- pow (square-and-multiply)
  -------------------------------------------------------------------------
  powp :: Integer -> Poly Integer -> Poly Integer
  powp f poly = go f (P [1]) poly
    where go 0 y _  = y
          go 1 y x  = mul y x
          go n y x | even n    = go (n `div` 2) y   (mul x x) 
                   | otherwise = go ((n-1) `div` 2) (mul y x) 
                                                    (mul x x)

  -------------------------------------------------------------------------
  -- pow (naive)
  -------------------------------------------------------------------------
  powmp2 :: Integer -> Integer -> Poly Integer -> Poly Integer
  powmp2 p f poly = go f poly
    where go 0 x = P [1]
          go 1 x = x
          go n x = go (n-1) (mulmp p poly x) -- better: double+add

  -------------------------------------------------------------------------
  -- pow (square-and-multiply)
  -------------------------------------------------------------------------
  powmp :: Integer -> Integer -> Poly Integer -> Poly Integer
  powmp p f poly = go f (P [1]) poly
    where go 0 y _ = y
          go 1 y x = mulmp p y x
          go n y x | even n    = go (n `div` 2) y   (mulmp p x x) 
                   | otherwise = go ((n-1) `div` 2) (mulmp p y x) 
                                                    (mulmp p x x)

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
  -- Polynomial mod Integer
  -------------------------------------------------------------------------
  pmod :: Poly Integer -> Integer -> Poly Integer
  pmod (P cs) p = P [c `mmod` p | c <- cs]

  -------------------------------------------------------------------------
  -- Polynomial mod Polynomial
  -------------------------------------------------------------------------
  pmmod :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  pmmod p poly m = snd (divmp p poly m) 

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
  newton s n ds seq = sum ts
    where hs = getHeads seq ds
          ts = [h * (B.choose (n-s) k) | (h,k) <- zip hs [0..n]]

  -------------------------------------------------------------------------
  -- Get Heads
  -------------------------------------------------------------------------
  getHeads :: [Integer] -> [[Integer]] -> [Integer]
  getHeads seq ds = map head (seq:ds)

  -------------------------------------------------------------------------
  -- Newton Polynomial
  -------------------------------------------------------------------------
  newtonp :: [[Integer]] -> [Integer] -> Poly Rational
  newtonp ds seq = sump ts
    where hs = getHeads seq ds
          n  = fromIntegral $ dpredict ds
          ts = [bin2poly h k | (h,k) <- zip hs [0..n]]

  -------------------------------------------------------------------------
  -- Express binomials as polynomials formulas
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
  findCoeffs ds seq = L.M (go 0 seq)
    where d = fromIntegral (length ds)
          go _ []  = []
          go n (x:xs) | n > d     = []
                      | otherwise = genCoeff d n x : go (n+1) xs 

  genCoeff :: Integer -> Integer -> Integer -> [Integer]
  genCoeff m n x = go 0 x
    where go i x | i >  m    = [x]
                 | otherwise = n^i : go (i+1) x

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
  -- Numerical root finding
  -------------------------------------------------------------------------
  -- a and b must be of opposite signedness
  bisect :: (Num a, Eq a, Ord a, Fractional a, Show a) => 
            Poly a -> a -> a -> a -> a
  bisect p t a b | abs fc < abs t          = c
                 | signum fc == signum fa  = bisect p t c b 
                 | otherwise               = bisect p t a c 
    where fa = apply p a
          fb = apply p b
          fc = apply p c
          c  = (a+b)/2

  -- newton's method
  newguess :: (Num a, Eq a, Ord a, Enum a, Fractional a) =>
              Poly a -> a -> a -> a -> a
  newguess p t m a | abs pa < t = a
                   | m <= 0     = a
                   | otherwise  = newguess p t (m-1) (a-pa/p'a)
    where p'  = derivative (*) p
          pa  = apply p a
          p'a = apply p' a

  -------------------------------------------------------------------------
  -- Solving equations
  -------------------------------------------------------------------------
  solve :: Poly Double -> [Double]
  solve p = case degree p of
              0 -> coeffs p
              1 -> solvel p
              2 -> solve2 p
              3 -> solve3 p
              _ -> error "I don't know how to solve this polynomial"

  solvel :: (Num a,Fractional a) => Poly a -> [a]
  solvel (P [a,b]) = [-b/a]
  solvel _         = error "oops!"

  solve2 :: Poly Double -> [Double]
  solve2 (P [a,b,c]) | det < 0   = []
                     | otherwise = let d  = sqrt det
                                       x1 = (-b + d) / 2*a
                                       x2 = (-b - d) / 2*a
                        in if x1 /= x2 then [x1,x2] else [x1]
    where det = b^2 - 4*a*c
  solve2 _           = error "oops!"

  -- not correct...
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
    where mymin (x:xs) = go x xs
          go x [] = fst x
          go x (z:zs) | snd x < snd z = go x zs
                      | otherwise     = go z zs
    
