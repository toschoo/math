---------------------------------------------------------------------------
-- Polynomials
---------------------------------------------------------------------------
module Poly
where

  import Data.List (nub,foldl')
  import Debug.Trace (trace)

  import qualified Modular as M

  data Poly a = P [a]
    deriving (Eq,Show)

  coeffs :: Poly a -> [a]
  coeffs (P as) = as

  pretty :: (Num a, Show a, Eq a) => Poly a -> String
  pretty p = go (weigh p)
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

  apply :: Num a => Poly a -> a -> a
  apply (P []) _ = 0
  apply (P as) x = go x $ zip [0..] as
    where go z [] = 0
          go z ((i,c):cs) = c*z^i + go z cs

  weigh :: (Num a) => Poly a -> [(Integer,a)]
  weigh (P []) = []
  weigh (P as) = (zip [0..] as)

  degree :: Poly a -> Int
  degree (P as) = length as - 1

  -------------------------------------------------------------------------
  -- Addition
  -------------------------------------------------------------------------
  add :: Num a => Poly a -> Poly a -> Poly a
  add = strich (+)

  -------------------------------------------------------------------------
  -- Addition
  -------------------------------------------------------------------------
  sub :: Num a => Poly a -> Poly a -> Poly a
  sub = strich (-)

  -------------------------------------------------------------------------
  -- Generic Strichrechnung
  -------------------------------------------------------------------------
  strich :: Num a => (a -> a -> a) -> Poly a -> Poly a -> Poly a
  strich o (P x) (P y)     = P $ strichlist o x y

  -------------------------------------------------------------------------
  -- Generic Strichrechnung on lists of coefficients
  -------------------------------------------------------------------------
  strichlist :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
  strichlist o xs ys = go xs ys
    where go [] bs         = bs
          go as []         = as
          go (a:as) (b:bs) = a `o` b : go as bs

  -------------------------------------------------------------------------
  -- Folding a list of lists of coefficients using strichrechnung
  -------------------------------------------------------------------------
  strichf :: Num a => (a -> a -> a) -> [[a]] -> [a]
  strichf o = foldl' (strichlist o) []

  -------------------------------------------------------------------------
  -- Multiplication over an infinite field
  -------------------------------------------------------------------------
  mul :: (Show a, Num a) => Poly a -> Poly a -> Poly a
  mul p1 p2 | d2 > d1   =  mul p2 p1
            | otherwise =  P (strichf (+) ms)
    where d1 = degree p1
          d2 = degree p2
          ms = [mul1 (*) i (coeffs p1) p | (i,p) <- zip [0..] (coeffs p2)]

  -------------------------------------------------------------------------
  -- Multiplication over a finite field
  -------------------------------------------------------------------------
  mulmp :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  mulmp p p1 p2 | d2 > d1   =  mulmp p p2 p1
                | otherwise =  P [m `mmod` p | m <- strichf (+) ms]
    where d1 = degree p1
          d2 = degree p2
          ms = [mul1 o i (coeffs p1) p | (i,p) <- zip [0..] (coeffs p2)]
          o  = modmul p

  -------------------------------------------------------------------------
  -- Mapping (a*) on a list of coefficients
  -------------------------------------------------------------------------
  mul1 :: Num a => (a -> a -> a) -> Int -> [a] -> a -> [a]
  mul1 o i as a = zeros i ++ go as a
    where go [] _     = []
          go (c:cs) x = c `o` x : go cs x 

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
  -- Multiply a list of coefficients (infinite field)
  -------------------------------------------------------------------------
  mulist :: (Show a, Num a) => [a] -> [a] -> [a]
  mulist c1 c2 = coeffs $ mul (P c1) (P c2)

  -------------------------------------------------------------------------
  -- Multiply a list of coefficients (finite field)
  -------------------------------------------------------------------------
  mulmlist :: Integer -> [Integer] -> [Integer] -> [Integer]
  mulmlist p c1 c2 = coeffs $ mulmp p (P c1) (P c2)

  -------------------------------------------------------------------------
  -- Division (rational or real numbers)
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
  -- Division (over a modular field of integers)
  -------------------------------------------------------------------------
  divmp :: Integer -> 
           Poly Integer -> Poly Integer -> (Poly Integer,Poly Integer)
  divmp p (P as) (P bs) = let (q,r) = go [] as in (P q, P r)
    where go q r | degree (P r) < db  = (q,r)
                 | null r || r == [0] = (q,r)
                 | otherwise          = -- trace (show (q,r)) $
                     let t  = modiv p (last r) (last bs)
                         d  = degree (P r) - db
                         ts = zeros d ++ [t]
                         m  = mulmlist p ts bs
                      in go [m `mmod` p | m <- cleanz $ strichlist (+) q ts]
                            [m `mmod` p | m <- cleanz $ strichlist (-) r m ]
          db = degree (P bs)

  -------------------------------------------------------------------------
  -- Integer Division mod p
  -------------------------------------------------------------------------
  modiv :: Integer -> Integer -> Integer -> Integer
  modiv p n d = modmul p n d'
    where d' = M.inverse d p

  mmod :: Integer -> Integer -> Integer
  mmod n p | n < 0 && (-n) > p = mmod (-(mmod (-n)) p) p
           | n < 0             = mmod (p + n) p
           | otherwise         = n `rem` p

  -------------------------------------------------------------------------
  -- Multiplication mod p
  -------------------------------------------------------------------------
  modmul :: Integer -> Integer -> Integer -> Integer
  modmul p f1 f2 = (f1 * f2) `mmod` p

  -------------------------------------------------------------------------
  -- Make polynomial mod p
  -------------------------------------------------------------------------
  modp :: Integer -> Poly Integer -> Poly Integer
  modp p (P as) = P (cleanz [a `mmod` p | a <- as])

  -------------------------------------------------------------------------
  -- Divides
  -------------------------------------------------------------------------
  divides :: (Show a, Num a, Eq a, Fractional a, Ord a) => 
          Poly a -> Poly a -> Bool
  divides a b = case divp b a of
                  (_,P [0]) -> True
                  _         -> False

  -------------------------------------------------------------------------
  -- GCD (infinite field)
  -------------------------------------------------------------------------
  gcdp :: (Show a, Num a, Eq a, Fractional a, Ord a) => 
          Poly a -> Poly a -> Poly a
  gcdp a b | degree b > degree a = gcdp b a
           | nullp b = a
           | otherwise = let (_,r) = divp a b in gcdp b r

  -------------------------------------------------------------------------
  -- GCD (finite field)
  -------------------------------------------------------------------------
  gcdmp :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  gcdmp p a b | degree b > degree a = gcdmp p b a
              | nullp b = a
              | otherwise = let (_,r) = divmp p a b in trace (show r) $ gcdmp p b r
 
  -------------------------------------------------------------------------
  -- Null
  -------------------------------------------------------------------------
  nullp :: (Num a, Eq a) => Poly a -> Bool
  nullp (P [0]) = True
  nullp _       = False

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
  -- Derivatives
  -------------------------------------------------------------------------
  derivative :: (Num a, Enum a) => (a -> a -> a) -> Poly a -> Poly a
  derivative o (P as) = P (go $ zip [1..] (drop 1 as))
    where go []         = []
          go ((x,c):cs) = (x `o` c) : go cs

  
  -- factor

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
    
