{-# Language BangPatterns #-}
---------------------------------------------------------------------------
-- Polynomials
---------------------------------------------------------------------------
module Poly
where

  import           Data.Function (on)
  import           Data.Maybe (fromJust)
  import           Data.List (nub,foldl',
                              sort,sortBy,groupBy,sortOn,
                              intersect,(\\),findIndex)
  import           Data.Ratio
  import           Control.Applicative ((<$>))
  import           Control.Concurrent
  import           Control.Monad (when,void,filterM,forM_,forM)
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
  -- Leading coefficient
  -------------------------------------------------------------------------
  lc :: Poly a -> a
  lc = last . coeffs

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

  rational :: Poly Integer -> Poly Rational
  rational (P cs) = P [c%1 | c <- cs]

  ---------------------------------------------------------------------------
  -- Content of a polynomial
  ---------------------------------------------------------------------------
  content :: Poly Integer -> Integer
  content (P cs) = M.mgcd cs

  ---------------------------------------------------------------------------
  -- Primitive part of a polynomial
  ---------------------------------------------------------------------------
  primitive :: Poly Integer -> Poly Integer
  primitive p@(P cs) = P (map (`div` c) cs)
    where c = content p

  ---------------------------------------------------------------------------
  -- Norm
  ---------------------------------------------------------------------------
  norm :: Poly Integer -> Integer
  norm p = sum [abs c | c <- coeffs p]

  ---------------------------------------------------------------------------
  -- Height of a polynomial over the integers (Cantor)
  ---------------------------------------------------------------------------
  height :: Poly Integer -> Integer
  height p@(P cs) = n-1+csum
    where n = fromIntegral $ degree p
          csum = sum (map abs cs)

  ---------------------------------------------------------------------------
  -- Generate monic polynomials over the integers (up to max coeff)
  ---------------------------------------------------------------------------
  polygen :: Int -> Integer -> [Poly Integer]
  polygen n m | n < 2 = []
              | otherwise = map poly $ go 2
     where go i | i == n = coefgen i m
                | otherwise = coefgen i m ++ go (i+1)

  ---------------------------------------------------------------------------
  -- Generate all coefficients up to m
  ---------------------------------------------------------------------------
  coefgen :: Int -> Integer -> [[Integer]]
  coefgen i m = filter noconst $ 
                       coefcountup m ((take (i-1) $ 
                             repeat (-m)) ++ [1])
    where noconst p = last p /= 0

  ---------------------------------------------------------------------------
  -- Count coefficients up to m
  ---------------------------------------------------------------------------
  coefcountup :: Integer -> [Integer] -> [[Integer]]
  coefcountup m cs | cs == ms  = [cs]
                   | otherwise = cs : (coefcountup m (coefinc m cs)) 
    where ms = take (length cs) (repeat m)

  ---------------------------------------------------------------------------
  -- Count coefficients up to m
  ---------------------------------------------------------------------------
  coefcountdown :: Integer -> [Integer] -> [[Integer]]
  coefcountdown m cs | cs == ms  = [cs]
                     | otherwise = cs : (coefcountdown m (coefdec m cs)) 
    where ms = take (length cs) (repeat m)

  ---------------------------------------------------------------------------
  -- Increment coefficients by 1
  ---------------------------------------------------------------------------
  coefinc :: Integer -> [Integer] -> [Integer]
  coefinc m [] = []
  coefinc m (c:cs) | c == m = 0:(coefinc m cs)
                   | otherwise = (c+1):cs

  ---------------------------------------------------------------------------
  -- Decrement coefficients by 1
  ---------------------------------------------------------------------------
  coefdec :: Integer -> [Integer] -> [Integer]
  coefdec m [] = []
  coefdec m (c:cs) | c == m = 0:(coefdec m cs)
                   | otherwise = (c-1):cs

  ---------------------------------------------------------------------------
  -- Filter irreducible polynomials (using kronecker)
  ---------------------------------------------------------------------------
  pirre :: [Poly Integer] -> [Poly Integer]
  pirre ps = filter (null . kron) ps
    where kron p = kronecker p (rs p)
          rs p = map (apply p) (samples p)
          samples p = [0..fromIntegral(degree p)]::[Integer]

  ---------------------------------------------------------------------------
  -- Filter irreducible polynomials (using cantor-zassenhaus)
  ---------------------------------------------------------------------------
  zirre :: [Poly Integer] -> IO [Poly Integer]
  zirre = filterM irre

  ---------------------------------------------------------------------------
  -- check irreducible polynomials (using cantor-zassenhaus)
  ---------------------------------------------------------------------------
  irre :: Poly Integer -> IO Bool
  irre q@(P cs) = do
    let m = maximum (map abs cs)
    let p = last (takeWhile (<4*m) P.allprimes)
    f <- cantorzassenhaus p q
    let ks = map (rational . snd) f
    let r = rational q
    let x = [k | k <- ks, snd (r `divp` k) == P [0]]
    return (null x)

  ---------------------------------------------------------------------------
  -- Eisenstein
  ---------------------------------------------------------------------------
  eisen :: Poly Integer -> Bool
  eisen p@(P cs) = go pp
    where pp = take 100 P.allprimes
          go [] = False
          go (p:ps) | c1 p && c2 p && c3 p = True
                    | otherwise            = go ps
          c1 p = all (\x -> x `mod` p == 0) $ init cs
          c2 p = (last cs) `mod` p /= 0
          c3 p = (head cs) `mod` (p^2) /= 0

  ---------------------------------------------------------------------------
  -- Sort by height
  ---------------------------------------------------------------------------
  hsort :: [Poly Integer] -> [Poly Integer]
  hsort = sortBy h
   where h a b = compare (height a) (height b)
  
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
  strichlist o xs ys =
          let us | xd >= yd = xs
                 | otherwise = xs ++ zeros (yd-xd)
              vs | yd >= yd = ys 
                 | otherwise = ys ++ zeros (xd-yd)
           in cleanz (go us vs)
    where xd               = length xs
          yd               = length ys
          go [] bs         = bs
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
  mul1 o i cs a = zeros i ++ [c `o` a | c <- cs]

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
  -- Euclidean Division (infinite field)
  -------------------------------------------------------------------------
  divpe :: (Show a, Num a, Eq a, Integral a, Ord a) => 
            Poly a -> Poly a -> (Poly a,Poly a)
  divpe a b = let (q,r) = go [] (coeffs a') in (P q, P r)
    where a' = scale (lc b) a
          bs = coeffs b
          db = degree b
          go q r | degree (P r) < db  = (q,r)
                 | null r || r == [0] = (q,r)
                 | otherwise          = -- trace (show (q,r)) $
                     let t  = last r `div` last bs
                         d  = degree (P r) - db
                         ts = zeros d ++ [t]
                         m  = mulist ts bs
                         u  = cleanz $ strichlist (+) q ts
                         v  = cleanz $ strichlist (-) r m
                      in go [x*(last v) | x <- u]
                            [x*(last u) | x <- v]

  divpe2 :: (Show a, Num a, Eq a, Integral a, Ord a) => 
            Poly a -> Poly a -> (Poly a,Poly a, a)
  divpe2 a b | d <  0 = (P[0], a, 1)
            | otherwise = trace (show t) $
                          let (q,r,c) = divpe2 (scale lb a) (mul t b)
                           in (add (scale c t) q, r, c*la)
    where d = degree a - degree b
          lb = lc b
          la = lc a
          t  = P (zeros d ++ [lb])

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
  -- GCD (infinite field, integal)
  -------------------------------------------------------------------------
  gcdpe :: (Show a, Num a, Eq a, Integral a, Ord a) => 
            Poly a -> Poly a -> Poly a
  gcdpe a b | degree b > degree a = gcdpe b a
            | zerop b = a
            | otherwise = let (_,r) = divpe a b in gcdpe b r

  -------------------------------------------------------------------------
  -- XGCD
  -------------------------------------------------------------------------
  xgcdp :: (Show a, Num a, Eq a, Fractional a, Ord a) => 
           Poly a -> Poly a -> 
           (Poly a, (Poly a, Poly a))
  xgcdp a b = go a b (P [1]) (P [0]) (P [0]) (P [1])
    where go c d uc vc ud vd | zerop c    = (d, (ud, vd))
                             | otherwise  = 
                               let (q, r) = divp d c 
                                in go r c  (sub ud (mul q uc))
                                           (sub vd (mul q vc)) uc vc
 
  -------------------------------------------------------------------------
  -- GCD (mod p)
  -------------------------------------------------------------------------
  gcdmp :: Integer -> Poly Integer -> Poly Integer -> Poly Integer
  gcdmp p a b | degree b > degree a = gcdmp p b a
              | zerop b = a
              | otherwise = let (_,r) = divmp p a b in gcdmp p b r

  -------------------------------------------------------------------------
  -- XGCD (mod p)
  -------------------------------------------------------------------------
  _xgcdmp :: Integer -> Poly Integer -> Poly Integer -> 
           (Poly Integer, (Poly Integer, Poly Integer))
  _xgcdmp p a b = go a b (P [1]) (P [0]) (P [0]) (P [1])
    where go c d uc vc ud vd | zerop c    = (d, (ud, vd))
                             | otherwise  = 
                               let (q, r) = divmp p d c 
                                in go r c
                                   (subp p ud (mulmp p q uc))
                                   (subp p vd (mulmp p q vc)) uc vc

  xgcdmp :: Integer -> Poly Integer -> Poly Integer -> 
            (Poly Integer, (Poly Integer, Poly Integer))
  xgcdmp p a b | zerop a || zerop b = (P[1],(P[1],P[1]))
               | otherwise = 
                    let la = lc a
                        lb = lc b
                        a' | la == 1 = a
                           | otherwise = monicp p a
                        b' | lb == 1 = b
                           | otherwise = monicp p b
                        uc = P [M.inverse la p]
                        vc = P [0]
                        ud = P [0]
                        vd = P [M.inverse lb p]
                     in go a' b' uc vc ud vd
    where go c d uc vc ud vd | zerop c    = (d, (ud, vd))
                             | otherwise  = 
                               let (q, r) = divmp p d c
                                   r'     = monicp p r
                                   l      = M.inverse (lc r) p
                                   s      = subp p ud (mulmp p q uc)
                                   t      = subp p vd (mulmp p q vc)
                                in if zerop r then (c, (uc, vc))
                                   else go r' c
                                           (modp p (scale l s))
                                           (modp p (scale l t)) uc vc
 
  -------------------------------------------------------------------------
  -- GCD (mod p) over list
  -------------------------------------------------------------------------
  mgcdmp :: Integer -> [Poly Integer] -> Poly Integer
  mgcdmp _ [] = P [1]
  mgcdmp _ [a] = a
  mgcdmp p (a:as) = foldl' (gcdmp p) a as

  -------------------------------------------------------------------------
  -- XGCD (mod p) over list
  -------------------------------------------------------------------------
  mxgcdmp :: Integer -> [Poly Integer] -> (Poly Integer, [Poly Integer])
  mxgcdmp p [] = (P[0],[])
  mxgcdmp p [x] = (x,[P[1]])
  mxgcdmp p (a:as) = let (g, rs) = go [] a as
                    in (g, reverse $ ks rs)
    where go rs i [j] = let (g, (x,y)) = xgcdmp p i j
                         in (g, [y,x] ++ rs)
          go rs i is = let (g, (x,y)) = xgcdmp p i (head is)
                        in go ([y,x]++rs) g (tail is)
          ks = M.distr (mulmp p) (P[1])

  -------------------------------------------------------------------------
  -- Test mxgcdmp
  -------------------------------------------------------------------------
  tstmxgcdmp :: IO ()
  tstmxgcdmp = do
    p <- randomPrime 4
    -- test empty list
    when (mxgcdmp p [] /= (P[0],[])) $ fail "empty list failed"
    a <- (monicp p) <$> randomPoly p 5

    -- test singleton
    when (mxgcdmp p [a] /= (a,[P[1]])) $ fail "singleton failed"
    b <- (monicp p) <$> randomPoly p 4

    -- test pair
    let (g,[x,y]) = mxgcdmp p [a,b]
    when (g /= monicp p( gcdmp p a b)) $ fail ("gcd in tuple failed: " ++ show g ++ " | " ++ show (gcdmp p a b))
    let r = addp p (mulmp p x a) (mulmp p y b)
    when (r /= g) (do
         -- putStrLn ("p " ++ show p ++ ": " ++ show a ++ ", " ++ show b ++ " -> " ++ show g ++ "(" ++ show x ++ ", " ++ show y ++ ")")
         -- putStrLn ("g: " ++ show g ++ " | " ++ show r)
         fail "formula in tuple failed")

    -- hundred times!
    forM_ [1..100] (\i -> do
      putStrLn ("running Test " ++ show i)
      s <- randomRIO(3,100) -- size
      l <- randomList p s
      let (g, rs) = mxgcdmp p l
      when (monicp p (mgcdmp p l) /= monicp p g) $ fail "gcd in list failed"
      let t = modp p $ sump [mulmp p x a | (x,a) <- zip rs l]
      when (t /= g) $ fail "formula in tuple failed")

    putStrLn "PASSED!"

    where randomList :: Integer -> Integer -> IO [Poly Integer]
          randomList p s | s == 0 = return []
                         | otherwise = do
                           d <- randomRIO (5,10)
                           x <- (monicp p) <$> randomPoly p d
                           l <- randomList p (s-1)
                           return (x:l)

  -------------------------------------------------------------------------
  -- Pseudo-remainder
  -------------------------------------------------------------------------
  prem :: Poly Integer -> Poly Integer -> Poly Integer
  prem a b = poly $ map numerator (coeffs $ snd (divp x y))
    where l = lc b
          k = l^(da-db+1)
          da = fromIntegral $ degree a
          db = fromIntegral $ degree b
          a' = scale k a
          x  = P (map (%1) $ coeffs a')
          y  = P (map (%1) $ coeffs b)

  -------------------------------------------------------------------------
  -- Pseudo-remainder sequence (general)
  -------------------------------------------------------------------------
  pgcd :: (Poly Integer -> Integer) ->
          Poly Integer -> Poly Integer -> [Poly Integer]
  pgcd alpha p q | zerop q = [p]
                 | otherwise = -- trace (show p ++ "|" ++ show q ++ "-->" ++ show r ++ "\n") $
                   let r = prem p q
                       a = alpha r
                       x = P [numerator (c%a) | c <- coeffs r]
                    in if zerop r then [] else x : pgcd alpha q x

  -------------------------------------------------------------------------
  -- Trivial Pseudo-remainder sequence
  -------------------------------------------------------------------------
  tpgcd :: Poly Integer -> Poly Integer -> [Poly Integer]
  tpgcd = pgcd one where one _ = 1

  -------------------------------------------------------------------------
  -- Primitive Pseudo-remainder sequence
  -------------------------------------------------------------------------
  ppgcd :: Poly Integer -> Poly Integer -> [Poly Integer]
  ppgcd = pgcd content

  -------------------------------------------------------------------------
  -- Subresultant Pseudo-remainder sequence
  -- see also: http://mathworld.wolfram.com/Resultant.html
  -- and https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Subresultants
  -- and https://math.stackexchange.com/questions/696335/
  --             how-does-one-compute-a-resultant-by-using-euclidean-algorithm/696515
  -- follows mainly: https://docs.sympy.org/1.0/_modules/sympy/polys/euclidtools.html
  -- Test case from wikipedia:
  -- a = P [-5,2,8,-3,-3,0,1,0,1]
  -- b = P [21,-9,-4,0,5,0,3]
  -- expected: [[9,0,-3,0,15], [245,125,65], [12300,9326], [260708]]
  -------------------------------------------------------------------------
  spgcd :: Poly Integer -> Poly Integer -> [Poly Integer]
  spgcd a b = let n = degree a
                  m = degree b
                  d = n - m
                  c = scale ((-1)^(d+1)) (prem a b)
                  l = lc b
                  z = -(l^d)
                  k = degree c
               in c:go z l b c k (m-k)
    where go c l f g n d | degree(g) == 0 = []
                         | otherwise = let y = (-l) * c^d
                                           h = P [x `div` y | x <- coeffs (prem f g)]
                                           m = degree h
                                           l' = lc g
                                           c' | d > 1 = (-l')^d `div` (c^(d-1))
                                              | otherwise = -l'
                                        in h:go c' l' g h m (n-m)

  -------------------------------------------------------------------------
  -- The resultant of two polynomials
  -- (which is the last in the subresultant sequence)
  -------------------------------------------------------------------------
  res :: Poly Integer -> Poly Integer -> Integer
  res a b = head $ coeffs $ last $ spgcd a b

  -------------------------------------------------------------------------
  -- The generalised discriminant
  -- computed as the resultant^2 * lc^(d(d-1)/2) 
  -------------------------------------------------------------------------
  dis :: Poly Integer -> Integer
  dis p = (-1)^x * (res p p') `div` l
    where x = d*(d-1) `div` 2
          d = degree p
          p' = derivative (*) p
          l  = lc p

  -------------------------------------------------------------------------
  -- Compute the discriminant from the roots
  -------------------------------------------------------------------------
  disroots :: [Integer] -> Integer
  disroots [] = -1
  disroots [r] = 0
  disroots (r:rs) = undefined

  -------------------------------------------------------------------------
  -- Test the discriminant for degree 2:
  -- compare dis2 (b^2 - 4ac), dis and
  -- the squared product of the roots times lc^2
  -------------------------------------------------------------------------
  tstDis2 :: IO ()
  tstDis2 = do
     let m = 100
     t <- forM [1..m] (\i -> do
          p <- randomPoly 9 3
          let d1 = dis2 p
          let d2 = dis  p
          let rs = solve2 (P [fromIntegral x | x <- coeffs p])
          let l  = fromIntegral (lc p)
          let r | null rs = -1
                | length rs == 1 = head rs
                | otherwise = l^2 * (head rs - last rs)^2
          let x | r <= 0 = floor r
                | otherwise = if floor r >= d1 then floor r else ceiling r
          putStrLn(show i ++ ") " ++ show p ++ ": " ++ show x ++ " = " ++ show d1 ++ " = " ++ show d2)
          return ((x <= 0 || x == d1) && d1 == d2))
     let cnt = length (filter (\x -> x) t)
     putStrLn ("true: " ++ show cnt ++ " | false: " ++ show (m - cnt))
     if and t then putStrLn "PASSED" else putStrLn "FAILED"

  -------------------------------------------------------------------------
  -- Slyvester Matrix of two polynomials
  -------------------------------------------------------------------------
  sylvester :: (Num a) => Poly a -> Poly a -> L.Matrix a
  sylvester a b = L.M (go 0 xs ys)
    where la = degree a
          lb = degree b
          ll = la + lb
          xs = (reverse $ coeffs a) ++ zeros (lb-1)
          ys = (reverse $ coeffs b) ++ zeros (la-1)
          go _ [] [] = []
          go i l1 l2 | i == ll   = []
                     | i >= lb   = l2:go (i+1) [] (0:init l2) 
                     | otherwise = l1:go (i+1) (0:init l1) l2

  -------------------------------------------------------------------------
  -- Show the slyvester matrix of a and b (test case for wikipedia)
  -------------------------------------------------------------------------
  sylvestershow :: IO ()
  sylvestershow = let s = sylvester a b
                   in do putStrLn ("Matrix with " ++ show (length $ L.rows s) ++ 
                                              "X" ++ show (length (head $ L.rows s)))
                         putStrLn (show $ sylvester a b)
    where a = P [-5,2,8,-3,-3,0,1,0,1]
          b = P [21,-9,-4,0,5,0,3]

  -------------------------------------------------------------------------
  -- Null
  -------------------------------------------------------------------------
  zerop :: (Num a, Eq a) => Poly a -> Bool
  zerop (P [0]) = True
  zerop _       = False

  -------------------------------------------------------------------------
  -- unity
  -------------------------------------------------------------------------
  unityp :: Poly Integer -> Bool
  unityp (P [1]) = True
  unityp _       = False

  -------------------------------------------------------------------------
  -- unity (finite field)
  -------------------------------------------------------------------------
  unitymp :: Integer -> Poly Integer -> Bool
  unitymp _ (P [1]) = True
  unitymp p (P [x]) = x `mmod` p `elem` [1,p-1]
  unitymp _ _       = False      

  -------------------------------------------------------------------------
  -- Derivatives (generic)
  -------------------------------------------------------------------------
  derivative :: (Eq a, Num a, Enum a) => (a -> a -> a) -> Poly a -> Poly a
  derivative o (P as) = P (cleanz (map op $ zip [1..] (drop 1 as)))
    where op (x,c) = x `o` c

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
  -- Get squarefree part (infinite field)
  -------------------------------------------------------------------------
  squarefreepart :: Poly Integer -> Poly Integer
  squarefreepart f | degree g == 0 = f
                   | otherwise = let (q,_) = divpe a g
                                  in primitive q
    where l = lc f
          a | l < 0 = P [(-c) | c <- coeffs f]
            | otherwise = f
          g = gcdpe a (derivative (*) a)
          
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
  -- where the length of the list determines the degree of the factor,
  --  where degree = length - 1
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
    where ds = pdivs i

  pdivs :: Integer -> [Integer]
  pdivs i | i < 0 = map negate $ pdivs (-i)
          | otherwise = [d | d <- [1..i], rem i d == 0] 

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
  -- Produce a random squarefree polynomial (modulo p) 
  -------------------------------------------------------------------------
  sqRandomPoly :: Integer -> Int -> IO (Poly Integer)
  sqRandomPoly p d = do
    r <- randomPoly p d
    if squarefree p r then return r else msRandomPoly p d

  -------------------------------------------------------------------------
  -- Produce n distinct random squarefree polynomials (modulo p) 
  -------------------------------------------------------------------------
  nSqRandomPolies :: Int -> Integer -> Int -> IO [Poly Integer]
  nSqRandomPolies n p d = go [] n p d
    where go rs n p d | n == 0    = return rs
                      | otherwise = do
                          x <- sqRandomPoly p d
                          case findIndex (==x) rs of
                            Nothing -> go (x:rs) (n-1) p d
                            Just _  -> go rs n p d

  -------------------------------------------------------------------------
  -- Produce n distinct random monic squarefree polynomials (modulo p) 
  -------------------------------------------------------------------------
  nMsRandomPolies :: Int -> Integer -> Int -> IO [Poly Integer]
  nMsRandomPolies n p d = go [] n p d
    where go rs n p d | n == 0    = return rs
                      | otherwise = do
                          x <- msRandomPoly p d
                          case findIndex (==x) rs of
                            Nothing -> go (x:rs) (n-1) p d
                            Just _  -> go rs n p d

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
  -- remove divisors
  -------------------------------------------------------------------------
  rmdivs :: Integer -> Poly Integer -> Poly Integer
  rmdivs p (P as) = P (cleanz [div20 a | a <- as])
    where ds = divs p \\ [1,-1,p,-p,p-1]
          div20 k | k `elem` ds = 0
                  | otherwise   = k 

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
  predict ds [] = Nothing
  predict ds xs = case go (reverse ds) of
                    0  -> Nothing
                    d  -> Just (d + (last xs))
    where go = foldl' (\x c -> last c + x) 0

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
  findCoeffs ds sq = L.M [genCoeff d n x | (n,x) <- zip [0..d] sq]
    where d = fromIntegral (length ds)

  genCoeff :: Integer -> Integer -> Integer -> [Integer]
  genCoeff m n x = map (n^) [0..m] ++ [x]

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
  taylor :: (Show a, Fractional a, Real a, Enum a) => 
            Integer -> a -> Poly a -> Poly a
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
  -- Bound for Hensel lifting
  -------------------------------------------------------------------------
  hbound :: (Num a) => Poly a -> a
  hbound p@(P cs) = 2^d * h * b
    where h = sum (map abs $ init cs)
          d = degree p
          b = abs (last cs)

  -------------------------------------------------------------------------
  -- compute prime power > 2*m0
  -------------------------------------------------------------------------
  pbound :: (Num a, Integral a) => a -> a -> a
  pbound p b = go p 1
    where go q k | q > b = q
                 | otherwise = go (q^2) (k+1)

  -------------------------------------------------------------------------
  -- compute wang bound
  -------------------------------------------------------------------------
  wbound :: (Num a, Integral a) => a -> a -> a -> a
  wbound p b r = ceiling (m**(1/q))
     where m = fromIntegral (pbound p b)
           q = fromIntegral r

  truncmp :: Integer -> Poly Integer -> Poly Integer
  truncmp p f = poly (map trunc $ coeffs f)
    where trunc c = let r = c `mod` p
                     in if r > p `div` 2 then r - p else r

  -------------------------------------------------------------------------
  -- one step of hensel lifting
  -- computes g',h',s',t', such that
  --      f = g'*h' (mod p^2) and s'*g' + t'*h' = 1 (mod p^2)
  -- given g, h, s, t fulfilling the equations mod p.
  -- https://docs.sympy.org/0.7.3/_modules/sympy/polys/factortools.html
  -------------------------------------------------------------------------
  hstep :: Integer -> Poly Integer -> Poly Integer -> Poly Integer ->
                      Poly Integer -> Poly Integer ->
                     (Poly Integer, Poly Integer,
                      Poly Integer, Poly Integer) 
  hstep p f g h s t = (g', h', s', t')
    where m     = p^2
          e     = truncmp m (sub f (mul g h))               -- f - gh    (mod m) 
          (q_,r_) = divmp m (mul s e) h              -- se / h    (mod m)
          q     = truncmp m q_
          r     = truncmp m r_
          u     = add (mul t e) (mul q g)   -- te + qg   (mod m)
          g'    = truncmp m (add g u)                           -- g + u     (mod m)
          h'    = truncmp m (add h r)                           -- h + r     (mod m)
          v     = add (mul s g') (mul t h') -- sg' + th' (mod m)
          b     = truncmp m (sub v (P [1]))                     -- v - 1     (mod m)
          (c_,d_) = divmp m (mul s b) h'             -- sb / h'   (mod m)
          c     = truncmp m c_
          d     = truncmp m d_
          w     = truncmp m (add (mul t b) (mul c g'))  -- tb + cg'  (mod m)
          s'    = truncmp m (sub s d)                           -- s - d     (mod m)
          t'    = truncmp m (sub t w)                           -- t - w     (mod m)

  hstep2 :: Integer -> Poly Integer -> Poly Integer -> Poly Integer ->
                       Poly Integer -> Poly Integer ->
                      (Poly Integer, Poly Integer,
                       Poly Integer, Poly Integer) 
  hstep2 p f g h s t = (g', h', s', t')
    where m     = p^2
          e     = subp m f (mulmp m g h)               -- f - gh    (mod m) 
          (q,r) = divmp m (mulmp m s e) h              -- se / h    (mod m)
          u     = addp m (mulmp m t e) (mulmp m q g)   -- te + qg   (mod m)
          g'    = addp m g u                           -- g + u     (mod m)
          h'    = addp m h r                           -- h + r     (mod m)
          v     = addp m (mulmp m s g') (mulmp m t h') -- sg' + th' (mod m)
          b     = subp m v (P [1])                     -- v - 1     (mod m)
          (c,d) = divmp m (mulmp m s b) h'             -- sb / h'   (mod m)
          w     = addp m (mulmp m t b) (mulmp m c g')  -- tb + cg'  (mod m)
          s'    = subp m s d                           -- s - d     (mod m)
          t'    = subp m t w                           -- t - w     (mod m)

  -------------------------------------------------------------------------
  -- hensel lifting
  -- https://docs.sympy.org/0.7.3/_modules/sympy/polys/factortools.html
  -------------------------------------------------------------------------
  hlift2 :: Integer -> Integer -> Poly Integer -> [Poly Integer] -> [Poly Integer]
  hlift2 p x f fs | r == 1    = [modp (p^x) $ scale (M.inverse l  (p^x)) f] -- base case: factor scaled by inverse of lc
                 | otherwise = let (g', h', _, _) = go d p g h s t       -- hlift
                                in hlift p x g' (take k fs) ++
                                   hlift p x h' (drop k fs)
    where r = length fs
          l = lc f -- mod p?
          k = r `div` 2
          d = floor (logBase 2 (fromIntegral x)) + 1
          g  | r == 0 = modp p (P[l])
             | otherwise = prodp (mulmp p) (P[l]:(take k fs)) -- [mulmp p (modp p (P[l])) a | a <- take k fs] -- product (lc * fs[i])
          h  | k+1 >= r = fs!!k
             | otherwise = prodp (mulmp p) (drop k fs) -- product (fs[k] * fs[k+i])
          (_,(s,t)) = xgcdmp p g h
          go n m g h s t | n == 0    = (g,h,s,t)
                         | otherwise = let (g',h',s',t') =
                                             {- trace ("G: " ++ show g ++ ", H: " ++ show h ++ ", S: " ++ show s ++ ", T: " ++ show t) $ -}
                                             hstep m f g h s t
                                        in go (n-1) (m^2) g' h' s' t'

  hlift :: Integer -> Integer -> Poly Integer -> [Poly Integer] -> [Poly Integer]
  hlift p x f fs | r == 1    = [modp (p^x) $ scale (M.inverse l  (p^x)) f] -- base case: factor scaled by inverse of lc
                 | otherwise = let (g', h', _, _) = go d p g h s t       -- hlift
                                in hlift p x g' (take k fs) ++
                                   hlift p x h' (drop k fs)
    where r = length fs
          l = lc f -- mod p?
          k = r `div` 2
          d = floor (logBase 2 (fromIntegral x)) + 1
          g  | r == 0 = truncmp p (P[l])
             | otherwise = prodp mul $ map (truncmp p) (P[l]:(take k fs)) -- [mulmp p (modp p (P[l])) a | a <- take k fs] -- product (lc * fs[i])
          h  | k+1 >= r = truncmp p (fs!!k)
             | otherwise = prodp mul (map (truncmp p) $ drop k fs) -- product (fs[k] * fs[k+i])
          (_,(s_,t_)) = xgcdmp p g h
          s = truncmp p s_
          t = truncmp p t_
          go n m g h s t | n == 0    = (g,h,s,t)
                         | otherwise = let (g',h',s',t') =
                                             {- trace ("G: " ++ show g ++ ", H: " ++ show h ++ ", S: " ++ show s ++ ", T: " ++ show t) $ -}
                                             hstep m f g h s t
                                        in go (n-1) (m^2) g' h' s' t'

  -------------------------------------------------------------------------
  -- find primes for zassenhaus
  -------------------------------------------------------------------------
  findzps :: Integer -> Integer -> Poly Integer -> [Integer]
  findzps q b f = take 5 $ filter zp (takeWhile (<=b) (dropWhile (<=q) P.allprimes))
    where zp p = (l `mod` p /= 0) && (squarefree p f)
          l    = lc f

  -------------------------------------------------------------------------
  -- filter factors for zassenhaus
  -- this is not so easy!!!
  -------------------------------------------------------------------------
  filterzfs :: Integer -> Poly Integer -> [Poly Integer] -> [Poly Integer]
  filterzfs pl f fs = let ps = tail (Perm.ps fs) -- we use the powerset! not working in practice!
                       in {- trace ("ps: " ++ show ps) $ -} go ps
    where l = lc f
          lp = P[l]
          go [] = []
          go (k:ks) | prodp mul (filter (not . zerop) k) == f = k -- do we need lc ?
                    | prodp mul (filter (not . zerop) (lp:k)) == f = lp:k -- do we need lc ?
                    | otherwise = {- trace ("k: " ++ show ((lp:k)) ++ ", " ++  show (prodp mul (filter (not . zerop) (lp:k)))) $ -} go ks

  extractfs :: Integer -> Integer -> Poly Integer -> [Poly Integer] -> [Poly Integer]
  extractfs pl bb f fs = go (length fs `div` 2) (tail $ Perm.ps [0..s])
    where fl = lc f
          s  = (length fs) - 1
          qtest q = let p | q > pl `div` 2 = q - pl
                          | otherwise = q
                     in if p == 0 then False else (fl `mod` q) /= 0
          go :: Int -> [[Int]] -> [Poly Integer]
          go k is | k < 0 = [] -- (length fs) `div` 2 = []
                  | otherwise = let (ms,r) = ffac f (filter (\i -> k==length i) is)
                                 in if null r then go (k-1) is else r++go k (is \\ ms)
          ffac :: Poly Integer -> [[Int]] -> ([[Int]], [Poly Integer])
          ffac _ [] = ([],[])
          ffac x (p:ps) = -- trace ("ffac: " ++ show x ++ ", " ++ show p) $
                          let l = lc x
                              fs' = [fs!!i | i <- p]
                              fp' = [fs!!i | i <- ([0..s] \\ p)] 
                              g = truncmp pl (mul (poly [l]) (prodp mul fs'))
                              tmp = (mul (poly [l]) (prodp mul fp'))
                              h = -- trace("h untrunked: " ++ show tmp ++ "(" ++ show pl ++ ")") $
                                  truncmp pl (mul (poly [l]) (prodp mul fp'))
                              q | l == 1    = product (map lc fs) `mod` pl
                                | otherwise = lc (primitive g)
                              t | l == 1    = qtest q
                                | otherwise = q /= 0 && fl `mod` q /= 0
                           in -- trace("g,h: " ++ show g ++ ", " ++ show h) $ 
                             if t || (norm g) * (norm h) > bb
                              then ffac x ps 
                              else let (ps',rs) = ffac (primitive h) ps
                                    in (p:ps', (primitive g):rs)
                                    
  -------------------------------------------------------------------------
  -- zassenhaus algorithm for factoring squarefree polynomials
  -- https://docs.sympy.org/0.7.3/_modules/sympy/polys/factortools.html
  -------------------------------------------------------------------------
  zassenhaus :: Poly Integer -> IO [Poly Integer]
  zassenhaus f | n == 1 = return [f]
               | otherwise = do
                    (p,fs) <- bpfactors 2
                    -- putStrLn ("smallest set of factors: " ++ show fs)
                    if null fs then return [f] else 
                      let x = ceiling (logBase (fromIntegral p) (fromIntegral (2*bb+1)))
                          pl = trace("BOUNDS: " ++ show aa ++ ", " ++ show bb ++ ", " ++ show cc ++ ", " ++ show gma ++ ", " ++ show pb ++ ", " ++ show x) p^x
                          g = sortOn degree (nub (hlift p x f fs)) -- $ map (modp p) fs)) -- hensel lifting
                          h = map primitive g
                          -- rs = filterzfs (p^l) f h
                          rs = extractfs (pl) bb f h
                       in do
                         -- putStrLn ("g : " ++ show g)
                         -- putStrLn ("h : " ++ show h)
                         -- putStrLn ("rs: " ++ show rs)
                         if null rs then return [f] else return rs
    where n  = fromIntegral (degree f)
          l  = lc f                            -- leading coefficient
          aa = maximum (map abs $ coeffs f)    -- greatest coefficient
          bb = abs((2^n*aa*l)*floor(sqrt(fromIntegral(n+1))))              -- bound for base
          cc = (n+1)^(2*n)*aa^(2*n-1)
          gma = ceiling(2*(logBase 2 (fromInteger cc)))
          pb = floor((fromIntegral (2*gma))*(log (fromIntegral gma)))
          sq = trace (show n ++ " " ++ show aa ++ " " ++ show l) $ ceiling (sqrt (fromIntegral (n+1))) -- sqrt of number of coefficients
          bpfactors q = let ps = findzps q pb f    -- baseprime factors
                         in if null ps then return (2,[])
                            else {- trace ("primes: " ++ show ps) $ -} do
                              -- putStrLn ("LC(f): " ++ show l)
                              fss <- mapM (\p -> (map snd) <$> cantorzassenhaus p (monicp p f)) ps
                              -- putStrLn ("factors: " ++ show fss)
                              let lss = dropWhile (<=1) $ sort (map length fss)
                              if null lss then bpfactors (maximum ps) else 
                                 let m = minimum lss
                                     i = fromJust (findIndex (==m) lss)
                                  in return ((ps!!i), (fss!!i))

  -------------------------------------------------------------------------
  -- Factor polynomials using zassenhaus algorithm
  -- https://docs.sympy.org/0.7.3/_modules/sympy/polys/factortools.html
  -------------------------------------------------------------------------
  factorp :: Poly Integer -> IO [Poly Integer]
  factorp f | degree f <= 1 = return [P[c], g]
            | otherwise     = do 
              fs <- zassenhaus s
              putStrLn (show fs)
              let rs = fs -- trialdiv s fs
              if c /= 1 then return (P[c]:rs) else return rs
    where c = content f
          g = primitive f
          l = lc g
          h | l < 0 = P [-c | c <- coeffs g]
            | otherwise = g
          s = squarefreepart h
  
  -------------------------------------------------------------------------
  -- Trial division
  -------------------------------------------------------------------------
  trialdiv :: Poly Integer -> [Poly Integer] -> [Poly Integer]
  trialdiv _ [] = []
  trialdiv f (p:ps) = go f p ++ trialdiv f ps
    where go a d | d == P[1] = []
                 | otherwise = 
                    let (q,r) = a `divpe` d
                    in if zerop r then d:go q d else []

  -------------------------------------------------------------------------
  -- combination
  -------------------------------------------------------------------------
  filterfacts :: Poly Integer -> [Poly Integer] -> [Poly Integer]
  filterfacts f fs = go (reverse $ tail (sortOn length $ Perm.ps fs))
    where go [] = []
          go (c:cs) | prodp mul c == f = c
                    | otherwise    = go cs
       
  -------------------------------------------------------------------------
  -- test zassenhaus algorithm
  -------------------------------------------------------------------------
  testLift :: IO ()
  testLift = do
    t <- forM [1..100] (\i -> do
      ts <- nMsRandomPolies 4 11 2
      let p = prodp mul ts
      let pc = content p
      let pp = primitive p
      -- fs <- (filterfacts pp) <$> zassenhaus pp
      fs <- zassenhaus pp
      -- showeq p pc fs ts
      -- if p /= prodp mul (P[pc]:fs) then return False 
      if pp /= prodp mul fs
         then do
           showeq pp pc fs ts
           return False 
         else if sortOn coeffs fs /= sortOn coeffs ts
              then return True --False
              else return True)
    if and t then putStrLn "PASSED" else putStrLn "FAILED"
    where showeq p pc fs ts = do
          -- when (length fs /= 3) $ putStrLn ("NOT FOUND: " ++ show (ts))
          if pc /= 1 
             then putStrLn (show p ++ " = " ++ "prodp mul [" ++ show (P[pc]:fs) ++ "]")
             else putStrLn (show p ++ " = " ++ "prodp mul [" ++ show fs ++ "]")
          
       
  -------------------------------------------------------------------------
  -- test zassenhaus algorithm
  -------------------------------------------------------------------------
  factorpoly :: Poly Integer -> IO [Poly Integer]
  factorpoly = zassenhaus

  -------------------------------------------------------------------------
  -- zassenhaus algorithm before 09/04/2020
  -- https://docs.sympy.org/0.7.3/_modules/sympy/polys/factortools.html
  -------------------------------------------------------------------------
  {-
  zassenhaus :: Poly Integer -> IO [Poly Integer]
  zassenhaus f | n == 1 = return [f]
               | otherwise = do
                    (p,fs) <- baseprime 2
                    if null fs then return [f] else do
                      let x = ceiling (logBase (fromIntegral p) (fromIntegral (2*bb+1)))
                      let g = sortOn degree (nub (hlift p x f $ map (modp p) fs))
                      let rs = filterzfs (p^l) f g
                      if null rs then return [f] else return rs
    where n  = fromIntegral (degree f)
          l  = lc f                            -- leading coefficient
          aa = maximum (map abs $ coeffs f)    -- greatest coefficient
          bb = 2^n*aa*l*sq                     -- bound for base
          sq = ceiling (sqrt (fromIntegral (n+1))) -- sqrt of number of coefficients
          c  = head (coeffs f)                 -- constant coefficient
          cc = (n+1)^(2*n)*aa*(2*n-1)
          gm = logBase 2 (fromIntegral cc)
          bound = ceiling (2*gm*(log gm))
          baseprime q = let ps = findzps q bb f
                         in if null ps then return (2,[])
                            else do
                              fss <- mapM (\p -> (map snd) <$> cantorzassenhaus p (modp p f)) ps
                              let lss = dropWhile (<=1) $ sort (map length fss)
                              if null lss then baseprime (maximum ps) else 
                                 let m = minimum lss
                                     i = fromJust (findIndex (==m) lss)
                                  in return ((ps!!i), (fss!!i))
  -}
       

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
  -- Vieta's Formulas
  -------------------
  -- 1) create the powerset of the roots
  -- 2) sort them by size
  -- 3) drop the first (the empty set)
  -- 4) group sets of equal length
  -- 5) build the products of each group
  -- 6) sum them
  -- 7) multiply them by (-1)^n
  -------------------------------------------------------------------------
  vieta :: (Real a) => [a] -> [a]
  vieta = c . g . d . s . Perm.ps
    where d   = drop 1
          g   = groupBy ((==) `on`  length) -- \x y -> length x == length y)
          s   = sortOn length -- (\x y -> length x `compare` length y)
          c p = [(-1)^n * sum (map product x) | (x,n) <- zip p [1..]] 
          
 
  -------------------------------------------------------------------------
  -- Solving equations
  -------------------------------------------------------------------------
  solve :: (Real a, Floating a, Fractional a) => Poly a -> [a]
  solve p = case degree p of
              0 -> coeffs p
              1 -> solve1 p
              2 -> solve2 p
              -- 3 -> solve3 p
              -- 4 -> solve4 p
              _ -> error "I don't know how to solve this polynomial"

  solve1 :: (Num a,Fractional a) => Poly a -> [a]
  solve1 (P [b,a]) = [-b/a]

  solve2 :: (Real a, Floating a, Fractional a) => Poly a -> [a]
  solve2 p@(P [c,b,a]) | dis2 p < 0 = []
                       | x1 /= x2  = [x1,x2]
                       | otherwise = [x1]
    where d  = sqrt (dis2 p)
          x1 = (-b + d) / (2*a)
          x2 = (-b - d) / (2*a)

  dis2 :: (Num a) => Poly a -> a 
  dis2 (P [c,b,a]) = b^2 - 4*a*c

  countRoots2 :: (Num a, Ord a) => Poly a -> Int
  countRoots2 p | dis2 p > 0 = 2
                | dis2 p < 0 = 0
                | otherwise = 1

  dis3 :: (Num a) => Poly a -> a
  dis3 (P [d,c,b,a]) = 18*a*b*c*d - 
                       4*b^3*d    +
                       b^2*c^2    -
                       4*a*c^3    -
                       27*a^2*d^2

  -- not correct...
  solve3 :: Poly Double -> [Double]
  solve3 (P [d,c,0,a]) | a /= 1    = solve3 (P [d/a,c/a,0,1])
                       | otherwise =
                         let disc = d^2/4 + c^3/9
                             u3   = -d/2 + sqrt disc
                             v3   = -d/2 - sqrt disc
                             u    | u3  < 0    = -(-u3)**(1/3)
                                  | otherwise  = u3**(1/3)
                             v    | v3  < 0    = -(-v3)**(1/3)
                                  | otherwise = v3**(1/3)
                          in if disc < 0 then [] else [u+v]
  solve3 (P [0,c,b,a]) | a /= 1    = solve3 (P [0,c/a,b/a,1])
                       | otherwise = 
                           let xs = solve2 (P [c,b,1])
                            in nub (0:xs)
  solve3 (P [d,c,b,a]) | a /= 1    = solve3 (P [d/a,c/a,b/a,1])
                       | otherwise = 
                         let p  = -(b^2)/3 + c
                             q  = (2*b^3)/27 - (b*c)/3 + d
                          in [y-a/3 | y <- solve3 (P [q,p,0,1])]
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
    
