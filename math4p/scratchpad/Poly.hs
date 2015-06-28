module Poly
where

  import Data.List (nub, sortBy, foldl', intercalate)
  import Data.Maybe (catMaybes)

  -- naming convention
  -- e.g. pdiv, pmul, etc.
  --   or divPoly, mulPoly, ..?
  type Term c = (Int, c)

  data Poly c = Poly [Term c]
    deriving (Show,Eq)

  -- fromList, toList
  -- fromTerms, toTerms 
  -- fromMath, toMath

  fromList :: (Eq c, Num c) => [c] -> Poly c
  fromList cs = let xs = dropWhile (== 0) cs
                 in Poly $ 
                      reverse $ zip [0..(fromIntegral $ length cs-1)] xs

  toString :: (Eq c, Show c, Num c) => Poly c -> String
  toString (Poly cs) = 
    intercalate " + " $ map (\(a,b) -> showTerm a b) cs
    where showTerm 0 c   = show c
          showTerm 1 1   = "x"
          showTerm 1 c   = show c ++ "x" 
          showTerm k 1   = "x^" ++ show k
          showTerm k c   = show c ++ "x^" ++ show k 

  fromTerms :: (Show c, Eq c, Num c) => [Term c] -> Poly c
  fromTerms cs = Poly (simplify cs)

  toTerms :: Poly c -> [Term c]
  toTerms (Poly cs) = cs

  simplify :: (Show c, Eq c, Num c) => [Term c] -> [Term c]
  simplify = filter (\x -> not (fromIntegral 0 == snd x)) . addXX . sortTerms
    where addXX []       = []
          addXX [x]      = [x]
          addXX ((k1,c1):(k2,c2):cs) 
            | k1 == k2  =   addXX ((k1, c1 + c2):cs) 
            | otherwise = (k1,c1):addXX ((k2,c2):cs)

  sortTerms :: [Term c] -> [Term c]
  sortTerms = sortBy (\(k1,_) (k2,_) -> compare k2 k1)

  deg :: Poly c -> Int
  deg (Poly cs) = degTerms cs

  lc :: (Show c, Num c) => Poly c -> c
  lc (Poly cs) = lcTerms cs

  pnull :: (Show c, Num c, Eq c) => Poly c -> Bool
  pnull (Poly cs) = null $ simplify cs

  padd :: (Eq c, Show c, Num c) => Poly c -> Poly c -> Poly c
  padd (Poly xs) (Poly ys) = fromTerms (xs ++ ys)

  psub :: (Eq c, Show c, Num c) => Poly c -> Poly c -> Poly c
  psub (Poly xs) (Poly ys) = fromTerms (xs ++ negateTerms ys)

  pmul :: (Eq c, Show c, Num c) => Poly c -> Poly c -> Poly c
  pmul (Poly xs) (Poly ys) = fromTerms (concat $ mulNN xs ys)

  ppow :: (Eq c, Show c, Num c, Integral e) => Poly c -> e -> Poly c
  ppow _ 0 = unity
  ppow p 1 = p
  ppow p e | odd e     = p `pmul` ppow p (e-1)
           | otherwise = let p' = ppow p (e `div` 2) 
                          in p' `pmul` p'
                              
  pdiv :: (Show c, Eq c, Num c, Fractional c) => 
              Poly c -> Poly c -> (Poly c, Poly c)
  pdiv (Poly xs) (Poly []) = error "pdiv: division by zero"
  pdiv (Poly xs) (Poly ys) = 
    let (q,r) = euclide [] xs (degTerms ys) (lcTerms ys) 
     in (fromTerms q, fromTerms r)
    where euclide q [] d c = (q,[])
          euclide q r@(x:xx) d c | not (null r) && degTerms r < d = (q,r)
                                 | otherwise      =
            let s         = (fst x - d, snd x / c)
                (Poly q') = padd (fromTerms q) (fromTerms [s]) 
                (Poly r') = psub (fromTerms r) (pmul (fromTerms [s]) 
                                                     (fromTerms ys))
             in euclide q' r' d c  

  pIntDiv :: (Show c, Eq c, Num c, Integral c) => 
             Poly c -> Poly c -> Maybe (Poly c, Poly c)
  pIntDiv p1 p2 = let (q,r) = (pFromIntegral p1) `pdiv` (pFromIntegral p2)
                   in if pIsInt q && pIsInt r 
                        then Just (pFloor q, pFloor r) else Nothing

  pAllDivide :: (Show c, Eq c, Num c, Integral c) =>
                Poly c -> Poly c -> Bool
  pAllDivide p1 p2 = let (q, r) = (pFromIntegral p1) `pdiv` (pFromIntegral p2)
                      in pIsInt q && pIsInt r

  pdivides :: (Show c, Eq c, Num c, Floating c) =>
              Poly c -> Poly c -> Bool
  pdivides p1 p2 = let (_, r) = p1 `pdiv` p2
                    in r == zero

  pnegate :: (Eq c, Show c, Num c) => Poly c -> Poly c
  pnegate (Poly cs) = fromTerms $ negateTerms cs

  invert :: (Eq c, Show c, Num c) => Poly c -> Poly c
  invert (Poly cs) = fromTerms $ negateTerms cs

  monic :: (Show c, Num c, Eq c, Fractional c) => Poly c -> Poly c
  monic (Poly []) = Poly []
  monic (Poly ((k,co):cs)) = fromTerms $ (k,fromIntegral 1):(map (clean co) cs)
    where clean c (k,co) = (k,co/c)

  pgcd :: (Eq c, Fractional c, Show c) => Poly c -> Poly c -> Poly c
  pgcd a b | a `ltPoly` b = pgcd b a
           | otherwise  = let (_,r) = a `pdiv` b
                           in if pnull r then monic b else pgcd b r

  derivative :: (Eq c, Show c, Num c, Fractional c) => Poly c -> Poly c
  derivative (Poly cs) = fromTerms $ map derive cs
    where derive (k,c) = if k /= 0 then (k-1,(fromIntegral k) * c)
                                   else (0,fromIntegral 0)

  integrate :: (Eq c, Show c, Num c, Fractional c) => Poly c -> Poly c
  integrate (Poly cs) = fromTerms $ map go cs
    where go (k,c) = if k /= -1 then (k+1, c / fromIntegral (k+1)) 
                                else error "integral should yield ln"

  sqrfree :: (Eq c, Show c, Num c, Fractional c) => Poly c -> Bool
  sqrfree p = pgcd p (derivative p) == unity

  unity :: (Num c, Eq c, Show c) => Poly c
  unity = fromTerms [(0,1)]

  zero :: (Num c, Eq c, Show c) => Poly c
  zero = fromTerms []

  xPoly :: (Num c, Eq c, Show c) => Poly c
  xPoly = fromTerms [(1,1)]

  apply :: (Num c) => Poly c -> c -> c
  apply (Poly cs) x = 
    foldl' (\v -> (+) v . (\(k,c) -> c * (pow x k))) (fromIntegral 0) cs
    where pow x 0 = 1
          pow x n = x * pow x (n-1)
  
  roots :: (Eq c, Show c, Num c, Floating c) => Poly c -> [c]
  roots p@(Poly cs) = go $ normTerms cs
    where go [] = []
          go [(1,a),(0,b)]       = [-b / a]
          go [(2,a),(1,b),(0,c)] = let det = sqrt (b^2 - 4*a*c)
                                     in nub [(-b-det)/(2*a),(-b+det)/(2*a)]
          go [(3,a), (2,b),(1,c),(0,d)] = undefined
          go [(4,a), (3,b), (2,c),(1,d),(0,e)] = undefined

  intFactors :: (Eq c, Show c, Num c, Integral c) => Poly c -> [Poly c]
  intFactors p = 
    let rs = roots (monic $ pFromIntegral p)
        is = catMaybes $ map getIntegrals rs
        ps = map mkPoly is
     in if lc p /= 1 && not (null ps) then (lcPoly p):ps else ps
    where getIntegrals k | isInt k    = Just $ floor k
                         | otherwise  = Nothing
          mkPoly k = fromTerms [(1,1),(0,-k)]
          lcPoly x = fromTerms [(0,lc x)]

  pproduct :: (Eq c, Show c, Num c) => [Poly c] -> Poly c
  pproduct = foldl' pmul unity

  psum :: (Eq c, Show c, Num c) => [Poly c] -> Poly c
  psum = foldl' padd zero

  pFromIntegral :: (Num c, Integral c, Eq b, Show b, Num b) => Poly c -> Poly b
  pFromIntegral (Poly cs) = fromTerms $ map (\(x,c) -> (x, fromIntegral c)) cs

  pFloor :: (Num c, Eq c, RealFrac c, Show b, Integral b) => Poly c -> Poly b
  pFloor (Poly cs) = fromTerms $ map (\(x,c) -> (x, floor c)) cs

  eisenstein :: (Show c, Eq c, Num c, Integral c) =>
                Poly c -> c -> Bool
  eisenstein (Poly cc) p | null cc   = True
                         | otherwise = let cs = normTerms cc
                                           rs = map (\c -> snd c `rem` p) cs
                                           r1 = head rs
                                           rn = (snd $ last cs) `rem` p^2 
                                        in all (== 0) (tail rs) &&
                                           rn /= 0       &&
                                           r1 /= 0

  pIsInt :: (Num c, RealFrac c, RealFloat c) => Poly c -> Bool
  pIsInt (Poly cs) = all (isInt . snd) cs

  isInt :: (Num f, RealFrac f, RealFloat f) => f -> Bool
  isInt f | isNaN f              = False
          | floor f == ceiling f = True
          | otherwise            = False

  normTerms :: (Num c) => [Term c] -> [Term c]
  normTerms []     = []
  normTerms (x:xx) = go (x:xx) (fst x)
    where go [] _  = []
          go xx@(x:xs) n | n > fst x = go ((n,0):xx) (n-1)
                         | otherwise = x:go xs (n-1)

  negateTerms :: (Eq c, Show c, Num c) => [Term c] -> [Term c]
  negateTerms cs = map (\(k,c) -> (k, c * fromIntegral (-1))) cs

  invertTerms :: (Eq c, Show c, Num c) => [Term c] -> [Term c]
  invertTerms cs = map (\(k,c) -> ((-1)*k,c)) cs

  mul1 :: (Num c, Show c) => Term c -> Term c -> Term c
  mul1 (k1,c1) (k2,c2) = (k1+k2,c1*c2)

  mulN :: (Num c, Show c) => [Term c] -> Term c -> [Term c]
  mulN xx x = map (mul1 x) xx

  mulNN :: (Num c, Show c) => [Term c] -> [Term c] -> [[Term c]]
  mulNN xx yy = map (mulN yy) xx

  degTerms :: [Term c] -> Int
  degTerms [] = 0
  degTerms cs = fst $ head cs
  
  lcTerms :: (Show c, Num c) => [Term c] -> c
  lcTerms [] = fromIntegral 0
  lcTerms cs = snd $ head cs

  cmpPoly :: (Int -> Int -> Bool) -> Poly c -> Poly c -> Bool
  cmpPoly cmp a b = (deg a) `cmp` (deg b)

  gtPoly, eqPoly, ltPoly, lePoly, gePoly :: Poly c -> Poly c -> Bool
  gtPoly = cmpPoly (>)
  gePoly = cmpPoly (>=)
  ltPoly = cmpPoly (<)
  lePoly = cmpPoly (<=)
  eqPoly = cmpPoly (==)

  -- factoring
  -- read from formula
  -- composition
  -- solve 2nd degree, 3rd degree, 4th degree
  -- galois
  -- plot
  -- from y-values
  -- transformations: binomial coefficients
