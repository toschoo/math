module Modular
where

  import Prelude hiding (mod)
  import Data.Ratio 
  import Binom
  import Prime
  import Debug.Trace (trace)
  import Data.List (nub,sort,delete,(\\))
  import Group hiding (mul)

  ring :: Integer -> [Integer]
  ring p = map (`rem` p) [1..p-1]

  data Module = Module Integer Integer

  tomod :: Integer -> Integer -> Module
  tomod n a | a < 0     = Module (-((-a) `rem` n) + n) n
            | otherwise = Module (a `rem` n) n
  
  instance Show Module where
    show (Module a p) = show (a `rem` p)

  instance Eq Module where
    (Module a1 n) == (Module a2 n') | n /= n'   = error "not the same ring"
                                    | otherwise = (a1 `rem` n) == 
                                                  (a2 `rem` n)

  instance Num Module where
    a + b = op_ add a b
    a - b = op_ sub a b
    a * b = op_ mul a b
    negate (Module a1 n) = Module (-a1) n
    abs    (Module a1 n) = Module (abs a1) n
    signum (Module a1 n) = Module (signum a1) n
    fromInteger i        = Module i 1

  instance Ord Module where
    compare x y = withGuard cmp x y 
      where cmp (Module a1 n) (Module a2 _) = compare a1 a2

  instance Integral Module where
    quotRem x@(Module _ n) y = (withGuard mDiv x y, Module 0 n)
    toInteger (Module a n) = a `rem` n

  instance Real Module where
    toRational (Module a n) = fromIntegral (a `rem` n)

  instance Enum Module where
    fromEnum (Module a n) = fromIntegral (a `rem` n)
    toEnum   i            = tomod 1 $ fromIntegral i

  instance Fractional Module where 
    (/) = mDiv
    fromRational r = let a = numerator   r
                         b = denominator r
                      in tomod b a

  instance Group Module where
    op = op_ mul
    idty = 1
    inv = minv
    group (Module n _) = map (tomod n) [1..n-1]

  op_ :: (Module -> Module -> Module) -> Module -> Module -> Module
  op_ = withGuard 

  withGuard :: (Module -> Module -> r) -> Module -> Module -> r
  withGuard f x@(Module a1 n) 
              y@(Module b2 n') | n /= n'   = error "not the same ring"
                               | otherwise = f x y

  add :: Module -> Module -> Module
  add (Module a1 n) (Module a2 _) = Module (a1 + a2 `rem` n) n

  sub :: Module -> Module -> Module
  sub (Module a1 n) (Module a2 _) | a1 < a2   = Module (a1 - a2 + n) n
                                  | otherwise = Module (a1 - a2    ) n

  mul :: Module -> Module -> Module
  mul (Module a1 n) (Module a2 _) = Module (a1 * a2 `rem` n) n

  -- this does work only if u * a1 + v * a2 = 1
  -- and this does work only if a1 and a2 a coprime
  -- therefore a prime module is a field!
  -- for instance xgcd 6 12 = (2,(1,0))
  -- 1 * 6 + 0 * 12 =/= gcd 6 12, but
  -- xgcd 6 11 = (1,(2,-1)) and
  -- 2 * 6 + (-1) * 11 = gcd 6 11 = 1
  mDiv :: Module -> Module -> Module
  mDiv (Module a1 n) (Module a2 _) = let (_,(u,_)) = xgcd a2 n
                                         k = if u < 0 then u + n else u
                                      in Module ((k * a1) `rem` n) n

  -- introduce inverse explicitly in section on Congruence
  inverse :: Integer -> Integer -> Integer
  inverse a p = let k = fst $ snd $ xgcd a p 
                 in if k < 0 then k + p else k

  minv :: Module -> Module
  minv (Module n a) = tomod n $ inverse a n

  -- show examples !
  inverses :: Integer -> [(Integer,Integer)]
  inverses 2 = [(1,1)]
  inverses p = let is = [(x, x `inverse` p) | x <- [1..p-1]]
                in clean is
    where clean [] = []
          clean ((f,s):xs) = (f,s) : clean (delete (s,f) xs)

  xgcd :: Integer -> Integer -> (Integer, (Integer, Integer))
  xgcd a b = go a b 1 0 0 1
    where go 0 d _  _  ud vd = (d,(ud,vd))
          go c d uc vc ud vd = -- trace ("ud = " ++ show ud ++ ", vd = " ++ show vd) $
                               let (q,r) = d `quotRem` c
                                in go r c (ud - q * uc) 
                                          (vd - q * vc) uc vc   

  mSum :: Integer -> [Module] -> Module
  mSum i = foldr (+) (tomod i 0)

  mProduct :: Integer -> [Module] -> Module
  mProduct i = foldr (*) (tomod i 1)

  -------------------------------------------------------------------------
  -- Congurence
  -------------------------------------------------------------------------
  congruent :: Integer -> Integer -> Integer -> Bool
  congruent a b n = rem (a-b) n == 0

  natCongruent :: Integer -> Integer -> Integer -> Bool
  natCongruent a b n = rem a n == rem b n

  mod :: Integer -> Integer -> Integer
  mod a n = fromIntegral (tomod n a)

  chinese :: [Integer] -> [Integer] -> Integer
  chinese as ns = let pN = product ns
                      e  = sum $ mm as (getEs as ns pN)
                   in e `mod` pN
    where getE a n pN = let b = pN `div` n
                            (_,(k,_)) = xgcd b n 
                         in k*b
          getEs [] _  _= []
          getEs _ []  _= []
          getEs (x:xs) (z:zs) pN = (getE x z pN) : getEs xs zs pN
          mm [] _ = []
          mm _ [] = []
          mm (x:xs) (z:zs) = (x * z) : mm xs zs

  -------------------------------------------------------------------------
  -- Map chinese on all combinations of coprimes of two coprimes
  -------------------------------------------------------------------------
  consys :: Integer -> Integer -> [[Integer]]
  consys a b = concatMap mm (coprimes b)
    where mm y = [[x,y] | x <- coprimes a]

  china :: Integer -> Integer -> [Integer]
  china a b = map (esenich [a,b]) (consys a b) 
    where esenich = flip chinese
  
  -------------------------------------------------------------------------
  -- Residues
  -------------------------------------------------------------------------
  isResidue :: Integer -> Integer -> Integer -> Bool
  isResidue q n x = x^2 `mod` n == q

  isResidueOf :: Integer -> Integer -> Bool
  isResidueOf 0 _ = True
  isResidueOf q p = let a = q^((p-1) `div` 2) 
                     in a `rem` p == 1

  residues :: Integer -> [Integer]
  residues n = sort $ nub [(x^2) `rem` n | x <-  [0..n-1]]

  countResidues :: Integer -> Int
  countResidues = length . residues

  hasResidue :: Integer -> Integer -> Bool
  hasResidue n q  | q < 0 && abs q > n = hasResidue n (q `rem` n)
                  | q < 0              = hasResidue n (n+q) 
                  | otherwise          = check 0
    where check x | x == n             = False
                  | (x^2) `mod` n == q = True
                  | otherwise          = check (x+1)

  haveResidue :: Integer -> [Integer] -> [Integer]
  haveResidue _ [] = []
  haveResidue q (n:ns) | n `hasResidue` q = n : haveResidue q ns
                       | otherwise        =     haveResidue q ns

  primesWithResidue :: Integer -> [Integer]
  primesWithResidue = (`haveResidue` (drop 1 allprimes))

  minus1Residues :: [Integer]
  minus1Residues = go (drop 1 allprimes)
    where go []  = []
          go (p:ps) | p `rem` 4 == 1 = p : go ps
                    | otherwise      =     go ps

  euCriterion :: Integer -> Integer -> Bool
  euCriterion a p | a < 0 && abs a > p = euCriterion (a `rem` p) p
                  | a < 0              = euCriterion (p + a)     p
                  | otherwise          = let n = (p-1) `div` 2 
                                          in (a^n) `mod` p == 1

  legendre :: Integer -> Integer -> Integer
  legendre a p = let n = (p-1) `div` 2 
                  in case (a^n) `mod` p of
                       0 -> 0
                       1 -> 1
                       x -> -1

  reciprocal :: Integer -> Integer -> Bool
  reciprocal p q | even (((p-1) * (q-1)) `div` 4) = hasResidue q p
                 | otherwise                      = not (hasResidue q p)

  
  -------------------------------------------------------------------------
  -- Generators
  -------------------------------------------------------------------------
  generate :: Integer -> Integer -> [Integer]
  generate p x = nub $ map (\a -> (x^a) `rem` p) [1..p-1]

  generators :: Integer -> [[Integer]]
  generators p = map (generate p) [2..p-2]

  countOrder :: Integer -> [Int]
  countOrder = nub . map length . generators

  minOrder :: Integer -> Int
  minOrder = minimum . countOrder

  orderRatio :: Integer -> Integer
  orderRatio p = p `div` (fromIntegral $ minOrder p)

  sophieprimes :: [Integer]
  sophieprimes = filter (\n -> wprime (2*n+1)) allprimes

  safeprimes :: [Integer]
  safeprimes = map (\n -> 2*n+1) sophieprimes

  coset :: Integer -> Integer -> Integer -> [Integer]
  coset p g c = let h = generate p g
                 in map (\a -> (a*c) `rem` p) h

  cosets :: Integer -> Integer -> [[Integer]]
  cosets p g = let h = generate p g
                   c = [1..p-1] \\ h
                in map (f h) c
    where f h c = map (\a -> (a*c) `rem` p) h

  {-
    Let p be a safe prime number. Then there exists a Sophie Germain prime
    number q and there exist strict subgroups H1, H2, H3, H4 of Z/pZ* such
    that Card H1 = 1 and Card H2 = 2 and Card H3 = q and Card H4 = 2 · q.
    and for every strict subgroup H of Z/pZ* holds H = H1 or H = H2 or
    H = H3 or H = H4.
  -}
    
