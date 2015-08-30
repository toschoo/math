module Linear
where

  import Data.List (foldl')

  indep :: [a] -> Bool
  indep = undefined

  data Vs a = Vs [a] -- [a] is the basis of Vs
                     -- that is linear independent and
                     -- spans Vs

  dim :: Vs a -> Int
  dim (Vs b) = length b

  zero :: (Num a) => [a] -> [a]
  zero xs = take (length xs) $ repeat 0 

  addV :: (Num a) => [a] -> [a] -> [a]
  addV xs ys = [x+y | (x,y) <- zip xs ys]

  sumV :: (Num a) => [[a]] -> [a]
  sumV = foldV addV 0

  foldV :: ([a] -> [a] -> [a]) -> a -> [[a]] -> [a]
  foldV _ z [] = [z]
  foldV f z xs = foldl' f zs xs
    where zs = case xs of
                 []:_ -> [z]
                 x:_  -> take (length x) $ repeat z

  smul :: (Num a) => a -> [a] -> [a]
  smul a xs = map (a*) xs

  infix 8 *-
  (*-) :: (Num a) => a -> [a] -> [a]
  (*-) = smul

  dotp :: (Num a) => [a] -> [a] -> [a]
  dotp xs ys = [x*y | (x,y) <- zip xs ys]

  infix 9 -*-
  (-*-) :: (Num a) => [a] -> [a] -> [a]
  (-*-) = dotp

  lcombine :: (Num a) => [a] -> [[a]] -> [a]
  lcombine as xss = foldl' addV (zero as) [a `smul` xs | (a,xs) <- zip as xss]

  data Matrix a = M [[a]]
    deriving (Show,Eq)

  addM :: (Num a) => Matrix a -> Matrix a -> Matrix a
  addM (M xs) (M ys) = M [addV x y | (x,y) <- zip xs ys]

  smulM :: (Num a) => a -> Matrix a -> Matrix a
  smulM a (M xs) = M [a `smul` x | x <- xs]

  mulM :: (Num a) => Matrix a -> Matrix a -> Matrix a
  mulM (M xs) (M ys) | rolen xs /= colen ys = 
                       error "Matrices are not compatible"
                     | otherwise = 
                       M [map sum [dotp x z | z <- zs] | x <- xs]
    where zs = col2row ys

  infix 9 |*|
  (|*|) :: (Num a) => Matrix a -> Matrix a -> Matrix a
  (|*|) = mulM

  col2row :: [[a]] -> [[a]]
  col2row []     = []
  col2row ([]:_) = []
  col2row zs     = heads zs : col2row [tail z | z <- zs]

  heads :: [[a]] -> [a]
  heads zs = [head z | z <- zs, not (null z)]

  colen :: [[a]] -> Int
  colen = length

  rolen :: [[a]] -> Int
  rolen []    = 0
  rolen (x:_) = length x

