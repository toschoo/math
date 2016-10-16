module Linear
where

  import Data.List (foldl')
  import Data.Ratio

  import Debug.Trace(trace)

  ------------------------------------------------------------------------
  -- Test for linear independence
  ------------------------------------------------------------------------
  indep :: [a] -> Bool
  indep = undefined

  ------------------------------------------------------------------------
  -- A vector space defined by its base
  -- The base is linearly independent and spans V
  ------------------------------------------------------------------------
  data Vs a = Vs [a] 

  ------------------------------------------------------------------------
  -- The standard basis, e.g. [[1,0,0],[0,1,0],[0,0,1]]
  ------------------------------------------------------------------------
  stdbasis :: (Num a) => Int -> [[a]]
  stdbasis b = ins 1 (take (b-1) (repeat 0))
    where ins x [] = [[x]]
          ins x (z:zs) = (x:z:zs) : (map (z:) $ ins x zs)

  ------------------------------------------------------------------------
  -- The dimension of a vector space as the length of its base
  ------------------------------------------------------------------------
  dim :: Vs a -> Int
  dim (Vs b) = length b

  ------------------------------------------------------------------------
  -- The zero vector (of the length of another vector)
  ------------------------------------------------------------------------
  zero :: (Num a) => [a] -> [a]
  zero xs = take (length xs) $ repeat 0 

  ------------------------------------------------------------------------
  -- Adding two vectors
  ------------------------------------------------------------------------
  addV :: (Num a) => [a] -> [a] -> [a]
  addV xs ys = [x+y | (x,y) <- zip xs ys]

  infix 8 -+-
  (-+-) :: (Num a) => [a] -> [a] -> [a]
  (-+-) = addV

  ------------------------------------------------------------------------
  -- Sum of n vectors
  ------------------------------------------------------------------------
  sumV :: (Num a) => [[a]] -> [a]
  sumV = foldV addV 0

  ------------------------------------------------------------------------
  -- Folding a vector function on a list of vectors
  ------------------------------------------------------------------------
  foldV :: ([a] -> [a] -> [a]) -> a -> [[a]] -> [a]
  foldV _ z [] = [z]
  foldV f z xs = foldl' f zs xs
    where zs = case xs of
                 []:_ -> [z]
                 x:_  -> take (length x) $ repeat z

  ------------------------------------------------------------------------
  -- A naive vector multiplication according to addition
  ------------------------------------------------------------------------
  vmul :: (Num a) => [a] -> [a] -> [a]
  vmul xs ys = [x*y | (x,y) <- zip xs ys]

  infix 8 -*-
  (-*-) :: (Num a) => [a] -> [a] -> [a]
  (-*-) = vmul

  ------------------------------------------------------------------------
  -- Scalar multiplication
  ------------------------------------------------------------------------
  smul :: (Num a) => a -> [a] -> [a]
  smul a xs = map (a*) xs

  infix 8 *-
  (*-) :: (Num a) => a -> [a] -> [a]
  (*-) = smul

  ------------------------------------------------------------------------
  -- Dot product
  ------------------------------------------------------------------------
  dotp :: (Num a) => [a] -> [a] -> a
  dotp xs ys = sum [x*y | (x,y) <- zip xs ys]

  infix 9 -*
  (-*) :: (Num a) => [a] -> [a] -> a
  (-*) = dotp

  ------------------------------------------------------------------------
  -- linear combination of two lists of vectors
  ------------------------------------------------------------------------
  lcombine :: (Num a) => [a] -> [[a]] -> [a]
  lcombine as xss = foldl' addV (zero as) [a `smul` xs | (a,xs) <- zip as xss]

  ------------------------------------------------------------------------
  -- The vector norm (its length in Euclidian space)
  ------------------------------------------------------------------------
  norm :: (Num a,Floating a) => [a] -> a
  norm x = sqrt (x -* x)

  ------------------------------------------------------------------------
  -- Two vectors are orthogonal if their dot product is 0
  -- Note: for real vector spaces we need an approximation...
  ------------------------------------------------------------------------
  orthogonal :: (Num a,Eq a) => [a] -> [a] -> Bool
  orthogonal xs ys = (xs -* ys) == 0

  ------------------------------------------------------------------------
  -- Dot product of a list of vectors
  ------------------------------------------------------------------------
  mdotp :: (Num a) => [[a]] -> a
  mdotp []  = 0
  mdotp [x] = 0
  mdotp (x:y:xs) = (x -* y) + mdotp (y:xs)

  ------------------------------------------------------------------------
  -- Test whether a list of vectors is orthonormal 
  -- Note: for real vector spaces we need an approximation...
  ------------------------------------------------------------------------
  orthonormal :: (Eq a,Num a,Floating a) => [[a]] -> Bool
  orthonormal vs = all (\v -> norm v == 1) vs && mdotp vs == 0

  ------------------------------------------------------------------------
  -- Projection of a vector
  ------------------------------------------------------------------------
  proj :: (Num a,Floating a,Eq a) => [a] -> [a] -> [a]
  proj u v | all (==0) u = u
           | otherwise   = ((v -* u) / (u -* u)) *- u 

  ------------------------------------------------------------------------
  -- Gram-Schmidt Process:
  -- Compute the "u"s and divide them by their norm
  -- Converts a base into a orthogonal base
  ------------------------------------------------------------------------
  gram :: (Num a,Floating a,Eq a) => [[a]] -> [[a]]
  gram xs = [(1/(norm u)) *- u | u <- us [] xs]

  ------------------------------------------------------------------------
  -- Compute the "u"s in the Gram-Schmidt Process
  ------------------------------------------------------------------------
  us :: (Num a, Floating a, Eq a) => [[a]] -> [[a]] -> [[a]]
  us rs [] = rs
  us [] (x:xs) = us [x] xs
  us rs (x:xs) = us ((x -+- (pj x (reverse rs))) : rs) xs
    where pj x [] = zero x
          pj x (s:ss) = ((-1) *- proj s x) -+- (pj x ss)

  ------------------------------------------------------------------------
  -- Matrices
  ------------------------------------------------------------------------
  data Matrix a = M [[a]]
    deriving (Show,Eq)

  rows :: Matrix a -> [[a]]
  rows (M rs) = rs

  ------------------------------------------------------------------------
  -- Add two matrices
  ------------------------------------------------------------------------
  addM :: (Num a) => Matrix a -> Matrix a -> Matrix a
  addM (M xs) (M ys) = M [addV x y | (x,y) <- zip xs ys]

  ------------------------------------------------------------------------
  -- Scalar multiplication
  ------------------------------------------------------------------------
  smulM :: (Num a) => a -> Matrix a -> Matrix a
  smulM a (M xs) = M [a `smul` x | x <- xs]

  ------------------------------------------------------------------------
  -- Matrix multiplication
  ------------------------------------------------------------------------
  mulM :: (Num a) => Matrix a -> Matrix a -> Matrix a
  mulM (M xs) (M ys) | rolen xs /= colen ys = 
                       error "Matrices are not compatible"
                     | otherwise = 
                       M [map sum [x -*- z | z <- zs] | x <- xs]
    where zs = col2row ys

  infix 9 |*|
  (|*|) :: (Num a) => Matrix a -> Matrix a -> Matrix a
  (|*|) = mulM

  ------------------------------------------------------------------------
  -- Convert a Matrix by making its columns the rows of the new matrix
  ------------------------------------------------------------------------
  col2row :: [[a]] -> [[a]]
  col2row []     = []
  col2row ([]:_) = []
  col2row zs     = heads zs : col2row [tail z | z <- zs]

  ------------------------------------------------------------------------
  -- This is called transpose
  ------------------------------------------------------------------------
  trans :: Matrix a -> Matrix a
  trans (M x) = M (col2row x)

  ------------------------------------------------------------------------
  -- First column
  ------------------------------------------------------------------------
  heads :: [[a]] -> [a]
  heads zs = [head z | z <- zs, not (null z)]

  ------------------------------------------------------------------------
  -- Column length
  ------------------------------------------------------------------------
  colen :: [[a]] -> Int
  colen = length

  ------------------------------------------------------------------------
  -- Row length
  ------------------------------------------------------------------------
  rolen :: [[a]] -> Int
  rolen []    = 0
  rolen (x:_) = length x

  ------------------------------------------------------------------------
  -- Bring a matrix into echelon form
  ------------------------------------------------------------------------
  echelon :: (Eq a,Num a) => Matrix a -> Matrix a
  echelon (M ms) = M (go ms)
    where go :: (Eq a,Num a) => [[a]] -> [[a]]
          go rs | null rs || 
                  null (head rs) = rs
                | null rs2       = map (0:) (go (map tail rs))
                | otherwise      = piv : map (0:) (go rs')
            where rs' = map (adjustWith piv) (rs1++rs3)
                  (rs1,rs2) = span (\(n:_) -> n==0) rs
                  (piv:rs3) = rs2

  ------------------------------------------------------------------------
  -- Adjust a row with a pivo
  ------------------------------------------------------------------------
  adjustWith :: (Num a) => [a] -> [a] -> [a]
  adjustWith (m:ms) (n:ns) = zipWith (-) (map (n*) ms) 
                                         (map (m*) ns)

  ------------------------------------------------------------------------
  -- Gaussian elimination
  ------------------------------------------------------------------------
  eliminate :: Rational -> Matrix Integer -> Matrix Integer
  eliminate r (M ms) = M (map (simplify n d) ms)
    where n = numerator   r
          d = denominator r
          simplify n d row = init (init row') ++ [d*lr - al*n]
            where lr   = last row
                  al   = last (init row)
                  row' = map (*d) row

  ------------------------------------------------------------------------
  -- Backward substitution
  -- Preconditions:
  --   - The input Matrix *must* be in echelon form
  --   - The number of columns must be greater than the number of rows,
  --     (otherwise we divide by 0)
  ------------------------------------------------------------------------
  backsub :: Matrix Integer -> [Rational]
  backsub (M ms) = go ms []
    where go [] rs = rs
          go xs rs = go xs' (p:rs)
            where a   = (last xs) !! ((rolen xs)-2)
                  c   = (last xs) !! ((rolen xs)-1)
                  p   = c % a
                  (M xs') = eliminate p $ M (init xs)

