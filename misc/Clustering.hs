module Clustering
where

  import Data.List (foldl',delete,nub,sort)
  import Debug.Trace (trace)

  type Vector = [Double]

  class Cluster c where
    label      :: c -> String
    empty      :: c -> Bool
    centroid   :: c -> Vector
    centrdst   :: c -> Double
    population :: c -> [Vector] -- keep it more flexible
    update     :: c -> c
    distance   :: Vector -> c -> Double
    insv       :: Vector -> c -> c
    delv       :: Vector -> c -> c

  -------------------------------------------------------------------------
  -- Create seeds
  -------------------------------------------------------------------------
  seeds :: (Cluster c) => 
           (String -> Vector -> c)       -> 
           (Int -> [Vector] -> IO [Vector]) ->
           [String] -> [Vector] -> IO [c]
  seeds mk sel ls vs = let i = length ls in do
    sds <- sel i vs
    return $ map (uncurry mk) $ zip ls sds

  -------------------------------------------------------------------------
  -- Partition into n clusters
  -------------------------------------------------------------------------
  part :: (Eq c, Cluster c) => [Vector] -> [c] -> [c]
  part [] cs = cs
  part (v:vs) cs = let c  = getMinimum cs $ map (distance v) cs
                       c' = update (insv v c)
                    in part vs (c':delete c cs) -- strict delete?

  -------------------------------------------------------------------------
  -- optimise clusters (n rounds)
  -------------------------------------------------------------------------
  optimiseN :: (Eq c,Cluster c) => Int -> [c] -> [c]
  optimiseN 0 c = c
  optimiseN n c = optimiseN (n-1) (optimiseall c)

  -------------------------------------------------------------------------
  -- optimise while condition is fulfilled
  -------------------------------------------------------------------------
  optimiseWhile :: (Eq c,Cluster c) => ([c] -> [c] -> Bool) -> [c] -> [c]
  optimiseWhile cond c = let c' = optimiseall c 
                          in if cond c c' 
                               then optimiseWhile cond c' 
                               else c'

  -------------------------------------------------------------------------
  -- optimise while there are changes (may run infinitely)
  -------------------------------------------------------------------------
  optimise :: (Eq c,Cluster c) => [c] -> [c]
  optimise = optimiseWhile anychange
    where anychange x y = any (uncurry change) $ zip x y

  -------------------------------------------------------------------------
  -- optimise list of clusters
  -------------------------------------------------------------------------
  optimiseall :: (Eq c,Cluster c) => [c] -> [c]
  optimiseall [] = []
  optimiseall cs = go cs cs 
    where go [] bs = bs
          go (a:as) bs = go as (optimise1 (a:delete a bs))

  -------------------------------------------------------------------------
  -- head of list donates to others
  -------------------------------------------------------------------------
  optimise1 :: (Eq c,Cluster c) => [c] -> [c]
  optimise1 [] = []
  optimise1 (c:cs) = let (c',cs') = go c cs (population c) in c':cs'
    where go c cs []     = (c,cs)
          go c cs (v:vs) = let (c',cs') = donate v c cs in go c' cs' vs

  -------------------------------------------------------------------------
  -- reevaluate all individuals
  -------------------------------------------------------------------------
  donate :: (Eq c,Cluster c) => Vector -> c -> [c] -> (c,[c])
  donate _ c [] = (c,[])
  donate v c cs = 
    let t  = getMinimum cs $ map (distance v) cs
        d1 = distance v c 
        d2 = distance v t
     in if d2 < d1 then (update (delv v c),
                         update (insv v t):delete t cs)
                   else (c,cs)

  -------------------------------------------------------------------------
  -- Quick test if there is a difference between two clusters
  -------------------------------------------------------------------------
  change :: (Eq c,Cluster c) => c -> c -> Bool
  change a b = centrdst a /= centrdst b

  -------------------------------------------------------------------------
  -- get max/min cluster
  -------------------------------------------------------------------------
  getMinimum :: Cluster c => [c] -> [Double] -> c
  getMinimun _  [] = error "minimum on empty list"
  getMinimum cs ds = cs!!(least ds)

  getMaximum :: Cluster c => [c] -> [Double] -> c
  getMaximum _  [] = error "maximum on empty list"
  getMaximum cs ds = cs!!(major ds)

  -------------------------------------------------------------------------
  -- get max/min position
  -------------------------------------------------------------------------
  least :: [Double] -> Int
  least = extrm (>)

  major :: [Double] -> Int
  major = extrm (<)

  extrm :: (Double -> Double -> Bool) -> [Double] -> Int
  extrm op ds = go 0 0 (head ds) ds
    where go _ s _ [] = s
          go i s p (x:xs) | p `op` x  = go (i+1) i x xs
                          | otherwise = go (i+1) s p xs

  -------------------------------------------------------------------------
  -- find 2 most distant vectors
  -------------------------------------------------------------------------
  mostDistant2 :: (Vector -> Vector -> Double) -> [Vector] -> [Vector]
  mostDistant2 _ [] = []
  mostDistant2 _ [v] = [v]
  mostDistant2 dst (v:u:vs) = go (dst v u) [v,u] vs
    where go _ rs [] = rs
          go d [v1,v2] (x:xs) = let d2 = dst v1 x
                                    d3 = dst v2 x
                                    d' = max d2 d3
                                    v' | d' == d2  = v1
                                       | otherwise = v2
                                 in if d' > d
                                      then go d' [v',x] xs
                                      else go d  [v1,v2] xs

  -------------------------------------------------------------------------
  -- K-means
  -------------------------------------------------------------------------
  data KMean = KMean {
                  cLab :: String,
                  cCtr :: Vector,
                  cDst :: Double,
                  cPop :: [Vector]}
    deriving Show

  mkMean :: String -> Vector -> KMean
  mkMean l v = KMean l v 0 [v]

  instance Cluster KMean where
    label = cLab
    empty = null . cPop 
    centroid = cCtr
    centrdst = cDst 
    population = cPop
    distance v c = euclid v (centroid c)
    update = means
    insv v c = c{cPop = v:cPop c}
    delv v c = c{cPop = delete v (cPop c)}

  instance Eq KMean where
    c1 == c2 = cLab c1 == cLab c2

  -------------------------------------------------------------------------
  -- recompute mean
  -------------------------------------------------------------------------
  means :: KMean -> KMean
  means c = let coid = vmean $ cPop c
                mdst = wcss coid $ cPop c
             in c{cCtr = coid, 
                  cDst = mdst}

  -------------------------------------------------------------------------
  -- mean centroid
  -------------------------------------------------------------------------
  vmean :: [Vector] -> Vector
  vmean [] = []
  vmean ([]:_) = []
  vmean vs = (avg $ map head vs) : vmean (map tail vs)

  -------------------------------------------------------------------------
  -- mean distance to centroid
  -------------------------------------------------------------------------
  wcss :: Vector -> [Vector] -> Double
  wcss m = avg . map (euclid m)

  -------------------------------------------------------------------------
  -- Euclidian distance
  -------------------------------------------------------------------------
  euclid :: Vector -> Vector -> Double
  euclid v u = sqrt $ sum [(p-q)^2 | (p,q) <- zip v u]

  -------------------------------------------------------------------------
  -- dot product
  -------------------------------------------------------------------------
  dotprod :: Vector -> Vector -> Double
  dotprod v u = sum [p*q | (p,q) <- zip v u]

  -------------------------------------------------------------------------
  -- median
  -------------------------------------------------------------------------
  median :: [Double] -> Double
  median [] = error "median on empty list"
  median ds = let xs = sort ds
                  l  = length xs
                  l2 = l `div` 2
                  m  | l <= 2    = take 1 xs
                     | even l    = take 2 $ drop (l2-1) xs
                     | otherwise = take 1 $ drop l2 xs
                  in avg m

  -------------------------------------------------------------------------
  -- mean
  -------------------------------------------------------------------------
  avg :: [Double] -> Double
  avg [] = error "avg on empty list"
  avg xs = sum xs / (fromIntegral $ length xs)

