module Clustering
where

  import Data.List (foldl',delete,nub)

  -------------------------------------------------------------------------
  -- Clustering machine (defining notions of "distance", "representative") 
  -- currently not used...
  -------------------------------------------------------------------------
  data Clustering a = Clustering {
                        clDist  :: Double -> Double -> Double,
                        clRep   :: [Double] -> Double,
                        clState :: [Cluster]
                      }

  -------------------------------------------------------------------------
  -- Cluster data type
  -------------------------------------------------------------------------
  data Cluster = Cluster String Double [Double]
    deriving (Show)

  -------------------------------------------------------------------------
  -- Clusters are equal if they have the same label
  -------------------------------------------------------------------------
  instance Eq  Cluster where
    (Cluster l1 _ _) == (Cluster l2 _ _) = l1 == l2

  -------------------------------------------------------------------------
  -- Create a cluster from scratch
  -------------------------------------------------------------------------
  mkCluster :: String -> [Double] -> Cluster
  mkCluster l ms = Cluster l (representative ms) ms

  -------------------------------------------------------------------------
  -- Create a cluster from another cluster
  -------------------------------------------------------------------------
  cluster :: Cluster -> [Double] -> Cluster
  cluster _               [] = error "Empty cluster!"
  cluster (Cluster l _ _) ms = Cluster l (representative ms) ms

  -------------------------------------------------------------------------
  -- Size of the cluster
  -------------------------------------------------------------------------
  size :: Cluster -> Int
  size (Cluster _ _ ms) = length ms

  -------------------------------------------------------------------------
  -- Cluster label
  -------------------------------------------------------------------------
  label :: Cluster -> String
  label (Cluster l _ _) = l

  -------------------------------------------------------------------------
  -- Representative
  -------------------------------------------------------------------------
  rep :: Cluster -> Double
  rep (Cluster _ r _) = r
 
  -------------------------------------------------------------------------
  -- Cluster members
  -------------------------------------------------------------------------
  members :: Cluster -> [Double]
  members (Cluster _ _ ms) = ms

  -------------------------------------------------------------------------
  -- Distance between two data points
  -------------------------------------------------------------------------
  distance :: Double -> Double -> Double
  distance x y  = abs (x-y)

  -------------------------------------------------------------------------
  -- Distance between a data point and a cluster
  -------------------------------------------------------------------------
  clusterDist :: Double -> Cluster -> Double
  clusterDist x (Cluster _ r _) = distance x r

  -------------------------------------------------------------------------
  -- Compute the representative of a cluster (medoid)
  -------------------------------------------------------------------------
  representative :: [Double] -> Double
  representative = medoid 

  -------------------------------------------------------------------------
  -- Compute the medoid
  -------------------------------------------------------------------------
  medoid :: [Double] -> Double
  medoid [] = error "medoid on empty list"
  medoid xs = let l  = length xs
                  l2 = l `div` 2
                  m  | l <= 2    = take 1 xs
                     | even l    = take 2 $ drop (l2-1) xs
                     | otherwise = take 1 $ drop l2 xs
                  in avg m

  -------------------------------------------------------------------------
  -- Compute the means
  -------------------------------------------------------------------------
  avg :: [Double] -> Double
  avg [] = error "avg on empty list"
  avg xs = sum xs / (fromIntegral $ length xs)
  
  -------------------------------------------------------------------------
  -- Add a data point to a cluster
  -------------------------------------------------------------------------
  ins :: Double -> Cluster -> Cluster 
  ins x c@(Cluster l r ms) = cluster c (x:ms)

  -------------------------------------------------------------------------
  -- Remove a data point from a cluster
  -------------------------------------------------------------------------
  del :: Double -> Cluster -> Cluster 
  del x c@(Cluster l r ms) | length ms <= 1 = c
                           | otherwise      = cluster c (delete x ms)

  -------------------------------------------------------------------------
  -- The data points farest away from the representative
  -------------------------------------------------------------------------
  farest :: Cluster -> [Double]
  farest (Cluster _ r ms) = let ds = map (distance r) ms
                             in fish (maximum ds) ms ds

  -------------------------------------------------------------------------
  -- Default cluster algorithm;
  -- we could think of a monadic approach:
  -- a Clustering as a monad that consists of functions for
  -- distance and representative
  -- and the current state [Cluster].
  -- We would then call runClustering which computes
  -- distance and representative as 
  --   - clDist Clustering and 
  --   - clRep  Clustering
  -------------------------------------------------------------------------
  cluster1 :: Int -> [String] -> [Double] -> [Cluster]
  cluster1 n ls xs | n > length ls = error "not enough labels!" 
                   | n < length ls = error "too many labels!"
                   | otherwise     =  
                     go (drop n xs) [Cluster l s [s] | 
                                     (l,s) <- take n $ zip ls xs]
    where go [] cs     = cs
          go (p:ps) cs = go ps (step p cs)

  -------------------------------------------------------------------------
  -- One clustering step
  -------------------------------------------------------------------------
  step :: Double -> [Cluster] -> [Cluster]
  step x cs = add2Cluster x (findCluster x cs) cs

  -------------------------------------------------------------------------
  -- Find the cluster to which a data point belongs
  -------------------------------------------------------------------------
  findCluster :: Double -> [Cluster] -> Cluster
  findCluster x [] = error "search on empty clustering"
  findCluster x cs = let ds = map (clusterDist x) cs
                      in case fish (minimum ds) cs ds of
                           []    -> error "ouch! No cluster found!"
                           (h:_) -> h

  -------------------------------------------------------------------------
  -- Add a data point to a cluster and recompute the clustering 
  -------------------------------------------------------------------------
  add2Cluster :: Double -> Cluster -> [Cluster] -> [Cluster]
  add2Cluster x _ [] = error "add to empty clustering"
  add2Cluster x c cs = let c'  = ins x c
                           fs  = farest c'
                        in migrate fs c' (c' : delete c cs)

  -------------------------------------------------------------------------
  -- Recompute clustering 
  -------------------------------------------------------------------------
  migrate :: [Double] -> Cluster -> [Cluster] -> [Cluster]
  migrate []     _ cs = cs
  migrate (f:fs) c cs = let c' = findCluster f cs 
                         in if c' == c then migrate fs c cs
                              else let cs' = (del f c) : delete c cs
                                    in migrate fs c (add2Cluster f c' cs')

  -------------------------------------------------------------------------
  -- Little helper that searches the minimum, maximum, etc.
  -------------------------------------------------------------------------
  fish :: (Eq a) => a -> [b] -> [a] -> [b]
  fish x us ds = map fst $ filter flt $ zip us ds 
    where flt (_,d) = d == x
