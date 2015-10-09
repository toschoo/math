module Main
where

  import           Clustering
  import qualified Graphics.GD as GD
  import           System.Environment
  import           System.Random
  import           Control.Applicative ((<$>))

  white,black :: GD.Color
  white = GD.rgb 255 255 255
  black = GD.rgb   0   0   0
  red   = GD.rgb 255   0   0
  green = GD.rgb   0 255   0
  blue  = GD.rgb   0   0 255

  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [f] -> do
        putStrLn "-----------------------------------"
        putStrLn " Clustering smoke tests..."
        putStrLn "-----------------------------------"
        ps <- makePoints 100
        let seed = map (\(l,v) -> mkMean l v) $ 
                       zip ["blue","green","red"] $ take 3 ps
        let [c1,c2,c3] = optimiseN 10 $ part (drop 3 ps) seed
        img <- GD.newImage (1000,1000)
        GD.fillImage white img
        drawCluster img blue  c1
        drawCluster img green c2
        drawCluster img red   c3
        GD.saveJpegFile 90 f img
        putStrLn $ "Blue : " ++ show (centroid c1) 
        putStrLn $ "Green: " ++ show (centroid c2) 
        putStrLn $ "Red  : " ++ show (centroid c2) 
        putStrLn "ready"

  drawCluster :: GD.Image -> GD.Color -> KMean -> IO ()
  drawCluster img col c = 
    let vs = [(round $ head v,round $ head $ tail v) | v <- cPop c]
     in mapM_ (\v -> GD.drawFilledEllipse v (5,5) col img) vs
    
  prettyCluster :: Cluster c => c -> String
  prettyCluster c = 
    label c ++ ":\n" 
            ++ "  "  ++ show (population c)
            ++ "\n\n"

  makePoints :: Int -> IO [[Double]]
  makePoints i = do
    xs <- makeCoords i
    ys <- makeCoords i
    return (merge xs ys)

  makeCoords :: Int -> IO [Double]
  makeCoords 0 = return []
  makeCoords i = do
    d  <- fromIntegral <$> (randomRIO (1,1000)::IO Int)
    (d:) <$> makeCoords (i-1)

  merge :: [a] -> [a] -> [[a]]
  merge [] _ = []
  merge _ [] = []
  merge (x:xs) (y:ys) = [x,y] : merge xs ys
    
