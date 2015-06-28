module Main
where

  import qualified Graphics.GD as GD
  import           System.Environment
  import           System.Random (randomRIO)
  import           Control.Applicative ((<$>))
  import           Control.Monad (when)

  width, height :: Int
  width  = 2100
  height = 2900

  white,black :: GD.Color
  white = GD.rgb 255 255 255
  black = GD.rgb   0   0   0
  
  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [i,o] -> copyImg i o
      _     -> error "I need an input and an output filename!"

  copyImg :: FilePath -> FilePath -> IO ()
  copyImg i o = do
    img <- GD.loadJpegFile i
    s   <- GD.imageSize img
    ps  <- readPixels img s
    print $ s
    writePixels o s ps

  readPixels :: GD.Image -> GD.Size -> IO [[GD.Color]]
  readPixels img (w,h) = mapM (readRow w) [0..h]
    where readRow w y = mapM (
            (\x -> {- isWhite <$> -} GD.getPixel (x,y) img)) [0..w]
          isWhite c | c == black = 0 -- '\NUL'
                    | otherwise  = 1 -- '\SOH'

  writePixels :: FilePath -> GD.Size -> [[GD.Color]] -> IO ()
  writePixels o (w,h) ps = do
    img <- GD.newImage (w,h)
    GD.fillImage white img
    -- fillRandom img
    rndPix img ps (w,h)
    GD.saveJpegFile (-1) o img -- output name

  rndPix :: GD.Image -> [[GD.Color]] -> GD.Size -> IO ()
  rndPix img ps (w,h) = drawRows 0 ps
    where -- drawRows :: Int -> [[Int]] -> IO ()
          drawRows _ [] = return ()
          drawRows y (c:cs) = drawRow 0 y c >> drawRows (y+1) cs
          -- drawRow :: Int -> Int -> [Int] -> IO ()
          drawRow _ _ [] = return ()
          drawRow x y (c:cs) = putPixel (x,y) c >> drawRow (x+1) y cs
          putPixel (x,y) c = do
            t <- decide 
            when t $ GD.setPixel (x,y) c img

  decide :: IO Bool
  decide = do
    i <- randomRIO (0,1) :: IO Int
    return (i > 0)

  drawImg :: String -> IO ()
  drawImg fn = do
    img <- GD.newImage (width,height)
    GD.fillImage white img
    fillRandom img
    GD.saveJpegFile (10) fn img

  fillRandom :: GD.Image -> IO ()
  fillRandom img = go 1000000
    where go :: Integer -> IO ()
          go 0 = return ()
          go n = do
            x <- randomRIO (0,width)
            y <- randomRIO (0,height)
            GD.setPixel (x,y) black img
            -- GD.drawFilledEllipse (x,y) (5,5) black img
            go (n-1)

