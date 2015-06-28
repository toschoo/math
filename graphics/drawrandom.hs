module Main
where

  import qualified Graphics.GD as GD
  import           System.Environment
  import           System.Random (randomRIO)

  width, height :: Int
  width  = 250 -- 2100
  height = 290 -- 2900

  white,black :: GD.Color
  white = GD.rgb 255 255 255
  black = GD.rgb   0   0   0
  
  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [fn] -> drawImg fn
      _    -> error "I need a filename!"

  drawImg :: String -> IO ()
  drawImg fn = do
    img <- GD.newImage (width,height)
    GD.fillImage white img
    fillRandom img
    GD.saveJpegFile (10) fn img

  fillRandom :: GD.Image -> IO ()
  fillRandom img = go 10000
    where go :: Integer -> IO ()
          go 0 = return ()
          go n = do
            x <- randomRIO (0,width)
            y <- randomRIO (0,height)
            GD.setPixel (x,y) black img
            -- GD.drawFilledEllipse (x,y) (5,5) black img
            go (n-1)

