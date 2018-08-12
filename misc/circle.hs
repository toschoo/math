------------------------------------------------------------------------
-- Draw a circle according to Bresenham's midpoint algorithm
------------------------------------------------------------------------
module Main
where

  import qualified Graphics.GD as GD
  import           System.Environment
  import           System.Random (randomRIO)
  import           Control.Applicative ((<$>))
  import           Control.Monad (when)

  ------------------------------------------------------------------------
  -- This is all given and would be parameters in a library
  ------------------------------------------------------------------------
  width, height :: Int
  width  = 1000
  height = 1000

  white,black :: GD.Color
  white = GD.rgb 255 255 255
  black = GD.rgb   0   0   0
  
  main :: IO ()
  main = do
    os <- getArgs
    case os of
      [o] -> drawCircle o (500,500) 100
      _   -> error "I need an output filename!"

  ------------------------------------------------------------------------
  -- Create the image > draw the circle > store the image
  ------------------------------------------------------------------------
  drawCircle :: FilePath -> GD.Size -> Int -> IO ()
  drawCircle o (x,y) r = do
    img <- GD.newImage (width,height)
    GD.fillImage white img
    drawCircleIn img (x,y) r
    GD.saveJpegFile (-1) o img 

  ------------------------------------------------------------------------
  -- kick off the algorithm
  -- we start on the y-axis (x=0, y=r)
  -- h is initialised to 1 - r, which is
  -- f(x+1, y-1/2) - r^2, i.e.
  -- f(1, r-1/2) - r^2  = 1 - r + 1/4 = 5/4 - r
  -- which we round down to 1 - r
  ------------------------------------------------------------------------
  drawCircleIn :: GD.Image -> GD.Size -> Int -> IO ()
  drawCircleIn img (mx,my) r = do
    let x = 0 
    let y = r 
    let h = 1 - r
    drawIt img (mx,my) (x,y) h

  ------------------------------------------------------------------------
  -- Draw the points
  ------------------------------------------------------------------------
  drawIt :: GD.Image -> GD.Size -> GD.Size -> Int -> IO ()
  drawIt img (mx,my) (x,y) h = do

    -- we draw 8 points, one in the segment
    GD.setPixel (px (mx,my) (x,y)) black img -- 12:00 to 01:30
    GD.setPixel (px (mx,my) (y,x)) black img -- 01:30 to 03:00
    GD.setPixel (px (mx,my) (y,-x)) black img -- 03:00 to 04:30
    GD.setPixel (px (mx,my) (x,-y)) black img -- 04:30 to 06:00
    GD.setPixel (px (mx,my) (-x,-y)) black img -- 06:00 to 07:30
    GD.setPixel (px (mx,my) (-y,-x)) black img -- 07:30 to 09:00
    GD.setPixel (px (mx,my) (-y,x)) black img -- 09:00 to 10:30
    GD.setPixel (px (mx,my) (-x,y)) black img -- 10:30 to 12:00

    -- draw the next point
    when (y > x) $ do
      let ((x', y'), h') = nextPoint (x,y) h
      drawIt img (mx,my) (x',y') h'
    
  ------------------------------------------------------------------------
  -- compute the next point:
  -- if h < 0 then the mid point is inside the circle, so we go east
  -- otherwise, it is outside the circle and we go south-east
  -- the new values for h are computed as:
  -- h < 0: (x+2)^2 + (y-1/2)^2 - (x+1)^2 + (y-1/2)^2
  -- else : (x+2)^2 + (y-3/2)^2 - (x+1)^2 + (y-1/2)^2
  ------------------------------------------------------------------------
  nextPoint :: GD.Size -> Int -> (GD.Size, Int)
  nextPoint (x,y) h | h < 0     = ((x+1, y), h+2*x+3)
                    | otherwise = ((x+1, y-1), h+2*(x-y)+5)

  ------------------------------------------------------------------------
  -- compute the pixel (the left upper corner = 0,0)
  ------------------------------------------------------------------------
  px :: GD.Size -> GD.Size -> GD.Size
  px (mx,my) (x,y) = (x+mx, (my-y))
  
