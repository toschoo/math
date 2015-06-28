module Sierpinski where

  ------------------------------------------------------------------------
  -- Sierpinski Triangle Demonstrator
  ------------------------------------------------------------------------

  import qualified SOE as SOE

  -- a point -------------------------------------------------------------
  type Point = (Int, Int)

  -- a triangle ----------------------------------------------------------
  type Triangle = (Point, Point, Point)

  -- window size ---------------------------------------------------------
  wSize :: Point
  wSize = (1200, 700)

  -- initial triangle size -----------------------------------------------
  tSize :: Point
  tSize = (700, 700)

  -- get x of a point -----------------------------------------------------
  getX :: Point -> Int
  getX p = fst p

  -- get y of a point -----------------------------------------------------
  getY :: Point -> Int
  getY p = snd p
    
  ------------------------------------------------------------------------
  -- get the initiator triangle
  ------------------------------------------------------------------------
  initiator :: Triangle
  initiator = 
    let x1 = (getX tSize) `div` 2 -- we start in the middle 
                                  -- of the triangle's width 
        y1 = 0                    -- at the top of the window

        x2 = 0           -- second point is left lower corner 
        y2 = getY tSize  -- of the triangle's region
        x3 = getX tSize  -- third point is right lower corner
        y3 = getY tSize  -- of the triangle's region
     in ((x1, y1), (x2, y2), (x3, y3))

  ------------------------------------------------------------------------
  -- generate all triangles up to iteration i
  -- starting from triangle t (the initiator)
  ------------------------------------------------------------------------
  generator :: Triangle -> Int -> [Triangle]
  generator _ 0 = [] -- stop when we reached the final iteration step
  generator (p1, p2, p3) i = 
    let x1 = getX p1 -- first point is the one
        y1 = getY p2 -- one the bottom

        -- the second is half the way up on the left side
        x2 = (getX p1) - ((getX p1) - (getX p2)) `div` 2
        y2 = (getY p1) + ((getY p2) - (getY p1)) `div` 2

        -- the third is half the way up on the right side
        x3 = (getX p1) + ((getX p3) - (getX p1)) `div` 2
        y3 = y2

        i' = i - 1 -- next iteration

    -- result is this new triangle concatenated
    -- to the triangles of the next iteration
    in ((x1, y1), (x2, y2), (x3, y3)) :
       ((generator (p1,       (x2, y2), (x3, y3)) i') ++ 
        (generator ((x2, y2), p2,       (x1, y1)) i') ++ 
        (generator ((x3, y3), (x1, y1), p3      ) i')) 

  ------------------------------------------------------------------------
  -- IO: draw a triangle in window w
  ------------------------------------------------------------------------
  drawTriangle :: SOE.Window -> Triangle -> IO ()
  drawTriangle w (p1, p2, p3) = do
    SOE.drawInWindow w (SOE.line p1 p2)
    SOE.drawInWindow w (SOE.line p2 p3)
    SOE.drawInWindow w (SOE.line p3 p1)

  ------------------------------------------------------------------------
  -- IO: draw a list of triangles in window w
  ------------------------------------------------------------------------
  drawTriangles :: SOE.Window -> [Triangle] -> IO ()
  drawTriangles w = mapM_ (drawTriangle w) 

  ------------------------------------------------------------------------
  -- IO: Wait for user event:
  --     q              - quit
  --     enter or space - next iteration
  --     backspace      - previous iteration
  --                      if iteration is not 0
  --     other inputs are ignored
  --     - clear window
  --     - indicate the iteration in the window
  --     - create the triangle up to the iteration selected by the user
  --     - draw all triagnles
  --     - repeat
  ------------------------------------------------------------------------
  waitForInput :: SOE.Window -> Int -> Triangle -> IO ()
  waitForInput w i t = do
    k <- getKey w
    let i' = case k of
               'q'  -> -1
               '\8' -> if i == 0 then 0 else i - 1
               ' '  -> i + 1
               '\r' -> i + 1
               _    -> i
    if i' < 0 then return ()
      else do
        let ts = generator t i' 
        SOE.clearWindow w
        SOE.drawInWindow w $ SOE.text (10, 10) (show i')
        drawTriangles w (t:ts)
        waitForInput  w i' t

  getKey :: SOE.Window -> IO Char
  getKey w = do e <- SOE.getWindowEvent w
                case e of
                  SOE.Key k _    -> return k
                  _              -> getKey w

  ------------------------------------------------------------------------
  -- IO: Start demonstration
  ------------------------------------------------------------------------
  drawSierpinski :: IO ()
  drawSierpinski = SOE.runGraphics $ do
    w <- SOE.openWindow ("The Sierpinski Triangle") wSize
    let t = initiator 
    drawTriangle w t
    SOE.drawInWindow w $ SOE.text (10, 10) "0"
    waitForInput w 0 t
    SOE.closeWindow w
        
