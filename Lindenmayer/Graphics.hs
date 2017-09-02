module Lindenmayer.Graphics where
  
  import qualified Graphics.SOE       as SOE
  import qualified Lindenmayer.Common as Com 
  import qualified Lindenmayer.Config as Config
  import qualified Lindenmayer.Model  as Mod
  import qualified Lindenmayer.System as Sys
        
  -----------------------------------------------------------------------
  -- Draw a system using config
  -----------------------------------------------------------------------
  drawSystem :: Config.Config -> IO ()
  drawSystem c = SOE.runGraphics $ 
    let s   = (Config.sys c)
        m   = (Config.mod c)
        g   = (Config.grf c)
        md  = case Mod.getModel (Config.mname m) of
              Nothing   -> error ("Unknown Model: " ++ (Config.mname m))
              Just mod' -> mod'
        o   = (Config.om s)
        r   = (Config.rule s)
     in do w <- SOE.openWindowEx (Config.sname s) Nothing 
                                 (Just $ Config.wSize g) 
                                 SOE.drawBufferedGraphic
           waitForInput w 0 md (o, r) 
                        ((Config.startP g),
                         (Config.length g),
                         (Config.angle  g),
                         (Config.comp   g)) 
           SOE.closeWindow w

  -----------------------------------------------------------------------
  -- Draw one turn and wait for user input
  -----------------------------------------------------------------------
  waitForInput :: SOE.Window -> Int -> Mod.Model -> 
                  (Com.Omega, [Com.Rule]) -> 
                  (Com.Point, Int, Int, Double) -> IO ()
  waitForInput w n model (omega, rules) (point, len, angle, comp) 
    | n < 0     = return ()
    | otherwise =
        let s  | n == 0    = [omega]
               | otherwise = Sys.lindenmayer omega rules n
            l' | n == 0    = len 
               | otherwise = floor $ fromIntegral len / 
                                     (comp ** fromIntegral n)
            s' = s !! n 
            ls = model s' point (l', angle)
         in do SOE.clearWindow w
               SOE.drawInWindow w (SOE.text (10, 10) (show n))
               drawLines w ls
               k <- getKey w
               let i | (l' <= 0) = n
                     | otherwise = case k of 
                       'q'  -> -1
                       'b'  -> if n == 0 then 0 else n - 1
                       ' '  -> n + 1
                       '\r' -> n + 1
                       _    -> n
               waitForInput w i model (omega, rules) 
                                      (point, len, angle, comp)

  getKey :: SOE.Window -> IO Char
  getKey w = do e <- SOE.getWindowEvent w
                case e of
                  SOE.Key k _    -> return k
                  _              -> getKey w

  -----------------------------------------------------------------------
  -- Draw lines from a set of lines
  -----------------------------------------------------------------------
  drawLines :: SOE.Window -> [Com.Line] -> IO ()
  drawLines w = mapM_ (SOE.drawInWindow w . uncurry (SOE.line))

