module Main
where

  import qualified Graphics.GD as GD
  import           Data.Ratio 
  import           Control.Applicative ((<$>))
  import           System.Environment

  import           Cantor
  import           Draw

  jpgf :: String
  jpgf = "test.jpg"

  pngf :: String
  pngf = "test.png"

  t5 :: [[String]]
  t5 = [
    ["1 % 1","1 % 2", "1 % 3", "4 % 3","7 % 3"], 
    ["1 % 1","1 % 2", "3 % 2", "5 % 2","7 % 2"], 
    ["1 % 1","2 % 1", "2 % 3", "2 % 5","7 % 5"], 
    ["1 % 1","2 % 1", "3 % 1", "3 % 4","7 % 4"]]

  t8 :: [[String]]
  t8 = [
    ["1 % 1","1 % 2", "1 % 3", "1 % 4","1 % 5", "6 % 5", "11 % 5", "11 % 16", "11 % 27", "38 % 27", "38 % 65"], 
    ["1 % 1","1 % 2", "1 % 3", "4 % 3","7 % 3", "10 % 3", "10 % 13", "23 % 13", "23 % 36", "23 % 59", "82 % 59"], 
    ["1 % 1","1 % 2", "3 % 2", "5 % 2","7 % 2", "9 % 2", "11 % 2" , "13 % 2", "15 % 2", "17 % 2", "19 % 2"], 
    ["1 % 1","1 % 2", "3 % 2", "3 % 5","8 % 5", "13 % 5", "13 % 18" , "13 % 31", "13 % 44", "57 % 44", "57 % 101"], 
    ["1 % 1","2 % 1", "2 % 3", "2 % 5","7 % 5", "7 % 12", "7 % 19", "7 % 26", "7 % 33", "7 % 40", "7 % 47"], 
    ["1 % 1","2 % 1", "2 % 3", "5 % 3","5 % 8", "5 % 13", "5 % 18", "23 % 18", "23 % 41", "64 % 41", "64 % 105"], 
    ["1 % 1","2 % 1", "3 % 1", "3 % 4","7 % 4", "11 % 4", "11 % 15", "11 % 26", "11 % 37", "48 % 37", "48 % 85"]]

  t7 :: [[String]]
  t7 = [
    ["1 % 1","1 % 2", "1 % 3", "1 % 4","1 % 5","1 % 6", "7 % 6"], 
    ["1 % 1","2 % 1", "3 % 1", "4 % 1","5 % 1","6 % 1", "7 % 1"]]

  t10 :: [[String]]
  t10 = [
    ["1 % 4", "5 % 4","5 % 9", "14 % 9", "23 % 9", "23 % 32", "55 % 32"]] 

  t12 :: [[String]]
  t12 = [
    ["1 % 1", "2 % 1", "2 % 3", "2 % 5", "2 % 7", "9 % 7", "9 % 16", "25 % 16",
     "25 % 41", "66 % 41", "66 % 107", "173 % 107"]] 


  main :: IO ()
  main = do
    img <- GD.newImage (3118,1600) -- should be defined by command line!
    -- img <- GD.newImage (3500,3000) -- should be defined by command line!
    GD.fillImage black img
    let reg = stdRegion img (2048,1530)
    -- let reg = stdRegion img (3430,2930)
    let gr  = stdGraph reg{regBase = (35,35)}
    os <- getArgs
    case os of
      [n] -> case n of
               "7"   -> seven  gr
               "8.1" -> eight1 gr
               "8.2" -> eight2 gr
               "sb7" -> sterb7 gr
               "x"   -> eightx gr
               _     -> seven  gr
      _   -> seven gr

  seven :: Graph -> IO ()
  seven gr0 = do
    let gr1 = tree2graph gr0 7 $ {- numerator <$> -} calwiTree 12 (1%1)
    let gr2 = markN t7 green gr1
    let gr3 = markN t5 red   gr2
    drawGraph 10 gr3
    savePng "seven.png" gr3

  sterb7 :: Graph -> IO ()
  sterb7 gr0 = do
    let gr = tree2graph gr0 5 $ {- numerator <$> -} sterbrocTree 12
    drawGraph 12 gr
    savePng "sterb7.png" gr

  eight1 :: Graph -> IO ()
  eight1 gr0 = do
    let gr1 = tree2graph gr0 7 $ {- numerator <$> -} calwiTree 12 (1%4)
    let gr2 = markN t10 red   gr1
    drawGraph 10 gr2
    savePng "eight1.png" gr2

  eightx :: Graph -> IO ()
  eightx gr0 = do
    let gr1 = tree2graph gr0 11 $ calwiTree 12 (1%1)
    let gr2 = markN t8 blue gr1
    drawGraph 6 gr2
    savePng "x.png" gr2

  eight2 :: Graph -> IO ()
  eight2 = undefined
