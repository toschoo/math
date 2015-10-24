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

  t7 :: [[String]]
  t7 = [
    ["1 % 1","1 % 2", "1 % 3", "1 % 4","1 % 5","1 % 6", "7 % 6"], 
    ["1 % 1","2 % 1", "3 % 1", "4 % 1","5 % 1","6 % 1", "7 % 1"]]

  t10 :: [[String]]
  t10 = [
    ["1 % 4", "5 % 4","5 % 9", "14 % 9", "23 % 9", "23 % 32", "55 % 32"]] 


  main :: IO ()
  main = do
    img <- GD.newImage (1500,1000) -- should be defined by command line!
    GD.fillImage white img
    let reg = stdRegion img (1430,930)
    let gr  = stdGraph reg{regBase = (35,35)}
    os <- getArgs
    case os of
      [n] -> case n of
               "7"   -> seven  gr
               "8.1" -> eight1 gr
               "8.2" -> eight2 gr
               "sb7" -> sterb7 gr
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

  eight2 :: Graph -> IO ()
  eight2 = undefined
