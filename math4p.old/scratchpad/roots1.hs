
import Poly

solve :: IO ()
solve = do
  showSolution $ fromTerms [(2,1.0)]
  showSolution $ fromTerms [(2,3.0),(1,5),(0,2)]
  showSolution $ fromTerms [(2,3.0),(1,6),(0,3)]
  showSolution $ fromTerms [(2,1.0),(1,2),(0,5)]

showSolution :: Poly Double -> IO ()
showSolution p =
  putStrLn $ toString p ++ ": " ++ show (roots p) 
