import Poly

test1 :: IO ()
test1 = do
  let p1 = fromTerms [(5,2.0),(4,3.0),(3,0.0),(2,1.0),(1,5.0),(0,15.0)]
  let p2 = derivative p1
  -- let p3 = padd p1 $ fromTerms [(-1,3)]
  putStrLn $ "integral of " ++ show p2 ++ " is: "
  print $ integrate p2

testIntFactors :: IO ()
testIntFactors = do
  testIntFactor $ fromTerms [(1,5),(0,10)]
  testIntFactor $ fromTerms [(2,1),(1,2),(0,1)]
  testIntFactor $ fromTerms [(2,1),(1,4),(0,4)]
  testIntFactor $ fromTerms [(2,1),(1,8),(0,16)]
  testIntFactor $ fromTerms [(2,1),(0,-1)]
  testIntFactor $ fromTerms [(2,3),(0,-3)]
  testIntFactor $ fromTerms [(2,5),(1,20),(0,20)]
  testIntFactor $ fromTerms [(2,3),(1,5),(0,5)]
  testIntFactor $ fromTerms [(2,1),(1,2),(0,4)]

testIntFactor :: Poly Int -> IO ()
testIntFactor p1 = do
  let p2 = intFactors p1
  putStrLn $ "factors of " ++ show p1 ++ ": " ++ show p2
