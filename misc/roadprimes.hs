module Main
where 

  import System.Environment
  import System.Exit
  import Data.Char

  ldp :: Integer -> Integer
  ldp  = ldpf primes 

  ldpf :: [Integer] -> Integer -> Integer
  ldpf (p:ps) n | rem n p == 0 = p
                | p^2 > n      = n
                | otherwise    = ldpf ps n

  primes :: [Integer]
  primes = 2:filter prime [3..]

  prime :: Integer -> Bool
  prime n | n < 1     = error "Not a positive integer!"
          | n == 1    = False
          | otherwise = ldp n == n

  main :: IO ()
  main = do
    os <- getArgs
    case getFilter os of
      Left s    -> putStrLn ("ERROR: " ++ s) >> exitFailure
      Right flt -> mapM_ (putStrLn . show) $ flt primes

  getFilter :: [String] -> Either String ([Integer] -> [Integer])
  getFilter [] = Right id
  getFilter [""] = Left "Empty argument"
  getFilter ("":_) = Left "Empty argument"
  getFilter (o:_) = case head o of
                      '-'  -> Right (take (fromIntegral (toNumber $ tail o)))
                      '='  -> Right (takeWhile (<= (toNumber $ tail o)))
                      _    -> Left "Invalid argument"
    where toNumber "" = 0
          toNumber n  | all isAlphaNum n = read n
                      | otherwise        = 0
