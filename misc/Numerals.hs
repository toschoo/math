module Numerals
where

  ------------------------------------------------------------------------
  -- Convert from dec to any
  ------------------------------------------------------------------------
  toBaseN :: Integer -> Integer -> [Int]
  toBaseN b = reverse . go
    where go x = case x `quotRem` b of
                   (0, r) -> [fromIntegral r]
                   (q, r) -> (fromIntegral r) : go q

  ------------------------------------------------------------------------
  -- Convert from any to dec
  ------------------------------------------------------------------------
  fromBaseN :: Integer -> [Int] -> Integer
  fromBaseN b = go 0 . map fromIntegral . reverse
    where go _ []     = 0
          go x (r:rs) = r*(b^x) + go (x+1) rs 

  ------------------------------------------------------------------------
  -- Convert from dec to binary
  ------------------------------------------------------------------------
  toBinary :: Integer -> [Int]
  toBinary = toBaseN 2

  ------------------------------------------------------------------------
  -- Convert from binary to dec
  ------------------------------------------------------------------------
  fromBinary :: [Int] -> Integer
  fromBinary = fromBaseN 2

  ------------------------------------------------------------------------
  -- Invert the interior bits
  ------------------------------------------------------------------------
  binverse :: [Int] -> [Int]
  binverse [] = []
  binverse bs = head bs : go (tail bs)
    where go []  = []
          go [x] = [x]
          go (0:xs) = 1 : go xs
          go (1:xs) = 0 : go xs

  ------------------------------------------------------------------------
  -- Find numbers with n digits and k digits /= 0
  ------------------------------------------------------------------------
  chooseNums :: Integer -> Integer -> [Integer]
  chooseNums n k = go 1
    where go x | x > 2^n            = []
               | has k (toBinary x) = x : go (x+1)
               | otherwise          =     go (x+1)
          has 0 [] = True
          has _ [] = False
          has i (x:xs) | x == 1 && i == 0 = False
                       | x == 1      = has (i-1) xs
                       | otherwise   = has i xs

