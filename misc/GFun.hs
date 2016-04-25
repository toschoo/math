module GFun
where
  import Perm
  import Binom
  import ConwayGuy

  ------------------------------------------------------------------------
  -- Gamma Function for Integers
  ------------------------------------------------------------------------
  gamman :: Integer -> Integer
  gamman 1 = 1
  gamman n | n <= 0    = undefined
           | otherwise = x*gamman x
    where x = n-1

  ------------------------------------------------------------------------
  -- Gamma Function according to e product formula
  ------------------------------------------------------------------------
  gammae :: Int -> Double -> Double
  gammae i t = f * go 1 (fromIntegral i)
    where f = e**((-1)*gamma*t)/t
          go n m | n > m     = 1
                 | otherwise = e**(t/n)
                               * (1+(t/n))**(-1)
                               * go (n+1) m

  ------------------------------------------------------------------------
  -- Gamma Function according to product formula
  ------------------------------------------------------------------------
  gammal :: Int -> Double -> Double
  gammal i t = (1/t) * go 1 (fromIntegral i)
    where go n m | n > m = 1
                 | otherwise = (1+1/n)**t / (1+t/n)
                               * go (n+1) m

  ------------------------------------------------------------------------
  -- Computing pi
  ------------------------------------------------------------------------
  gammapi :: Int -> Double
  gammapi i = gammal i 0.5 * gammal i 0.5

  ------------------------------------------------------------------------
  -- real binomial coefficients
  ------------------------------------------------------------------------
  chooser :: Int -> Double -> Double -> Double
  chooser i n k = gammal i (n+1) / 
                 (gammal i (k+1) * gammal i (n-k+1))

  ------------------------------------------------------------------------
  -- The standard defintion of the Gamma Function
  -- make use of integrals:
  -- Integral(0..inf) x^(t-1)e^(-x)dx
  ------------------------------------------------------------------------
  gammas :: Double -> Double
  gammas = undefined

