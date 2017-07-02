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
  gammae i t = f * product [e**(t/n) * (1+(t/n))**(-1) | n <- [1..m]]
    where f = e**((-1)*gamma*t)/t
          m = fromIntegral (i-1)

  ------------------------------------------------------------------------
  -- Gamma Function according to product formula
  ------------------------------------------------------------------------
  gammal :: Int -> Double -> Double
  gammal i t = (1/t) * product [(1+1/n)**t / (1+t/n) | n <- [1..m]]
    where m = fromIntegral i

  ------------------------------------------------------------------------
  -- Computing pi
  ------------------------------------------------------------------------
  gammapi :: Int -> Double
  gammapi i = gammal i 0.5 * gammal i 0.5

  ------------------------------------------------------------------------
  -- halves of odds
  ------------------------------------------------------------------------
  gammaho :: Integer -> Double
  gammaho 1 = sqrt pi
  gammaho n | even n = error "not an odd number!"
            | otherwise = rff (n-2) / 2**i * sqrt pi
    where rff = fromIntegral . facfac
          i   = fromIntegral ((n-1) `div` 2)

  ------------------------------------------------------------------------
  -- real binomial coefficients
  ------------------------------------------------------------------------
  chooser :: Int -> Double -> Double -> Double
  chooser i n k = gammal i (n+1) / 
                 (gammal i (k+1) * gammal i (n-k+1))

  ------------------------------------------------------------------------
  -- The standard defintion of the Gamma Function
  -- make use of the Euler integral of the second kind:
  -- Integral(0..inf) x^(t-1)e^(-x)dx
  ------------------------------------------------------------------------
  gammas :: Double -> Double
  gammas = undefined

