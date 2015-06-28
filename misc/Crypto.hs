module Crypto
where

  import System.Random (randomRIO)
  import Control.Applicative ((<$>))
  import Control.Concurrent
  import Control.Monad (void,forever,unless)
  import Data.Bits (xor)
  import Prelude hiding (mod)
  import Prime
  import Modular

  -------------------------------------------------------------------------
  -- Common
  -------------------------------------------------------------------------
  generatePrime :: Integer -> IO Integer
  generatePrime k = do n <- randomRIO (2^(k-1),2^k-1)
                       t <- rabinMiller 64 n
                       if t then return n 
                            else generatePrime k

  safePrime :: Integer -> IO Integer
  safePrime k = generatePrime (k-1) >>= \q -> let p = 2*q + 1 in do
                  t <- rabinMiller 64 p
                  if t then return p
                       else safePrime k

  -------------------------------------------------------------------------
  -- Diffie-Hellman
  -------------------------------------------------------------------------
  generator :: Integer -> IO Integer
  generator p = do a <- randomRIO (2,p-2)
                   case (a^2) `rem` p of
                     1 -> generator p
                     g -> if g == p - 1 
                            then generator p
                            else if legendre g p == 1 
                                 then return g
                                 else generator p
                  

  generatePublic :: Integer -> IO (Integer,Integer)
  generatePublic k = do p <- safePrime k
                        g <- generator p
                        return (p,g)

  generatePrivate :: Integer -> Integer -> IO Integer
  generatePrivate p g = do x <- randomRIO (2,p-2)
                           let gx = g^x
                           if gx `rem` p == p - 1 ||
                              legendre gx p /= 1  
                             then generatePrivate p g
                             else return x

  data DHData = DH {
                   dhIn    :: Chan Integer,
                   dhOut   :: Chan Integer,
                   dhPrime :: Integer,
                   dhGen   :: Integer,
                   dhPart  :: Integer,
                   dhKey   :: Integer}

  
  dhDemo :: Integer -> IO ()
  dhDemo k = do
    (p,g) <- generatePublic k
    ach   <- newChan
    bch   <- newChan
    ech   <- newChan 
    m     <- newMVar ()
    let sfPrint = put m
    void $ forkIO (alice ach ech sfPrint p g)
    void $ forkIO (bob   bch ech sfPrint p g)
    eve ech ach bch sfPrint

  put :: MVar () -> String -> IO ()
  put m s = withMVar m $ \_ ->  putStrLn s

  eve :: Chan Integer -> Chan Integer -> Chan Integer -> (String -> IO ()) -> IO ()
  eve inch ach bch sfp = do
    m1 <- readChan inch
    sfp $ "Eve  : from Alice: " ++ show m1
    writeChan bch m1
    m2 <- readChan inch
    sfp $ "Eve  : from Bob  : " ++ show m2
    writeChan ach m2 
    m3 <- readChan inch
    sfp $ "Eve  : from Bob  : " ++ show m3
    writeChan ach m3
    threadDelay 2000000
    
  alice :: Chan Integer -> Chan Integer -> (String -> IO ()) -> 
           Integer -> Integer -> IO ()
  alice inch outch sfp p g = do
    dh <- initProtocol inch outch p g
    sfp $ "Alice: key is " ++ show (dhKey dh) ++ " (" ++ show (dhPart dh) ++ ")"
    m  <- sfRead dh
    sfp $ "Alice: message received: " ++ show m
    threadDelay 1000000

  bob :: Chan Integer -> Chan Integer -> (String -> IO ()) -> 
         Integer -> Integer -> IO ()
  bob inch outch sfp p g = do
    dh <- acceptProtocol inch outch p g
    sfp $ "Bob  : key is " ++ show (dhKey dh) ++ " (" ++ show (dhPart dh) ++ ")"
    threadDelay 1000000
    m  <- randomRIO (10,1000)
    sfp $ "Bob  : sending secret message " ++ show m
    sfSend dh m
    threadDelay 1000000
    
  initProtocol :: Chan Integer -> Chan Integer -> Integer -> Integer -> IO DHData
  initProtocol inch outch p g = do
    x  <- randomRIO (2,p-2) -- generatePrivate p g
    let gx = (g^x) `mod` p
    writeChan outch gx
    gy <- readChan inch
    unless (checkG p gy) $ error ("suspicious G: " ++ show p ++ ", " ++ show g ++ ": " ++ show gy)
    let k = (gy^x) `mod` p
    return DH {
             dhIn    = inch,
             dhOut   = outch,
             dhPrime = p,
             dhGen   = g,
             dhPart  = gx,
             dhKey   = k}
    
  acceptProtocol :: Chan Integer -> Chan Integer -> Integer -> Integer -> IO DHData
  acceptProtocol inch outch p g = do
    gx <- readChan inch
    unless (checkG p gx) $ error ("suspicious G: " ++ show p ++ ", " ++ show g ++ ": " ++ show gx)
    y  <- generatePrivate p g
    let k  = (gx^y) `rem` p
    let gy = (g^y)  `rem` p
    writeChan outch gy
    return DH {
             dhIn    = inch,
             dhOut   = outch,
             dhPrime = p,
             dhGen   = g,
             dhPart  = y,
             dhKey   = k}

  checkG :: Integer -> Integer -> Bool
  checkG p g = g /= 1 && legendre g p == 1

  sfSend :: DHData -> Integer -> IO ()
  sfSend dh m = writeChan (dhOut dh) (dhEncrypt dh m)

  sfRead :: DHData -> IO Integer
  sfRead dh = (dhDecrypt dh) <$> readChan (dhIn dh)

  dhEncrypt :: DHData -> Integer -> Integer
  dhEncrypt dh = xor (dhKey dh)

  dhDecrypt :: DHData -> Integer -> Integer
  dhDecrypt dh = xor (dhKey dh)
                   
  -------------------------------------------------------------------------
  -- RSA
  -------------------------------------------------------------------------
  type PublicK  = (Integer, Integer)
  type PrivateK = (Integer,Integer,Integer,Integer)

  pubN, pubE :: PublicK -> Integer
  pubN = fst
  pubE = snd

  privP, privQ, privT, privD :: PrivateK -> Integer
  privP (p,_,_,_) = p
  privQ (_,q,_,_) = q
  privT (_,_,t,_) = t
  privD (_,_,_,d) = d

  generateKeys :: Integer -> IO (PublicK,PrivateK)
  generateKeys k | k < 10    = error "Key size too small" 
                 | k > 100   = error "Key size too big"
                 | otherwise = do 
                      p <- genPrime 1
                      q <- genPrime p
                      let t = lcm (p-1) (q-1)
                      pair <- findED 1000 t
                      case pair of
                        Nothing    -> generateKeys k
                        Just (e,d) -> if e == d || 
                                         e == p || e == q ||
                                         d == p || d == q 
                                        then generateKeys k
                                        else let pub  = (p*q,e)
                                                 priv = (p,q,t,d)
                                              in return (pub,priv)
    where genPrime p = do q <- generatePrime (k `div` 2) -- safePrime
                          if p == q then genPrime p 
                                    else return q

  findED :: Integer -> Integer -> IO (Maybe (Integer,Integer))
  findED 0 _  = return Nothing
  findED i t  = randomRIO (7,t-2) >>= \x ->
    let z | even x    = x-1
          | otherwise = x 
        (e,d)         = tryXgcd z t
     in if (e*d) `mod` t == 1 then if e > t `div` 2 then return (Just (d,e)) 
                                                    else return (Just (e,d))
                              else findED (i-1) t
    where tryXgcd a t = let (g,(u,_)) = xgcd a t 
                         in if g == 1 then (a,u `mod` t)
                                      else (0,0) 

  encrypt :: PublicK -> Integer -> Integer
  encrypt k m = (m^(pubE k)) `mod` (pubN k)

  decrypt :: PublicK -> PrivateK -> Integer -> Integer
  decrypt pu pr c = (c^(privD pr)) `mod` (pubN pu)

  sign :: PublicK -> PrivateK -> Integer -> Integer
  sign = decrypt 

  authenticate :: PublicK -> Integer -> Integer -> Bool
  authenticate k c m = encrypt k c == m
  
