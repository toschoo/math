module ECCrypto 
where

  import Elliptic
  import Modular hiding (mul,add)

  import System.Random (randomRIO)
  import Control.Applicative ((<$>))
  import Control.Concurrent
  import Control.Monad
  import Data.Bits (xor)
  import Prelude hiding (mod)

  ------------------------------------------------------------------------
  -- Diffie-Hellman
  ------------------------------------------------------------------------
  data ECDHParams = ECDHP Curve Point 

  ecdhRandomPrivate :: ECDHParams -> IO Integer
  ecdhRandomPrivate (ECDHP c g) = randomRIO (2,o-1)
    where o = gorder c g 

  ecdhPublic :: ECDHParams -> Integer -> Point
  ecdhPublic (ECDHP c g) d = mul c d g

  ecdhInit :: ECDHParams -> IO (Integer, Point)
  ecdhInit ps@(ECDHP c g) = do
    k <- ecdhRandomPrivate ps
    return (k, ecdhPublic ps k)

  alice :: ECDHParams -> PFun -> Chan Point -> Chan Point -> IO ()
  alice ps@(ECDHP c g) sfp ich och = do
    (d,qa) <- ecdhInit ps
    sfp ("Alice: private key is " ++ show d)
    writeChan och qa
    qb <- readChan ich -- check if (x,y) is on the curve 
                       -- or even better: reduce y to a 1 or 0 bit:
                       -- just send (x,0) (e.g. plus/minus root).
                       -- check order (do no accept small order!)
    let k = mul c d qb
    sfp ("Alice: common key is " ++ show k)
    -- send bob a secret
    m <- randomRIO (1,18) :: IO Integer
    sfp ("Alice: sending " ++ show m)
    sfSend och k m
    threadDelay 1000000

  bob :: ECDHParams -> PFun -> Chan Point -> Chan Point -> IO ()
  bob ps@(ECDHP c g) sfp ich och = do
    (d,qb) <- ecdhInit ps
    sfp ("Bob  : private key is " ++ show d)
    qa     <- readChan ich
    writeChan och qb
    let k = mul c d qa
    sfp ("Bob  : common key is " ++ show k)
    -- recevie secret
    m <- sfRead ich k 
    sfp ("Bob  : received: " ++ show m)
    threadDelay 1000000

  eve :: PFun -> Chan Point -> Chan Point -> Chan Point -> Chan Point -> IO ()
  eve sfp aich aoch bich boch = do
    a <- readChan aoch
    sfp ("Eve  : Alice to Bob: " ++ show a)
    writeChan bich a
    b <- readChan boch
    sfp ("Eve  : Bob to Alice: " ++ show b)
    writeChan aich b
    m <- readChan aoch
    sfp ("Eve  : Alice to Bob: " ++ show m)
    writeChan bich m
    threadDelay 1000000

  type PFun = String -> IO ()

  put :: MVar () -> String -> IO ()
  put m s = withMVar m (\_ -> putStrLn s)

  ecdhDemo :: IO ()
  ecdhDemo = do
    aich <- newChan
    aoch <- newChan
    bich <- newChan
    boch <- newChan
    m    <- newMVar ()
    let sfp = put m
    let ecdhp = ECDHP (Curve 2 2 17) (P 5 1)
    void $ forkIO (eve sfp aich aoch bich boch)
    void $ forkIO (alice ecdhp sfp aich aoch)
    void $ forkIO (bob   ecdhp sfp bich boch)
    threadDelay 5000000

  sfSend :: Chan Point -> Point -> Integer -> IO ()
  sfSend ch p m = writeChan ch (ecdhEncrypt p m)

  sfRead :: Chan Point -> Point -> IO Integer
  sfRead ch p = (ecdhDecrypt p) <$> readChan ch

  ecdhEncrypt :: Point -> Integer -> Point
  ecdhEncrypt (P x y) m = P (xor (x+y) m) 0

  ecdhDecrypt :: Point -> Point -> Integer
  ecdhDecrypt (P x y) (P m _) = xor (x+y) m
     
  ------------------------------------------------------------------------
  -- EC Digital Signature Algorithm
  -- Requirement: the order of point q in parameters must be prime
  ------------------------------------------------------------------------
  data ECDSAParams  = ECDSA Curve Point 
  type ECDSAKeyPair = (Integer,Point)
  type ECDSASig     = (Integer,Integer)

  ecdsaPrivate :: ECDSAParams -> IO Integer
  ecdsaPrivate (ECDSA c q) = randomRIO(2,o-1)
    where o = gorder c q -- must be prime!

  ecdsaPublic :: ECDSAParams -> Integer -> Point
  ecdsaPublic (ECDSA c q) d = mul c d q

  ecdsaKeyPair :: ECDSAParams -> IO ECDSAKeyPair
  ecdsaKeyPair ps = do
    d <- ecdsaPrivate ps
    return (d, ecdsaPublic ps d)

  ecdsaEphem :: ECDSAParams -> IO Integer
  ecdsaEphem (ECDSA c q) = randomRIO(2,o-1)
    where o = gorder c q  -- must be prime!

  ecdsaSign :: ECDSAParams -> ECDSAKeyPair -> Integer -> IO ECDSASig
  ecdsaSign ps@(ECDSA c q) (d,a) m = do
    k <- ecdsaEphem ps
    let r  = xco (mul c k q) -- since k < o, this is not O!
    let k' = inverse k o
    let s1 = (m+d*r) `mod` o -- this may be 0 mod p!
    let s  = (s1*k') `mod` o
    if  s1 == 0 then ecdsaSign ps (d,a) m
                else return (r,s)
    where o = gorder c q -- must be prime!

  ------------------------------------------------------------------------
  -- Verify 
  ------------------------------------------------------------------------
  -- looks complicated, but it is quite easy
  --   Sign computes
  --
  --     s = (m+dr)k' mod o
  --
  --   This is
  --
  --   ks = m+dr       mod o | *k
  --   k  = ms' + drs' mod o | *s'
  --
  --   but:
  --   u = ms' and v = rs'
  --   so, we have:
  --
  --   k = u + vd mod o
  --
  --   Note that q is a primitive element of the group.
  --   We, hence, can multiply q with both sides of the equation:
  --
  --   kq = uq + vdq
  --
  --   dq is the public key a. We therefore have
  --
  --   kq = uq + va
  -- 
  --   kq is the point whose x coordinate is r. 
  ------------------------------------------------------------------------
  ecdsaVerify :: ECDSAParams -> Point -> Integer -> ECDSASig -> Bool
  ecdsaVerify ps@(ECDSA c q) a m (r,s) = x == r 
    where o  = gorder c q        -- must be prime
          s' = inverse s o
          u  = (s' * m) `mod` o
          v  = (s' * r) `mod` o
          x  = (xco p)  `mod` o
          p  = add c (mul c u q)
                     (mul c v a)

  ------------------------------------------------------------------------
  -- Test with curve c1 and generator p1
  ------------------------------------------------------------------------
  ecdsaTest :: Bool -> Integer -> IO Bool
  ecdsaTest verbose m = do
    let params = ECDSA c1 p1
    (priv,pub) <- ecdsaKeyPair params
    when verbose (do
      putStrLn $ "private: " ++ show priv
      putStrLn $ "public : " ++ show pub)
    sig        <- ecdsaSign params (priv,pub) m
    when verbose (putStrLn $ "sig    : " ++ show sig)
    return (ecdsaVerify params pub m sig)

