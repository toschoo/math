module ECCrypto 
where

  import Elliptic

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
    return (k, mul c k g)

  alice :: ECDHParams -> PFun -> Chan Point -> Chan Point -> IO ()
  alice ps@(ECDHP c g) sfp ich och = do
    (d,qa) <- ecdhInit ps
    sfp ("Alice: private key is " ++ show d)
    writeChan och qa
    qb <- readChan ich
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
    let ecdhp = ECDHP (Curve 2 2 17) (P (5,1))
    void $ forkIO (eve sfp aich aoch bich boch)
    void $ forkIO (alice ecdhp sfp aich aoch)
    void $ forkIO (bob   ecdhp sfp bich boch)
    threadDelay 5000000

  sfSend :: Chan Point -> Point -> Integer -> IO ()
  sfSend ch p m = writeChan ch (ecdhEncrypt p m)

  sfRead :: Chan Point -> Point -> IO Integer
  sfRead ch p = (ecdhDecrypt p) <$> readChan ch

  ecdhEncrypt :: Point -> Integer -> Point
  ecdhEncrypt (P (x,y)) m = P (xor (x+y) m,0)

  ecdhDecrypt :: Point -> Point -> Integer
  ecdhDecrypt (P (x,y)) (P (m,_)) = xor (x+y) m
     
