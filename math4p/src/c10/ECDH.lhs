\ignore{
\begin{code}
module ECDH
where
  import Random
  import ECModulo
  import Prelude hiding (mod)
  import Prime
  import Modular hiding (add,mull,mDiv)
\end{code}
}

Diffie-Hellman on elliptic curves is
very similar to Diffie-Hellman on 
the group of remainders of a prime.
We just change the group operation.
Everything else remains (more or less) the same.
We start by defining the group parameters:

\begin{minipage}{\textwidth}
\begin{code}
  data ECDHParams = ECDHP Curve Point 
\end{code}
\end{minipage}

That is, the parameters we need to establish
the protocol in the first place are the curve
we are using and a starting point, which is
a generator of the group of the curve.
Now we define a function to generate a random
private key:

\begin{minipage}{\textwidth}
\begin{code}
  ecdhRandomPrivate :: ECDHParams -> IO Natural
  ecdhRandomPrivate (ECDHP c g) = randomNat (2,o-1)
    where o = gorder c g 
\end{code}
\end{minipage}

In plain English, we select a random number
between two and the size of the group minus one.
From this we can compute the public key,
\ie, the point that will be exchange over the
unsecure channel:

\begin{minipage}{\textwidth}
\begin{code}
  ecdhPublic :: ECDHParams -> Natural -> Point
  ecdhPublic (ECDHP c g) d = mul c d g
\end{code}
\end{minipage}

The public key, hence, is the multiplication
of the start point, |g|, by a numer |d|,
which is the number we have just chosen 
randomly from the range $2\dots o-1$.
We put this into a convenient protocol
initialisation routine:

\begin{minipage}{\textwidth}
\begin{code}
  ecdhInit :: ECDHParams -> IO (Natural, Point)
  ecdhInit ps@(ECDHP c g) = do
    k <- ecdhRandomPrivate ps
    return (k, ecdhPublic ps k)
\end{code}
\end{minipage}

Before we present the communication
between Alice and Bob, we define a set
of concurrent printing function: 

\begin{minipage}{\textwidth}
\begin{code}
  type PFun = String -> IO ()
  put :: MVar () -> String -> IO ()
  put m s = withMVar m (\_ -> putStrLn s)
\end{code}
\end{minipage}

As you can see, the function locks
an |MVar| before writing to $stdout$.
The intention is to avoid ending up
with jumbled strings printed to $stdout$.

We further implement two pairs of functions that
together establish a secure channel
on top of an unsure one using the 
common key established by the Diffie-Hellman
key exchange protocol. Note that these
functions are not part of Diffie-Hellman itself.
We, in fact, use a quite stupid encryption
just for illustration purpose:

\begin{minipage}{\textwidth}
\begin{code}
  ecdhEncrypt :: Point -> Natural -> Point
  ecdhEncrypt (P x y) m = P (xor (x+y) m) 0

  ecdhDecrypt :: Point -> Point -> Natural
  ecdhDecrypt (P x y) (P m _) = xor (x+y) m
\end{code}
\end{minipage}

The first function encrypts a message 
(represented as a natural number) using
a key, which is a point. The encryption
is just an |xor| of the message using
the sum of the coordinates of the point.
Decryption consists in |xor|ing the cipher,
again, with the sum of the point coordinates.
(This, certainly, is not a good encryption
 algorithm -- but, finally, this is not
 a cryptography tutorial!)

The next pair of functions use the cryptographic
functions above to send a message through
the otherwise unsecure channel:

\begin{minipage}{\textwidth}
\begin{code}
  sfSend :: Chan Point -> Point -> Integer -> IO ()
  sfSend ch p m = writeChan ch (ecdhEncrypt p m)

  sfRead :: Chan Point -> Point -> IO Integer
  sfRead ch p = (ecdhDecrypt p) <$> readChan ch
\end{code}
\end{minipage}

We now implement Alice, the one who
initiates the protocol:

\begin{minipage}{\textwidth}
\begin{code}
  alice :: ECDHParams -> PFun -> Chan Point -> Chan Point -> IO ()
  alice ps@(ECDHP c g) sfp ich och = do
    (d,qa) <- ecdhInit ps
    sfp ("Alice: private key is " ++ show d)
    writeChan och qa
    qb <- readChan ich 
    let k = mul c d qb
    sfp ("Alice: common key is " ++ show k)
    m <- randomNat (1,18) :: IO Natural
    sfp ("Alice: sending " ++ show m)
    sfSend och k m
    threadDelay 1000000
\end{code}
\end{minipage}

This function receives the |ECDHParams|, 
the concurrent printing function
and two channels, one outgoing and the other incoming.
Alice starts by initiating the protocol
receiving the secret, |d| and the public key, |qa|
(standing for point q multiplied by alice's secret).
Alice prints the secret to $stdout$, so we can follow
what is happening.
She then sends the public key, which is a point,
through the outgoing channel (|och|) and waits
on the incoming channel (|ich|) for Bob's answer.
From the answer, she creates the joint secret
by multiplying Bob's point (|qb|) with her own secret |d|.
She prints it to $std$ for us to see and then creates
a random number, which is the message to be protected
by the common secret, prints the message to $stdout$ and
sends it encrypted by the common secret, |k|, 
through the unsecure channel (which is just the
outgoing channel used before).
Finally, we delay the task for a second
to give all players the time to finish their business.

Here is what Bob does:

\begin{minipage}{\textwidth}
\begin{code}
  bob :: ECDHParams -> PFun -> Chan Point -> Chan Point -> IO ()
  bob ps@(ECDHP c g) sfp ich och = do
    (d,qb) <- ecdhInit ps
    sfp ("Bob  : private key is " ++ show d)
    qa     <- readChan ich
    writeChan och qb
    let k = mul c d qa
    sfp ("Bob  : common key is " ++ show k)
    m <- sfRead ich k 
    sfp ("Bob  : received: " ++ show m)
    threadDelay 1000000
\end{code}
\end{minipage}

Bob initialises the protocol, obtaining
a secret key and a public key, prints
the secret key to $stdout$ and waits
for Alice to start the protocol.
At some point he receives Alice's
public key, |qa|, and then sends his own
public key, |qb|.
He computes the common secret $k$
and prints it to $stdout$ for us to see.
The he waits for the encrypted message
on the now secured channel and writes it
to $stdout$.
Finally, we delay the task for a second
to give all players the time to finish their business.

Of course, we also need the evesdropper Eve:

\begin{minipage}{\textwidth}
\begin{code}
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
\end{code}
\end{minipage}

Alice is a \term{woman-in-the-middle} and
holds all channels, those used by Alice and
those holds by Bob. The communication means
used by Bob's and Alice's channels, therefore,
is unsecure. Alice, now, reads Alice channel,
prints what she sees, and sends the message
as it is to Bob. Then she waits for Bob's
response. When it arrives, she again prints
what she sees and sends it to Alice.
She now waits for Alice message, which will
be encrypted by our simple |xor|ing encryption
function. She again prints what she sees,
sends it to Bob and delay execution for a second
for the others to terminate their stuff.

