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

Diffie-Hellman on elliptic curves (\acronym{ecdh}) 
is very similar to Diffie-Hellman on 
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
which we can exchange over the unsecure channel:

\begin{minipage}{\textwidth}
\begin{code}
  ecdhPublic :: ECDHParams -> Natural -> Point
  ecdhPublic (ECDHP c g) d = mul c d g
\end{code}
\end{minipage}

The public key, hence, is a point, namely
the result of the multiplication
of the start point, |g|, by a number |d|,
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

Before we go on to present the communication
between Alice and Bob, we define a 
concurrent printing function that should
help us inspecting what is going on between
the two of them: 

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
on top of an unsecure one. The key we are using 
to secure the channel is the result of the 
Diffie-Hellman key exchange protocol. Note that these
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

Note that the encryption result is a point,
not a number. This has no further significance.
We just do that, because we want to use the
same channels for exchanging messages that
were used for establishing the key.
Since these channels are defined as |Chan Point|,
the cipher sent through this channel must be
a point too. Indeed, the $y$-coordinate of
the cipher is just 0. It has no meaning whatsover.

The next pair of functions use the cryptographic
functions above to send a message 
(which is just a natural number) through
the otherwise unsecure channel:

\begin{minipage}{\textwidth}
\begin{code}
  sfSend :: Chan Point -> Point -> Natural -> IO ()
  sfSend ch p m = writeChan ch (ecdhEncrypt p m)

  sfRead :: Chan Point -> Point -> IO Natural
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
    m <- randomNat (1,o-1) :: IO Natural
    sfp ("Alice: sending " ++ show m)
    sfSend och k m
    threadDelay 1000000
    where o = gorder c g
\end{code}
\end{minipage}

This function receives the |ECDHParams|, 
the concurrent printing function
and two channels, one outgoing and the other incoming.
Alice starts by initialising the protocol
creating her secret, |d| and her public key, |qa|
(standing for point q multiplied by alice's secret).
Alice prints the secret to $stdout$, so we can follow
what is happening.

She then sends the public key, which is a point,
through the outgoing channel (|och|) and waits
on the incoming channel (|ich|) for Bob's answer.
From the answer, she creates the joint secret
by multiplying Bob's point (|qb|) by her own secret |d|.
She prints it to $stdout$ for us to see and then creates
a random number, which is the message to be protected
by the common secret. Note that, due to our simplistic
encryption function, the message must be a number 
in the group we are happening to use.
This is not realistic of course.

Alice prints the message to $stdout$ and
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
Then he waits for the encrypted message
on the now secured channel and writes it
to $stdout$.
Finally, we delay the task for a second
to give all players the time to finish their business.

Of course, we also need the evesdropper Eve:

\begin{minipage}{\textwidth}
\begin{code}
  eve :: PFun ->  Chan Point -> Chan Point -> 
                  Chan Point -> Chan Point -> IO ()
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

Eve is a \term{woman-in-the-middle} and
holds all channels, those used by Alice and
those used by Bob. That this is possible 
means that these channels are unsecure.
Alice and Bob cannot rely on them when
communicating confidential messages between them,
such as love letters that should be kept
away from their parents; or explosive news
kept from the public by the authoritarian government
that rules the country where Alice and Bob live.

Eve reads Alice channel,
prints what she sees, and sends the message
as it is to Bob. Then she waits for Bob's
response. When it arrives, she again prints
what she sees and sends it to Alice.
She now waits for Alice message, which will
be encrypted by our simple |xor|ing encryption
function. She again prints what she sees,
sends it to Bob and delays execution for a second
for the others to terminate their stuff.

The point here is that Eve can see everything
that is on the channel. But she cannot guess
what the common secret key is Alice and Bob
are using. She sees two numbers, the public
key of Alice and that of Bob. But she does 
not know the factors |d| that were used by them
to generate those numbers. 
To know them, she would need to solve the \acronym{ecdlp},
which is hard for huge numbers.
Alice and Bob, on the other hand, compute the shared
secret by computing $a\times qb$ and $b\times qa$,
respectively. Since $qb$, Bob's public key,
is $b\times q$ and $qa$, Alice's public key,
is $a\times q$, we end up with the computations
$a\times b\times q$ and $b\times a\times q$.
These computations, however, 
since group operations are associative,
result in the same number.

Here is a demo program that puts all the pieces together:

\begin{minipage}{\textwidth}
\begin{code}
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
\end{code}
\end{minipage}

Since the protocol is not deterministic,
but uses random numbers, the output of this program
will vary between calls.
A possible output is:

\begin{verbatim}
Alice: private key is 13
Bob  : private key is 2
Eve  : Alice to Bob: (16,4)
Bob  : common key is (0,6)
Eve  : Bob to Alice: (6,3)
Alice: common key is (0,6)
Alice: sending 11
Eve  : Alice to Bob: (13,0)
Bob  : received: 11
\end{verbatim}

The first line is Alice to reveal her secret key to us,
which is 13. Then Bob tells us his secret, which is 2.
Alice now sends her public key to Bob. This message
is intercepted by Eve. She sees the point $(16,4)$.
Indeed, $13\times (5,1)$ in the curve |Curve 2 2 17| is
$(16,4)$.

In the next line we see a triumphant Bob, revealing
the shared secret $(0,6)$, that he computed as 
$2\times (16,4)$. Now Bob sends his public key $(6,3)$ to Alice,
which is observed by Eve. Note that $2\times (5,1)$ is indeed
$(6,3)$. In the next line, Alice has computed the shared secret
and it is of course equal to Bob's result, 
since $13\times (6,3) = (0,6)$.

Alice now uses the shared secret to send the secret message,
11, through the wire. Eve sees the point $(13,0)$.
The $x$-coordinate of this point, 13, is the result of
$xor$ing $0+6$ and 11, 
since $6_{10} = 110_2$ and 
$11_{10} = 1011_2$. When we $xor$, we compute

\begin{center}
\begin{tabular}{||c||c||c||c||}\hline
0 & 1 & 1 & 0\\
1 & 0 & 1 & 1\\\hline
1 & 1 & 0 & 1\\\hline
\end{tabular}
\end{center}

and $1101_2 = 13_{10}$.
As we can see in the last line,
Bob has received the correct message 11.

