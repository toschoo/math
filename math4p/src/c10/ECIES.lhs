\ignore{
\begin{code}
module ECIES
where
  import Random
  import ECModulo
  import Prelude hiding (mod)
  import Prime
  import Modular hiding (add,mull,mDiv)
  import ECDH
\end{code}
}
The \acronym{ec} Integrated Encryption Scheme,
\acronym{ecies}, is much more complex, but also
much more complete than the \acronym{ecdh}.
Mathematically, however, it is very similar.
It differs from \acronym{ecdh} mainly in what
is defined on top of the mathematical basis.

In concrete terms, \acronym{ecies} defines
a complete encryption scheme as part of its 
parameters. Just as \acronym{ecdh}, it defines
in its parameters the curve, the primitive element
from where we start. Additionally, it defines
a public key (which in Diffie-Hellman is computed
randomly as part of the key exchange session),
an algorithm to enrich the secret that is derived
from the public key, so it can be used in a
symmetric encryption function, this encryption
function as such and an authentication scheme.
Since we, here, focus on the mathematical aspects,
we simplify this parameter set a bit. We are not
interested in the key enrichment and, for the moment,
we are not interested in authentication.
We therefore present the \acronym{ecies} parameters
in the much simpler form:

\begin{minipage}{\textwidth}
\begin{code}
  data ECIESParams = ECIES Curve Point Point CryptoF
\end{code}
\end{minipage} 

and define |CryptoF| as

\begin{minipage}{\textwidth}
\begin{code}
  type CryptoF = Natural -> Natural -> Natural
\end{code}
\end{minipage} 

It represents an encryption function that receives
a key and a message (both natural numbers) and uses
the key to transform the message into a third
natural number, the ciphertext. A simple example
for such a crypto function is again |xor|.

We now have two keys, 
the public key known to everybody and
the private key known only to the owner of the key,
just as in traditional \acronym{rsa}.
The key generation, hence, is not part of the protocol.
Keys are generated in advance and the public key
is made available to people that might want to communicate
to the owner of the key pair, say Bob.

So, at some point in time, Bob decides 
he wants to use encryption for part of his communication
and, to this end, he creates a key pair:

\begin{minipage}{\textwidth}
\begin{code}
  eciesKeyPair :: Curve -> Point -> IO (Natural,Point)
  eciesKeyPair c g = do
    p <- randomNatural (2,o-1)
    let k = mul c p g
    return (p,k)
    where o = gorder c g
\end{code}
\end{minipage} 

The key generation function |eciesKeyPair|
receives a part of the parameters, namely
the curve and the primitive element, and
generates by means of this input a natural number,
the private key, and a point, the public key.
The key generation itself is just as in \acronym{ECDH}:
we generate a random number $p$ in the range of 
the order of the underlying group
and multiply the generator $g$ by this number.
The number $p$ is the private key and the resulting point
is the public key.

Based on this key generator function,
we can define a function that creates us the parameters:

\begin{minipage}{\textwidth}
\begin{code}
  eciesMakeParams :: Curve -> Point -> CryptoF -> IO (Natural,ECIESParams)
  eciesMakeParams c g f = do
    (p,k) <- eciesKeyPair c g
    return (p,ECIES c g k f)
\end{code}
\end{minipage} 

The function receives a curve, a point (the generator) and
a crypto function and yields a natural number (the private key)
and the parameters. How it does this, is straight forward.

Now, we look at encryption.
To encrypt a message to Bob, Alice would use Bob's public key
to derive a common secret. This step is very similar to 
\acronym{ECDH}. The difference is that the public key
is known beforehand.
Here is a function for Alice to derive a secrete using
Bob's public key:

\begin{minipage}{\textwidth}
\begin{code}
  eciesSecret :: ECIESParams -> IO (Natural,Point)
  eciesSecret ps@(ECIES c g k _) = do
    r <- randomNatural (2,o-1)
    let  p = mul c r g
    let  q = mul c r k
    if   q == O  then eciesSecret ps
                 else return (xco q,p)
    where o = gorder c g
\end{code}
\end{minipage}

Alice starts by, again, selecting a random number
in the order of the group.
She multiplies this number with the generator 
to obtain point $p$ and multiplies it with
Bob's public key to obtain point $q$.
If $q$ is the identity, she tries again.
(Note that $p$ cannot be the identity,
 since $r$ is in the order of the group
 and $g$ is a primitive element).
Otherwise, she returns the $x$-coordinate of $q$
(the product of $r$ and Bob's public key)
and the point $p$ (the product of $r$ and the generator).

Here is how she uses the secret to encrypt a message $m$:

\begin{minipage}{\textwidth}
\begin{code}
  eciesEncrypt :: ECIESParams -> Natural -> IO (Point,Natural)
  eciesEncrypt ps@(ECIES c g k f) m = do
    (s,p) <- eciesSecret ps
    return (p,f s m)
\end{code}
\end{minipage}

She starts by creating the secret |(s,p)|, where
$s$ is the $x$-coordinate of $q$ above and $p$ is
just the point $p$ already returned by the secret function.
She returns $p$ and |f s m|, where |f| is the encryption function,
$s$, the secret, and $m$, the message to be encrypted.

Now, what does Bob do with this stuff to decrypt the message?
Here it is:

\begin{minipage}{\textwidth}
\begin{code}
  eciesDecrypt :: ECIESParams -> Natural -> (Point,Natural) -> Natural
  eciesDecrypt (ECIES c _ _ f) k (p,cm) = let s = xco (mul c k p) in f s cm 
\end{code}
\end{minipage}

For decryption, he needs the parameters,
his own private key and the tuple |(Point,Natural)| generated by Alice.
Now, he does the following:
He multiplies the private key ($k$) with the point $p$.
When we go back, we see that this point $p$
resulted from multiplying the primitive element $g$ by a random number $r$.
The public key, however, is also a product of a random number, namely,
Bob's private key, and the generator.
The secret, $s$ was generated by multiplying Bob's public key
by $r$. When Bob multiplies the point $p$ with his private key $k$,
he hence derives the same point.

To make that a bit clearer, let us adopt a better terminology.
We will write points with capital letters and natural numbers
in small letters. We then have 
$G$, the primitive element,
$K$, Bob's public key, 
$P$, a point generated by Alice and
$Q$, another point generated by Alice.
We also have
$k$, Bob's private key and
$r$, a random number generated by Alice.

Alice computes:
$P=rG$ and $Q=rK$. The $x$-coordinate of the latter is the shared secret.
$K$, Bob's public key is $kG$.
$Q$, hence, is $Q=rkG$.
Bob, when decrypting computes $kP$, where $P=rG$.
He, hence, computes $krG$ and,
since our group is associative and commutative, that is just $Q$
whose $x$-coordinate is the shared secret.

Here comes a simple testing function
that puts all the bits together.
The function uses the previously defined curve 
$c1$ with generator $p1$:

\begin{minipage}{\textwidth}
\begin{code}
  eciesTest :: Bool -> Natural -> IO Bool
  eciesTest verbose m = do
    (pri,ps@(ECIES _ _ pub _)) <- eciesMakeParams c1 p1 xor
    when verbose (do
      putStrLn $ "private: " ++ show pri
      putStrLn $ "public : " ++ show pub)
    (p,cipher) <- eciesEncrypt ps m
    when verbose (do
      putStrLn $ "cipher : " ++ show cipher
      putStrLn $ "p      : " ++ show p)
    return (eciesDecrypt ps pri (p,cipher) == m)
\end{code}
\end{minipage}

