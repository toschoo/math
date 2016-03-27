\ignore{
\begin{code}
module ECDSA
where
  import Random
  import ECModulo
  import Prelude hiding (mod)
  import Prime
  import Modular hiding (add,mull,mDiv)
  import ECDH
\end{code}
}
The \acronym{ec} Digital Signature Algorithm,
\acronym{ecdsa}, is less complex than \acronym{ecies}
in terms of parameters. But it is mathematically
much more interesting.
The objective of this algorithm is to provide
message authentication, \ie\ a scheme to
sign and verify messages.
As \acronym{ecies}, it uses a pair of
a private and a public key.
The private key is used for signing
and the public key is used to verify the signature,
just as in \acronym{rsa}.

We start to describe the math of signing and verification.
We have the following components (besides the usual
parameters curve $c$ and generator $G$):
a random number $r$ called the ephemeral key,
a point $P=rG$ and its $x$-coordinate $a$.
The private key $k$, a number, 
the public key $K$, a point computed as $kG$ 
and the message $m$.
The signature consists of the pair $(a,s)$.
We compute $s$ as

\begin{equation}
s \equiv (m+ak)r' \pmod{o},
\end{equation}

where $r'$ is the inverse of $r$ in the group
established by $o$ the order of the group $G$,
which itself must be a prime number.

The challenge for verification is
to compute $P=rG$ and to compare the result
with the $x$-coordinate $a$ of $P$ without
having access to the private key $k$.
To achieve this, we transform the equation above
to get rid of $k$.
We start by multiplying by $r$ 
on both sides of the equation: 

\begin{equation}
rs \equiv (m+ak) \pmod{o}.
\end{equation}

We then multiply by the inverse of $s$:

\begin{equation}
r \equiv ms'+as'k \pmod{o}.
\end{equation}

Now we multiply the generator $G$ on both sides of this equation.
Note that the values that appear in the equation are modulo $o$,
which is the order of the group generated by $G$ and, hence,
the result is still in the group of the elliptic curve.

\begin{equation}
rG=ms'G+as'kG.
\end{equation}

$kG$, the product of private key and generator, however, 
is just the public key.
We can thus simplify to

\begin{equation}
rG=ms'G+as'K.
\end{equation}

All the values on the right-hand sight of the last equation are known
without knowing the private key.
$G$ is the generator, which is part of the parameters;
$m$ is the message to be verified;
$s'$ is the inverse of the second element 
of the signature, which can be computed
using the order of the group generated by $G$.
$a$, finally,
is the first part of the signature. 
We, hence, can compute $rG$ as $ms'G + as'K$.
$rG$, however, is the point, $P$, 
from which the $x$-coordinate $a$ was taken.
If the $x$-coordinate of the result of our computation
equals $a$, the signature is correct. 
Otherwise, it is a forgery.

Let us put the mathematics into code.
We start, as usual, with the parameters:

\begin{minipage}{\textwidth}
\begin{code}
  data ECDSAParams = ECDSA Curve Point Point
\end{code}
\end{minipage} 

The parameters consists of the curve, 
the generator and the public key.
Next, we define a function to generate
the key pair:

\begin{minipage}{\textwidth}
\begin{code}
  ecdsaKeyPair :: Curve -> Point -> IO (Natural,Point)
  ecdsaKeyPair c g = do
    k <- randomNatural (2,o-1)
    return (k, mul c k g)
    where o = gorder c g
\end{code}
\end{minipage}

This is nothing new.
We generate a random number
in the order of the group of the elliptic curve
and multply the generator by this number.
The number is the private key, $k$, and the
resulting point is the public key, which
we will call $q$ in the following.

Here comes the function that creates the key pair
and the parameters:

\begin{minipage}{\textwidth}
\begin{code}
  ecdsaMakeParams :: Curve -> Point -> IO (Natural,ECDSAParams)
  ecdsaMakeParams c g = do
    (k,q) <- ecdsaKeyPair c g
    return (k, ECDSA c g q)
\end{code}
\end{minipage} 

Now, we implement the sign function:

\begin{minipage}{\textwidth}
\begin{code}
  ecdsaSign :: ECDSAParams -> Natural -> Natural -> IO (Natural,Natural) 
  ecdsaSign ps@(ECDSA c g q) k m = do
    r <- randomNatural(2,o-1)
    if s1 == 0  then ecdsaSign ps k m
                else return (a,s)
    where  o   = gorder c q 
           a   = xco (mul c r g) 
           r'  = inverse r o
           s1  = (m+k*a)  `mod` o 
           s   = (s1*r')  `mod` o
\end{code}
\end{minipage} 

We start by creating the so called ephemeral key,
a random number $r$ in the order of the group.
We generate a point and get its $x$-coordinate $a$.
We then get the inverse of $r$ in the group $o$.
Note that we treat the numbers in this range
as the remainders of a prime number $o$.
The order of the group, therefore, must be a prime number.

We now compute $s$ in two steps.
First, we compute $m+ak$, the message added to the product
of the private key and the $a$ we just computed modulo $o$.
We then multiply the result by $r'$, the inverse of $r$.
Should $s1$ be a multiple of $o$, we try again with another $r$.
Note that, if $s1$ is not a multiple of $o$, then $s1 \times r'$
is not a multiple of $o$ either, since $r'$ is from the
remainder group of $o$. If $o$ is a prime, this number and $o$
do not share divisors and there is not way to get a multiple of $o$
by multiplying by another number that does not share divisors with $o$.
Otherwise, we return the pair $(a,s)$.
This is the signature.

Verification:

\begin{minipage}{\textwidth}
\begin{code}
  ecdsaVerify :: ECDSAParams -> (Natural,Natural) -> Natural -> Bool
  ecdsaVerify ps@(ECDSA c g q) (a,s) m = x == a
    where  o   = gorder c g
           s'  = inverse s o
           u   = (s' * m)  `mod` o
           v   = (s' * a)  `mod` o
           x   = xco p
           p   = add c  (mul c u g)
                        (mul c v q)
\end{code}
\end{minipage}

The function receives the paramters,
the signature pair and the message.
It computes the inverse of $s$ and two
variables $u$ and $v$ as
$u = ms'$ and $v = as'$,
both modulo $o$.
Multiplying the generator by $u$ results in the point $ms'G$;
multiplying the public key $q$ by $v$ results in the point $as'K$. 
Their sum $ms'G + as'K$ equals $rG$, the point whose 
$x$-coordinate is $a$.
Finally, we compare the result $x$ with $a$.
The comparison verifies the signature.

Here is a test function that brings all the bits together:

\begin{minipage}{\textwidth}
\begin{code}
  ecdsaTest :: Bool -> Natural -> IO Bool
  ecdsaTest verbose m = do
    (k,ps) <- ecdsaMakeParams c1 p1
    when verbose (do
      putStrLn $ "private: " ++ show k
      putStrLn $ "public : " ++ show q)
    sig  <- ecdsaSign ps k m
    when verbose (
      putStrLn $ "sig    : " ++ show sig)
    return (ecdsaVerify ps sig m )
\end{code}
\end{minipage} 
