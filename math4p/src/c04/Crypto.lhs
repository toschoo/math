\ignore{
\begin{code}
module Crypto
where
  import System.Random (randomRIO)
  import Control.Applicative ((<$>))
  import Control.Monad (unless)
  import Control.Concurrent
  import Data.List (nub)
  import Natural
  import Fact
  import Modular
  import Residues
  import Tests
\end{code}
}
\ignore{$}

To say this right at the beginning:
this is not an introduction to cryptography!
We will go through some examples
of how primes are used in cryptography, in particular
the Diffie-Hellman  key exchange protocol and 
\acronym{rsa}. But we will not discuss
pitfalls, common errors or other issues
you have to take care of when implementing
cryptosystems. If you want to learn about
cryptography, you definitely
have to study the literature on the topic.
Cryptography poses very hard engineering challenges
and we are far from adressing them here.
Again: this is not an introduction to cryptography!
We do not even use a big number library
that would be able to cope with numbers
with hundreds or thousands of digits.
When you use the algorithms presented here
(like the Rabin-Miller test)
on numbers of that size, you will probably
have to wait minutes or even hours for results.
Raising a number of thousands of digits
to another number of thousands of digits
requests special handling.
You cannot do that with our |Natural| data type
or even the Haskell |Integer| data type.
So, once again: this is not an introduction
to cryptography.

Primes come into play in cryptography, typically,
in very special applications.
Usually, to encrypt and decrypt a message,
secret keys are used that are known to both
sides of the communication, traditionally called
Alice and Bob in the literature.
The algorithms to convert the plain message
into the \term{cyphertext} have in most cases
nothing to do with primes, but are rather
combinations of basic operations like \acronym{xor}
and \term{bit shuffle} involving the original message
and the key material.

The weakest link in this kind of cryptography is 
the key itself. All parties that take part 
in the secure communication must know the key
and, in consequence, the key must be shared among them
in a secure way.
How to share the key safely is indeed a major challenge.
For the German submarines in World War II,
just to name a popular example,
the loss of a codebook was at least as challenging
as the cryptoanalysis by British specialists
at Bletchley Park.
A new codebook could hardly be distributed among
all submarines on the ocean in due time.

The challenge is an instance of the bootstrapping problem:
we have to start a process, namely secure communication,
without having the means to run this process,
namely secret keys distributed among all parties.
One solution is to use publicly available information
to convert a plain message into a cyphertext
by means of a \term{one-way function},
\ie\ a function that is easy to calculate,
but difficult to revert.
Here is where primes come in.

A one-way function $f$, is a function
that computes a result from a given input,
such as $f(x) = y$, for which no inverse
$f'$ is known, such that $f'(y) = x$ or
$f'(f(x)) = x$.
Plain multiplication, for instance, is not
a good one-way function,
since with the result $y$ known, 
we simply can revert 
the effect by division.
If we have a function $f(x) = ax$,
and a cyphertext $f(x) = y$.
We can reconstruct $x$, simply, by $f'(y) = y/a$.
Multiplication would be a good one-way function, however,
if both $a$ and $x$ were unknown.
The inverse, would then be factoring --
and factoring of large numbers is indeed
a hard problem.

Before we have a closer look at concrete examples,
we have to come back to a question we have already discussed
(but without satisfying results),
namely how to generate huge primes.
We have seen Mersenne primes and Fermat primes,
but little is known about this kind of numbers
and in particular, no recipe is known how to find new ones.
Practial algorithms are in fact much simpler
in terms of mathematical ideas.
Prime generators usually generate a random number
in a given range, test whether it is prime and, if it is,
return this number or, if it is not, try again.

A reasonable prime generator for natural numbers,
using once again the random number generator |randomNatural|,
could look like this:

\begin{minipage}{\textwidth}\begin{code}
  generatePrime :: Natural -> IO Natural
  generatePrime k = do  n <-  randomNatural (2^(k-1),2^k-1) 
                        t <-  rmptest n
                        if t  then  return n 
                              else  generatePrime k

\end{code}\end{minipage}

As you can see, this is just a trial-and-error approach
using the Rabin-Miller test to check whether a given
number is prime.
Is there not a risk of running a lot of time
depending on the range we have chosen?
Indeed, it can take a lot of time,
before we actually find a prime.
However, we know some things about the distribution
of primes, so that we are guaranteed
to find a prime within a reasonably chosen range.
We will discuss some of the relevant aspects
concerning the distribution of primes in the next section.
For the moment, it may suffice to mention
that a range like $k\dots k+c$,
where $c$ is some constant smaller than $k$,
is obviously not 
sufficient. The range above, however,
is chosen in terms of powers of 2 and,
assuming that $k$ is at least 10,
we are guaranteed to find some prime numbers
in such a range like, for $k=10$, $512 \dots 1023$.
It is quite common, by the way,
to give the size of the prime wanted
in bits, \ie\ in terms of 
the number of digits in binary representation,
rather than in decimal representation.

The first example of prime-based key exchange
is the Diffie-Hellman protocol
developed by Whitfield Diffie, Martin Hellman
and Ralph Merkle in the 70ies.
There was another group of scientists
that developed a similar algorithm shortly before
Diffie and Hellman published their results.
Those scientists, unfortunately, were working for the British 
secret service, \acronym{gchq}, at the time and were not allowed
to publish their results. 
They, thus, escaped eternity.

The Diffie-Hellman protocol is very simple and elegant,
but has some pitfalls that must be addressed.
Its main purpose is to exchange a secret key
between Alice and Bob without Eve, another
fictional character famous in cryptography literature,
getting to know that key by eavesdropping. 
Before the Diffie-Hellman protocol actually starts,
Alice and Bob have agreed somehow on a public key
that may be known to anyone including Eve.
This public key may be assigned to either Alice or Bob
and may be registered in a kind of phone book
or it may be agreed upon in the \term{handshaking} phase
of the protocol.

The public key consists of two parts:
a prime number $p$ and a generator $g$.
When Alice initiates the protocol,
she chooses a number $x$ from the range
$2\dots p-2$, computes $g^x \bmod{p}$ and
sends this number to Bob.
Bob, in his turn, chooses a number $y$
from the same range, computes $g^y \bmod{p}$
and sends the result back to Alice.
Evil Eve knows $p$ and $g$ and may see $g^x$ and $g^y$.
But none of these values is actually the key.
The key, instead, is $g^{xy}$.
At the end of the protocol,
this number is known to Alice and Bob.
Each of them just has to raise the number he or she receives
from the other by his or her own number.
So Alice chooses $x$ and sends $g^x$ to Bob.
Bob chooses $y$ and sends $g^y$.
Alice computes the key as $k = g^{y^x}$,
\ie\ she raises the number she receives from Bob
to her own number;
Bob computes the key, accordingly, as 
$g^{x^y}$, \ie\ he raises the number he receives from Alice
to his own number.
That is all.

For a simple example, let us assume the public key
is $p=11$ and $g=6$. 
Now, Alice chooses a random number from the range
$2\dots 9$, say, 4, and Bob likewise, say, 3.
Alice computes $6^4 \bmod{11} = 9$ and sends 9 to Bob.
Bob computes $6^3 \bmod{11} = 7$ and sends 7 to Alice.
Alice computes the key as $k = 7^4 \bmod{11} = 3$ and
Bob computes it as $k = 9^3 \bmod{11} = 3$.
The result is the same for both, of course,
because, eventually, both have performed the same operation:
$g^{xy}$.

The security is based on the difficulty to solve
the equation 

\begin{equation}
  k = g^{xy},
\end{equation}

where $g$, $g^x$ and $g^y$ are known.
This is an instance of the \term{discrete logarithm} problem,
the logarithm in a finite field.
If $k$, $g$ and $xy$ were ordinary real numbers,
we could solve the equation in three steps:
$x = log_g(g^{x})$, $y = log_g(g^{y})$ and, finally,
$k = g^{xy}$.
For the discrete logarithm, however, no efficient solution
is known today.

In a trivial examples like the one we used above,
Eve can simply try out all possible combinations.
In real cryptography applications, we therefore 
have to use very large primes.
But the security of the algorithm also depends on
the order of $g$.
For instance, if we want a security that corresponds
to \num{1000} bits (a number with more than 300 digits
in decimal representation), then the order of $g$ should
be much more than some hundreds or thousands or even millions
of numbers generated by that $g$.
To choose a proper $g$ is therefore essential
for the strength of the protocol.
The question now is: how to choose a proper $g$?

If you have carefully read the previous sections,
you already know the answer:
we must use a $g$ from the Schnorr group of a
safe prime. 
Indeed, knowing the order of $g$ is again
a hard problem. We have to solve the equation
$g^k = 1 \mod p$, which is again an instance
of the discrete logarithm.
We can circumvent this problem
of finding an appropriate group
by finding an appropriate prime. 
With a safe prime, the selection
of an appropriate $g$ is indeed simple.
We just have to avoid one of the trivial groups,
that is we have to avoid 1 and $p-1$, 
then we are guaranteed that $g$ is in 
the group of the Sophie Germain prime $q$
or in the larger group of the safe prime $2q+1$.

Sophie Germain primes, hence,
give us a means to reduce
the difficult problem of finding a proper $g$ to the much
simpler problem of selecting a proper prime:

\begin{minipage}{\textwidth}\begin{code}
  safePrime :: Natural -> IO Natural
  safePrime k = generatePrime k >>= \q -> let p = 2*q + 1 in do
                   t <- rmptest p
                   if t  then return p
                         else safePrime k
\end{code}\end{minipage}

This function generates a \term{safe prime},
\ie\ a prime of the form $2q + 1$,
where $q$ is a Sophie Germain prime.
It just generates a random prime $q$,
doubles it and adds 1 and checks
whether the resulting number $p$ is again prime
and, if not, repeats the process.
From the group of this prime,
we can then take a random $g$,
$g \neq 1, g \neq p-1$, and this $g$
belongs in the worst case 
to the group $q$ of order $\frac{p-1}{2}$.
Since $q$ was generated according to 
security level $k$, the group of 
$q$ is exactly what we need
and our main issue is solved.

There is still a problem, though.
From the communication between Alice and Bob,
Eve sees $g^x$ and $g^y$. 
If she has read the section on quadratic residues,
she can determine whether $g$ is a square (modulo $p$)
using the Legendre symbol.
If $g$ is a nonresidue, then 
she has an attack: 
she can repeat the test on the numbers she sees,
namely, $g^x$ and $g^y$. 
If a number of the form $g^z$ is a nonresidue, 
where $g$ as well is a nonresidue, then $z$ is odd,
otherwise, $z$ is even.
This is because even exponents are just 
repeated squares. So, if $z$ is even,
$g^z$ is a residue and, otherwise, it is not.

We should avoid this leakage.
Even though Eve does not learn the whole number,
she gets an information she is not entitled to have.
The leakage effectively reduces the security level
by the factor 2, since Eve learns the least significant
digit of the number in binary representation:
odd numbers in binary representation end on 1, 
even numbers on 0.
We can avoid this problem simply 
by chosing a residue right from the beginning.
The function
to generate an appropriate $g$
could then look like this:

\begin{minipage}{\textwidth}\begin{code}
  generator :: Natural -> IO Natural
  generator p = do  a <- randomNatural (2,p-2)
                    let g = (a^2) `rem` p
                    if  g == p - 1  then generator p
                                    else return g
\end{code}\end{minipage}

We select an $a$ from the range $2\dots p-2$
and square it. The number, hence, is guaranteed
to be a residue eliminating the problem
discussed above.
We test if $g = p-1$, which is forbidden,
since it is in a trivial group.
$a^2$, as you know, 
can become $p-1$, if
$p \equiv 1 \pmod{4}$.
You may argue that this is not the case,
when $p$ is a safe prime, because,
if $\frac{p-1}{2}$ is prime, then
$p-1$ cannot be a multiple of 4.
It is a good practice, however,
to keep different components 
of the security infrastructure  
independent of each other.
In spite of the fact that we usually use |generator|
with safe primes, it could happen
that someone uses it with another kind of prime
(for example a prime of the form $nq+1$).
This test simply avoids that anything bad happens
under such circumstances.

Note that we really do not need to check for $g=1$,
since $a^2$, with $a$ chosen from the range
$2 \dots p-2$, excluding both 1 and $p-1$,
can never be 1.

The next function initialises the protocol.
It is assumed that $p$ and $g$ are known already
to all involved parties when it is called:

\begin{minipage}{\textwidth}\begin{code}
  initProtocol ::  Chan Natural -> Chan Natural -> 
                   Natural -> Natural -> IO Natural
  initProtocol inch outch p g = do
    x   <- randomNatural (2,p-2)
    let gx = (g^x) `rem` p
    writeChan outch gx
    gy  <- readChan inch
    unless (checkG p gy) $ error ("suspicious value: " ++ show gy)
    return ((gy^x) `rem` p)
\end{code}\end{minipage}
\ignore{$}

The function receives four arguments:
An input channel and an output channel and
$p$ and $g$.
It starts by generating a random $x$ from the range $2\dots p-2$.
It then computes $g^x \bmod{p}$ and sends the result
through the outgoing channel.
Then it waits for an answer through the incoming channel.
When a response is received,
the value is checked by |checkG|, at which we will look in an instant.
If the value is accepted, the function returns this value
raised to $x$ modulo $p$. This is the secret key.

It is actually necessary to check incoming values,
since Eve may have intercepted the communication
and may have sent a value
that is not in the Schnorr group.
To protect against this \term{man-in-the-middle} attack,
both sides apply the following tests:

\begin{minipage}{\textwidth}\begin{code}
  checkG :: Natural -> Natural -> Bool
  checkG p x =  x /= 1 && 
                x <  p &&
                legendre  (fromIntegral x) 
                          (fromIntegral p) == 1
\end{code}\end{minipage}

For any value $x$ received through the channel,
it must hold that $x \neq 1$ 
(because 1 is in the wrong group),
it must also hold that $x < p$, \ie\
it must be a value modulo $p$, and 
$x$ must be a residue of $p$.
This is because we have chosen $g$ to be a square
and any square raised to some power is still a residue of $p$.
So, if one of these conditions does not hold,
something is wrong and we immediately abort the protocol.

Now, we look at the other side of the communication:

\begin{minipage}{\textwidth}\begin{code}
  acceptProtocol ::  Chan Natural -> Chan Natural ->  
                     Natural -> Natural -> IO Natural
  acceptProtocol inch outch p g = do
    gx  <- readChan inch
    unless (checkG p gx) $ error ("suspicious value: " ++ show gx)
    y   <- randomNatural (2,p-2) 
    let gy = (g^y)  `rem` p
    writeChan outch gy
    return ((gx^y) `rem` p)
\end{code}\end{minipage}
\ignore{$}

The function starts by waiting on input.
When it receives some input, it checks it using |checkG|.
If the value is accepted,
the function generates a random $y$ from the range $2\dots p-2$,
computes $g^y \bmod{p}$ and sends it back;
finally the key is returned.

Now, the protocol has terminated, both sides,
Alice and Bob, know the key and they can start
to use this key to encrypt the messages
exchanged on the channel.
Since, as mentioned several times,
this is not an introduction to cryptography,
we have omitted a lot of details.
One example is a robust defence against
\term {denial-of-service} attacks.
In the code above, we wait for input forever.
This is never a good idea.
An attacker could initiate one protocol after the other
without terminating any of them. 
The server waiting for requests
would quickly run out of resources.

\ignore{
Schnorr signature
}

A cryptosystem with somewhat different
purposes than Diffie-Hellman is \acronym{rsa},
named after its inventors Ronald Rivest,
Adi Shamir and Leonard Adleman and published in 1978.
Similar to Diffie-Hellman, \acronym{rsa} aims
to provide an encryption system for key exchange,
but, beyond encryption, \acronym{rsa} is also designed
as an authentication system 
based on electronic signatures.
The latter implies that the public key is 
assigned to a person.
Furthermore, an infrastructure is needed
that inspires the confidence that a given 
public key is really the key of the person
one believes one is communicating with.
There are many ways to create such an infrastructure.
It may be community-based, for example,
so that people one trusts guarantee for people
they trust; another way is that trusted organisations
provide phone books where signatures can be looked up.
All possible implementations depend at some point
on trust. In fact, trust is just the other side
of the security coin: without trust,
there is no security.

When Alice uses \acronym{rsa} to send a secret message
to Bob, she uses Bob's public key to encrypt the message,
and Bob, later, uses his private key to decrypt the message.
The design of \acronym{rsa} guarantees that it is 
extremely difficult to decrypt the cyphertext without
knowledge of Bob's private key.
If Alice, additionally, wants to sign the message
to make sure that Eve cannot exchange her messages
by other ones, she uses her own private key
to create a signature on the message (usually 
by first creating a \term{hash} of that message).
Bob can then assure himself that the message
was really sent by Alice by verifying the signature
using Alice's public key.
The role of public and private key, hence,
is swapped in encryption and signing.
Encryption is done with the addressee's
pupblic key and undone by the addresse's private key;
signing is done with the sender's private key
and approved with the sender's public key.

Just as Diffie-Hellman, \acronym{rsa} is based 
on modular arithmetic -- but with a composite modulus.
With a composite modulus, some care must be taken,
of course, since it does behave as
a prime only with respect to those numbers
that are coprime to it. 
The modulus $n$ is created by multiplying two large primes,
$p$ and $q$. This guarantees that $n$ behaves like 
an ``ordinary'' prime with respect to most numbers
in the range $1\dots n-1$, \viz\ all numbers
that are not multiples of $p$ and $q$.

We also need a number
$t$, such that for numbers $a$ coprime to $n$,
it holds that $a^t \equiv 1 \pmod{n}$.
For a prime number $p$, 
as we know from Fermat's little theorem,
this $t$ would be $p-1$.
For the prime factors of $n$, hence,
$p-1$ and $q-1$ would do the trick.
But any multiple of $p-1$ and $q-1$
would do the trick as well, including of course
$(p-1)(q-1)$ or $\textrm{lcm}(p-1,q-1)$,
the least common multiple of $p-1$ and $q-1$.

Now, consider Fermat's theorem written like this:

\begin{equation}
a^p \equiv a \pmod{p}
\end{equation}

and try to solve the following congruence system:

\begin{align*}
x & \equiv a \pmod{p}\\
x & \equiv a \pmod{q}.
\end{align*}

An obvious solution to this system,
according to the Chinese Remainder theorem,
is the number $x$ that is congruent to $a$ modulo $pq$.
Let us write $x$ as $a^t$, 
where $t = \textrm{lcm}(p-1,q-1)$:

\begin{align*}
a^t & \equiv a \pmod{p}\\
a^t & \equiv a \pmod{q}.
\end{align*}

An obvious solution to this system,
still according to the Chinese Remainder theorem,
is $a^t \equiv a \pmod{pq}$.
Let us look at an example: $p=7$, $q=11$ and $n = pq = 77$.
The $\textrm{lcm}$ of $p-1=6$ and $q-1=10$ is 30.
Therefore, for any number $a$: 
$a^{30} \equiv 1 \pmod{7}$ and
$a^{30} \equiv 1 \pmod{11}$,
but also:
$a^{31} \equiv a \pmod{7}$ and
$a^{31} \equiv a \pmod{11}$ 
and, this is important,
$a^{30} \equiv 1 \pmod{77}$ and
$a^{31} \equiv a \pmod{77}$.
For instance $a=3$: 
$3^{30} \equiv 1 \pmod{7}$, 
$3^{31} \equiv 3 \pmod{7}$ and
$3^{30} \equiv 1 \pmod{11}$ and
$3^{31} \equiv 3 \pmod{11}$. 
But also:
$3^{30} \equiv 1 \pmod{77}$ and
$3^{31} \equiv 3 \pmod{77}$.

Note that this is nothing new.
We just applied the Chinese Remainder theorem
to a quite trivial case.
However, from this trivial case
(and with some help from Euler
as we will see later),
an important theorem follows, namely
\term{Carmichael's theorem} that can be stated
in the scope of our problem here as:
the least number $t$ fulfilling the congruence
$a^t \equiv 1 \pmod{n}$
for any number $a$ coprime to $n$
is the $\textrm{lcm}$ of 
$p_1-1, p_2-1, \dots p_s-1$,
where $p_1\dots p_s$ are the prime factors of $n$.
Since our number $n$ has only two prime factors,
namely $p$ and $q$, our $t$ is $\textrm{lcm}(p-1,q-1)$.
So, finally, this fellow Carmichael is not
only bugging us with crazy numbers,
but he actually lends a hand to solve a problem
once in a while!
Thanks Robert!

The importance for the \acronym{rsa} system
is related to the fact that we need a public number
to compute the cyphertext and a private number
to reconstruct the original message.
The method to compute the cyphertext is exponentiation.
For this purpose, we need an exponent called $e$.
The number $e$ is chosen such that 
it is a small odd number, $1 < e < t$ and
$e$ does not divide $t$ nor $n$.
Now we find the inverse $e'$ of $e \bmod{t}$,
such that $ee' \equiv 1 \pmod{t}$.

When Alice encrypts a message $m$,
she computes $c = m^e \mod{n}$. 
To decrypt the cyphertext $c$, Bob 
computes $c^{e'}$, which is 
$m = m^{e^{e'}} = m^{ee'} \mod{n}$.
Modulo $t$ $m^{ee'}$ would just be $m^1 = m$,
since $ee' \equiv 1 \pmod{t}$.
Modulo $n$, this number is some multiple of $t$
plus 1: $ee' \equiv kt + 1 \pmod{n}$.
We, hence, have 
$m^{kt+1} = m^{kt} \times m = m^{t^k} \times m$.
But since $m^t \equiv 1 \pmod{n}$,
due to the Carmichael theorem, we have
$1^k \times m = 1 \times m = m$.
Again, Mr Carmichael, thank you!

To resume what we need for \acronym{rsa}:
We have a public key |(n,e)| and
a private key |(p,q,t,e')|, where
$n = pq$, 
$t = \textrm{lcm}((p-1),(q-1))$ and
$ee' \equiv 1 \pmod{t}$.
It is essential that 
all components of the private key
remain secret.
Any component that leaks out
helps Eve reveal the entire private key.
The core of the secret is $e'$,
since it can be used directly to decrypt
encrypted messages and to sign 
messages in the name of its owner.
With $t$ revealed, $e'$ can be simply
computed with the extended $\gcd$ algorithm;
with one of $p$ or $q$ revealed, 
the respective other factor of $n$
can be simply computed by $q = n/p$ or $p = n/q$.
With $p$ and $q$ both known, however,
$t$ can be computed by means of the $\textrm{lcm}$.
This boils down to the fact that the security
of \acronym{rsa} depends on the difficulty
of the discrete logarithm and the factoring
of large numbers.

Since, in any concrete implementation of \acronym{rsa},
we have to refer to the components of the keys
quite often, let us define data types 
to encapsulate the public and the private information:

\begin{minipage}{\textwidth}\begin{code}
  type PublicK  = (Natural,Natural)
  type PrivateK = (Natural,Natural,Natural,Natural)

  pubN,pubE :: PublicK -> Natural
  pubN = fst
  pubE = snd

  privP,privQ,privT,privD :: PrivateK -> Natural
  privP (p,_,_,_) = p
  privQ (_,q,_,_) = q
  privT (_,_,t,_) = t
  privD (_,_,_,d) = d
\end{code}\end{minipage}

This is just two type synonyms for private and public
key and a set of accessor functions.
Note that we call the inverse of $e$ in the private key
$d$ as it is often referred to in the literature.

As for Diffie-Hellman, chosing good values for
public and private key is an essential part of the system.
The following function is a reasonable key generator:

\begin{minipage}{\textwidth}\begin{code}
  generateKeys :: Natural -> IO (PublicK,PrivateK)
  generateKeys k  = do  p     <- genPrime 1
                        q     <- genPrime p
                        let t = lcm (p-1) (q-1)
                        pair  <- findED 1000 t
                        case  pair of
                              Nothing     ->  generateKeys k
                              Just (e,d)  ->  if  e == d || 
                                                  e == p || e == q ||
                                                  d == p || d == q 
                                              then generateKeys k
                                              else  let  pub  = (p*q,e)
                                                         priv = (p,q,t,d)
                                                    in   return (pub,priv)
    where genPrime p = do  q <- generatePrime (k `div` 2)
                           if p == q  then  genPrime p 
                                      else  return q
\end{code}\end{minipage}

We start by generating two primes using |genPrime|.
|genPrime| incorporates a test to ensure that we do not
accidently choose the same prime twice. 
When we call |genPrime| for $p$, we pass a number
that is certainly not equal to the generated prime,
namely one. In the second call to generate $q$,
we pass $p$.
Then we create $t$ as $\textrm{lcm}(p-1,q-1)$
and then we find the pair |(e,d)| using |findED|,
at which we will look next.
|findED| returns a |Maybe| value.
If the result is |Nothing|, we start all over again.
Otherwise, we ensure that $e$ and $d$ differ
and that $e$ and $d$ differ from $p$ and $q$.
This is to ensure that we do not accidently
publish one of the secrets, namely $e$
or one of the prime factors of $n$.
Finally, we create the public key as $(pq,e)$
and the private key as $(p,q,t,d)$.

The most intriguing part of the key generator
is finding the pair $(e,d)$. Since $t$ is not
a prime number, we are not guaranteed to find
an inverse for any $e < t$. So, we may need
several tries to find an $e$. But it might
even be that there is no such pair at all
for $t$. Since $t$ depends on the primes
$p$ and $q$, in such a case, we have to start
from the beginning. As an example, consider the primes
$p=5$ and $q=7$; $t$ in this case is 12.
If we try all combinations of the numbers
$2\dots 10$, we see that the only pairs
of numbers whose product is 1 are $5\times 5$
and $7\times 7$. In this case: $e = d$,
an option that any attacker will probably
try first and should therefore be
avoided. Here is an implementation of |findED|:

\begin{minipage}{\textwidth}\begin{code}
  findED :: Natural -> Natural -> IO (Maybe (Natural,Natural))
  findED 0 _   =  return Nothing
  findED i t   =  randomNatural (7,t-2) >>= \x ->
    let  z  |  even x     = x-1
            |  otherwise  = x 
         (e,d)            = tryXgcd z t
    in if (e*d) `rem` t == 1  then  if e > t `div` 2  then  return (Just (d,e))
                                                      else  return (Just (e,d))
                              else  findED (i-1) t
    where tryXgcd a t  =  case  nxgcd a t of
                                (1,k)  -> (a,k)
                                _      -> (0,0)
\end{code}\end{minipage}

The function takes two arguments.
The first is a counter that, when expired,
indicates that we give up the search 
with the given $t$.
As you can see above in |generateKeys|,
the counter is defined as 1000.
That is just some randomly chosen value --
there may be better ones.
We could try to invent some ratio for $t$
such as half of the odd numbers smaller than $t$.
But $t$ is a very large number.
Any ratio would force us to test single
numbers for hours if we are unlucky.

We then choose a random number 
from the range $7\dots t-2$ as a candidate for $e$.
We do not want $e=t-1$, since the inverse 
in this case is likely to be $e$ itself.
We start with 7, since we want to avoid
very small values for $e$.
We also want $e$ to be odd.
Otherwise, it will not be coprime to $t$,
which is even. 

We then compute $(e,d)$ by means of |tryXgcd|.
This function computes the |gcd| and the inverse
by means of |nxgcd|,
which we defined in the section on modular arithmetic.
If the |gcd| is not 1, then we are unlucky,
since $e$ and $t$ must be coprime.
In this case, we return |(0,0)|, a pair that
certainly will not pass the test 
$ed \equiv 1 \pmod{t}$
and, this way, we cause the next try of |findED|.
Otherwise, we return the pair $(a,k)$,
$e$ and its inverse modulo $t$.

If we have found a suitable pair in |findED|,
we return this pair either as $(e,d)$ or as $(d,e)$,
if $e > t `div` 2$.
The reasoning is that encryption will be more efficient
with a small $e$.
On the other hand, we do not want to impose any
explicit property on $d$ (such as $d > e$),
since that would be a hint that reduces the security level.

The key generator is usually not used,
when we establish a secure communication.
Instead, the key pair is considered to be stable
as long as it has not been compromised
by loss or by an attack on the server where
key pairs are stored.
Some stability is necessary 
for the authentication part of \acronym{rsa}.
If public keys changed frequently,
it would be more difficult to be sure
that a given key really belongs to the person
one thinks it belongs to.

Once the keys are available and the public key
has been published, Alice can encrypt a message
to Bob using the encryption function:

\begin{minipage}{\textwidth}\begin{code}
  encrypt :: PublicK -> Natural -> Natural
  encrypt pub m = (m^(pubE pub)) `rem` (pubN pub)
\end{code}\end{minipage}

Alice uses this function with Bob's
public key and a message $m$ represented
as a (large) integer value. 
The function raises the message
$m$ to $e$ and takes the result modular $n$.
Bob can decrypt the cyphertext using
his private key:

\begin{minipage}{\textwidth}\begin{code}
  decrypt :: PublicK -> PrivateK -> Natural -> Natural
  decrypt pub priv c = (c^(privD priv)) `rem` (pubN pub)
\end{code}\end{minipage}

To sign a message, Alice would use her private key:

\begin{minipage}{\textwidth}\begin{code}
  sign :: PublicK -> PrivateK -> Natural -> Natural
  sign pub priv m = (m^(privD priv)) `rem` (pubN pub)
\end{code}\end{minipage}

Signing, hence, is just the same as decryption,
but on a message that was not yet encrypted.
To verify the signature, Bob uses Alice's
public key with a function similar to encryption:

\begin{minipage}{\textwidth}\begin{code}
  verify :: PublicK -> Natural -> Natural
  verify pub s = (s^(pubE pub)) `rem` (pubN pub)
\end{code}\end{minipage}

In this form, \acronym{rsa} is not safe, however.
There are a lot of issues that must be addressed.
In particular, we omitted everything related
to \term{padding} messages before they are encrypted
and to \term{hashing} messages before they are signed.
These steps are essential for \acronym{rsa} to be secure.
But since these steps have little relation
to the mathematics of primes,
they are not relevant to this chapter.
After all, this is not an introduction to cryptography.
