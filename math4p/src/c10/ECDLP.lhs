\ignore{
\begin{code}
module ECDLP
where
  import ECModulo
  import Prelude hiding (mod)

  import Prime
  import Modular hiding (add,mul,mDiv)
\end{code}
}

To summarise the results from the previous sections,
we can describe EC Crypto as a system with three layers.
The first layer consists in modular integer arithmetic
and is used to do the math unterlying point addition.
Point addition itself belongs to the second layer,
the additive group of points on the curve.
Since all arithmetic modulo a prime is done within
the base formula for point addition, when we look
at point addition itself, we do not ``see'' 
modular arithmetic anymore. We do not add points
modulo something, we just add them using the addition
formula.
The third layer consists of the cryptosystems built
on top of the group of points. To actually build
cryptosystems on top of elliptic curves,
we first need a secret that we can use as private key
and an open parameter acting as public key.

When we look back at classic cryptography, we see
that we typically used an invertible operation,
namely \term{exponentiation}. In some cases,
like in Diffie-Hellman, the private key was a
combination of public keys of the form
$a^{xy} = (a^x)^y = (a^y)^x$,
in others,
like \acronym{rsa}, public and private keys
were inverses of each other, such that 
$a^{e\times d} = a^1 = a$.
In either case, the security of the private key
was based on the hardness of the 
discrete logarithm problem (\acronym{dlp}).
The \acronym{dlp} aims to solve equations
of the form 

\[
a^x = b
\]

for $x$, \ie\ it asks for the number $x$,
to which we have to raise $a$ to get $b$.
The classic crypto schemes are based on
a multiplicative group. The \acronym{dlp},
hence, asks for the number of repetitions
of successive multiplications of $a$ to get $b$.
In \acronym{ec}, however, we have an additive group or,
at least, we use ``additive terminology'' describing
the group operation. When we ask for the number
of repetitions of that operation on $a$ to get $b$,
we would hence ask for multiplication, not
for exponentiation. There is some potential 
for confusion in this terminology switch
from multiplicative to additive groups.
The problem on which the security in \acronym{ec} relies
is unfortunately called \acronym{dlp}, 
discrete logarithm problem.
It would be much more precise in this case to refer to the
discrete quotient problem, since we talk about
multiplication, not exponentiation.
However, the terminology is like that.
So, we stick with it and now define the \acronym{dlp}
for \acronym{ec} crypto.

The public information of \acronym{EC} crypto systems
consist of parameters describing the curve,
the coefficients, the modulus, the starting point $P$ and perhaps
some other details specifying the exact curve,
and a point $Q$, which represents the public key.
We secret, then, is a natural number $n$, such that

\begin{equation}
nP = Q.
\end{equation}

The \acronym{dlp}, hence, consists in finding
a \emph{factor} $n$ that determines how often
we have to add $P$ to get to $Q$.
This may sound weird, but, in fact, it is
a hard problem equivalent in computational complexity
to finding the discrete logarithm in classic
cryptography.

Consider the curve we already used above
with starting point $P=(5,1)$. The public
key is $Q=(9,16)$. Can you say, without
looking at the whole group above, what $n$ is,
such that $n\times (5,1) = (9,16)$?
The point is that no algorithm is known
that would do that in acceptable time
for large groups.
For this toy group, we obviously can try.
We would see that:

\[
(5,1) + (5,1) = (6,3)  = 2P,
\]\[
(6,3) + (5,1) = (10,6) = 3P,
\]\[
(10,6) + (5,1) = (3,1) = 4P,
\]\[
(3,1) + (5,1) = (9,16) = 5P.
\]

We, thus, find our point after four additions,
which corresponds to multiplying the point by 5.
With a group that contains $2^{256}$ elements,
this approach would not be feasible.

But how can it then be feasible 
to compute $nP$ for large $n$?
We would need $n$ steps to produce that result
and $n$ can be a really large number.
Obviously, we need a way to perform
multiplication in significantly less than $n$ steps,
in $\log{n}$ steps, for instance.

There is indeed a way to do this.
The algorithm is quite similar to the algorithm
we used to raise a number to a huge power,
which was \term{double-and-square}.
Here we use a variant of that algorithm called
\term{double-and-add}, since we are dealing
with an additive group.
The algorithm may be implemented like this:

\begin{minipage}{\textwidth}
\begin{code}
  mul :: Curve -> Integer -> Point -> Point
  mul _ 0 _  = O
  mul _ _ O  = O
  mul c n p  = foldl da p (tail $ toBinary n) 
    where  da q 0  = add c q q
           da q 1  = add c p (add c q q)
\end{code}
\end{minipage}

First 0 times anything is the identity.
Then, the identity multiplied by any number
is just the identity. It never changes by multiplication.
Just as zero would never change by multiplication
in the realm of numbers.
Any point multiplied by zero, on the other hand,
is the identity of the additive group and that, again, is zero.

In all other cases, we convert $n$ into a list
of binary digits of which we process all but the head.
For each digit, we double the intermediate result $q$,
that is we compute |add c q q|.
If the current digit is 0, we are done and continue
with the next digit.
Otherwise, if it is 1, we additionally add $p$.
When there are no more digits left,
we have a result.

Consider for example $5 \times P$.
The binary representation of 5 is |[1,0,1]|.
We, hence, would compute $P+P$ for 0, starting with the tail of |[1,0,1]|.
This is $2P$. Then we compute $2P+2P+P for 1,
which is $5P$.

