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
and is used to do the math underlying point addition.
Point addition itself belongs to the second layer,
the additive group of points on the curve,
which results from \emph{using} point addition.
Since all arithmetic modulo a prime is done within
point addition, when we look
at point addition itself, we do not ``see'' 
modular arithmetic anymore. We do not add points
modulo something, we just add them using the addition
formula.

The third layer consists of the cryptosystems built
on top of the group of points. To actually build
cryptosystems on top of elliptic curves,
we need a secret that we can use as private key
and an open parameter acting as public key.
Perhaps the following schematic sketch helps:

\begin{center}
\begin{tikzpicture}
\draw (0,0) rectangle (6,1.5);
\node [font=\small,align=center] (g1) at (3,0.75) 
      {$a^{\lambda(n)} \equiv 1 \pmod{n}$};
\draw (0,2) rectangle (6,3.5);
\node [font=\small,align=center] (g2) at (3,2.75) 
      {$nP = \mathcal{O}$};
\draw (0,4) rectangle (1.5,5.5);
\node [font=\small,align=center] (s1) at (0.75,4.75) 
      {\acronym{ecdh}};
\draw (1.5,4) rectangle (3,5.5);
\node [font=\small,align=center] (s1) at (2.25,4.75) 
      {\acronym{ecies}};
\draw (3,4) rectangle (4.5,5.5);
\node [font=\small,align=center] (s1) at (3.75,4.75) 
      {\acronym{ecdsa}};
\draw (4.5,4) rectangle (6,5.5);
\node [font=\small,align=center] (s1) at (5.25,4.75) 
      {\acronym{$\dots$}};
\draw [fill=white] (3, 1.75) ellipse (3cm and 0.5cm);
\node [font=\tiny ,align=center] (pa) at (3,1.75)
      {$(x_P,y_P) + (x_Q,y_Q) = (x_R,y_R)$}; 
\draw [fill=white] (3, 3.75) ellipse (3cm and 0.5cm);
\node [font=\tiny ,align=center] (sec) at (3,3.75)
      {$(private,public)$}; 
\end{tikzpicture}
\end{center}
\ignore{$}

Bottom up, we see first the arithmetic group
modulo some prime, indicated by Car\-michael's
theorem. On top of it, we built point addition,
which, in its turn, is the basis for the group
of points on the curve, indicated by the fact
that repeated addition (\ie\ multiplication)
of a point $P$ yields the identity of that group
$\mathcal{O}$. By means of this group,
we build a key pair $(public,private)$,
which then enables us to build concrete
cryptographic schemes, such as 
\acronym{ec} Diffie-Hellman (\acronym{ecdh}),
\acronym{ec} Integrated Encryption Scheme (\acronym{ecies}) and the
\acronym{ec} Digital Signature Algorithm (\acronym{ecdsa}),
which we will discuss in the next sections.
As indicated by the right-most box in that layer,
there are many more cryptographic schemes
for elliptic curves.
But we will not discuss all of them here. 

When we look back at classic cryptography, we see
that we typically used an invertible operation,
namely \term{exponentiation}. In some cases,
like in Diffie-Hellman, the private key was a
combination of public keys of the form
$a^{xy} = (a^x)^y = (a^y)^x$,
in others,
like \acronym{rsa}, public and private keys
were inverses of each other, such that 
$a^{ed} = a^1 = a$.
In either case, the security of the private key
was based on the hardness of the 
discrete logarithm problem (\acronym{dlp}).
The \acronym{dlp} aims to solve equations
of the form 

\[
a^x = b
\]

for $x$, \ie\ it asks for the number $x$,
to which we have to raise $a$ to get to $b$.
The classic crypto schemes are based on
a multiplicative group. The \acronym{dlp},
hence, asks for the number of repetitions
of successive multiplications of $a$ to get to $b$.
In \acronym{ec}, however, we have an additive group or,
at least, we use ``additive terminology'' describing
the group operation. When we ask for the number
of repetitions of that operation on $a$ to get $b$,
we would hence ask for multiplication, not
for exponentiation. There is some potential 
for confusion in this terminology switch
from multiplicative to additive groups.
The problem on which the security in \acronym{ec} relies
is unfortunately also called \acronym{dlp}, 
discrete logarithm problem.
It would be much more precise in this case to refer to the
\term{discrete quotient problem}, since we talk about
multiplication, not exponentiation.
However, the terminology is like that.
So, we stick to it and define the \acronym{dlp}
for \acronym{ec} crypto.

The information of \acronym{ec} crypto systems
that is publicly known 
consists of parameters describing the curve,
the coefficients, the modulus, a starting point $P$ and perhaps
some other details specifying the exact curve.
The public key is typically a point $Q$ and the 
secret is a natural number $n$, such that

\begin{equation}
nP = Q.
\end{equation}

The \acronym{dlp}, hence, consists in finding
a \emph{factor} $n$ that determines how often
we have to add $P$ to itself to get to $Q$.
This may sound weird, but, in fact, it is
a hard problem equivalent in computational complexity
to finding the discrete logarithm in classic
cryptography.

Consider the curve we already used above
with starting point $P=(5,1)$. If you consider the public
key to be $Q=(9,16)$, can you tell, without
looking at the whole group above, what $n$ must be,
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
to compute $nP$ for large $n$ in the first place?
We would need $n$ steps to produce that result
and $n$ can be a really large number.
Obviously, we need a way to perform
multiplication in significantly less than $n$ steps,
in $\log{n}$ steps, for instance.

There is indeed a way to do this.
The algorithm is quite similar to the algorithm
we used to raise a number to a huge power,
which was \term{multiply-and-square}.
Here we use a variant of that algorithm called
\term{double-and-add}, since we are dealing
with an additive group.
The algorithm may be implemented like this:

\begin{minipage}{\textwidth}
\begin{code}
  mul :: Curve -> Natural -> Point -> Point
  mul _ 0 _  = O
  mul _ _ O  = O
  mul c n p  = foldl da p (tail $ toBinary n) 
    where  da q 0  = add c q q
           da q 1  = add c p (add c q q)
\end{code}
\end{minipage}

The first base case states that
0 times anything is the identity.
Then, the identity multiplied by any number
is again the identity. The identity, indeed,
never changes with multiplication.
Just as zero would never change by multiplication
in the realm of numbers.
Any point multiplied by zero, on the other hand,
is the identity of the additive group and that, again, is zero.

In all other cases, we convert $n$ into a list
of binary digits of which we process all but the head.
For each digit, we double the intermediate result $q$,
that is we compute |add c q q|.
If the current digit is 0, we are done with this digit and continue
with the next one.
Otherwise, if it is 1, we additionally add $p$.
When there are no more digits left,
we have a result.

It is noteworthy that this implementation of multiplication
does not use the means of the arithmetic group modulo a prime
that underlies point addition. It is build on top of addition
using only terms related to the group of points on the curve,
the second layer in the sketch above.

Let us look at an example. Say, we want to
compute $19P$.
The binary representation of 19 is |[1,0,0,1,1]|.
We, hence, would compute $P+P$ for the first digit 0 
(which is the head of the tail of our number).
This is $2P$. With this result we go into the next round.
The next digit is 0 again and we get $4P$,
which is the input for the next iteration.
The next digit is 1, so we double and add.
We, hence, compute $4P+4P+P = 9P$.
This is now the input to the final digit.
Since it is 1 again, we again double and add
and we have $9P+9P+P=19P$.

It works perfectly. But why does it work? Consider
the representation of a number in terms of
powers of 2 multiplied by a number $k$:

\[
k(a_r2^r + a_{r-1}2^{r-1} + \dots + a_02^0),
\]

where, for $i \in \lbrace 0\dots r\rbrace$, 
$a_i \in \lbrace 0,1\rbrace$.
Multiplying this out, we get

\[
a_r2^rk + a_{r-1}2^{r-1}k + \dots + a_02^0k.
\]

Obviously, from step to step, that is from plus sign
to plus sign, right to left, $k$ doubles. 
Ignoring the coefficients $a_i$
for a moment, this would look for the concrete number
$10011_2 = 19_{10}$ like

\[
16k + 8k + 4k + 2k + k.
\]

Doubling alone would in this case generate the number $16k$,
which would indeed be the correct result if the 
binary number were $10000_2$.
Now, we eliminate all terms with coefficient $a_i = 0$,
which are $8k$ and $4k$. We are left with

\[
16k + 2k + k.
\]

The value we add to $16k$ corresponds exactly
to the value of $k$ we would add with |mul|. For the example,
we would add one $k$ processing the last but one digit.
This $k$ is now part of the intermediate result, $2q+k$, that
goes into the processing of the last digit.
Processing the last digit, we double the previous result,
obtaining $4q+2k$ and, since the last digit is also 1,
we add $k$ again. We, hence, get three ``extra'' $k$s,
which we add to the overall doubling result 16 and get
$16+2+1=16+3=19$.
