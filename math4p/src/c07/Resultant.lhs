\ignore{
\begin{code}
module Resultant
where
  import Roots
\end{code}
}

When we started to discuss roots,
we mentioned the discriminant for
polynomials of degree 2,

\[
b^2 - 4ac,
\]

which tells us, just looking at the coefficients,
how many real roots the polynomial has,
namely two, if the discriminant is positive,
one repeated root, if it is 0, and
none at all, if it is negative.

Wouldn't it be nice to have a discriminant for
any degree? It turns out, there is.
At its very heart, this generalised discriminant
is the product
of the differences of the roots;
for instance, if we have the three
roots $\alpha, \beta, \gamma$, the
core of the discriminant is

\[
(\alpha - \beta)(\alpha - \gamma)(\beta - \gamma).
\]

In general we have

\begin{equation}
\prod_{i<j}{(x_i - x_j)}.
\end{equation}

Notice that there are $\binom{n}{2}$ factors
for $n$ the number of roots. For two roots,
there is only one factor, namely $\alpha - \beta$;
for three roots, there are three factors,
namely those given above; for four roots,
there are six factors and so on.

Now, when there is a repeated root,
then one of the factors will be 0 and,
as such, the whole product will be 0.
If at least one of the roots is non-real,
then the product will be non-real too.
We, hence, can read off the same properties
from this discriminant as we already could read
from the old one.

But the new discriminant tells us even more:
if it is irrational, then all roots are real,
but at least one is irrational. The other way round,
if it is rational, then all roots are rational.

There is a snag though.
In the generalised formula, we assumed a given
order of the roots -- but there is no such privileged order.
Imagine a polynomial with the roots
$-1, 2, -3, 4$, which is

\[
(x+1)(x-2)(x+3)(x-4) = x^4 - 2x^3 - 13x^2 + 14x + 24.
\]

When we compute the differences of the roots
in this order (and note that there are
$\binom{4}{2} = 6$
factors), \ie

\[
(-1-2)(-1+3)(-1-4)(2+3)(2-4)(-3-4),
\]

we get the result 2100.
But when we compute, just changing the order
of the roots to $2,-1,-3,4$,

\[
(2+1)(2+3)(2-4)(-1+3)(-1-4)(-3-4)
\]

we get -2100.
Indeed, of the $4! = 24$ permutations
of the roots, 12 lead to the positive
result and 12 to the negative one. 
That means: the discriminant in this form,
is not well defined!

We can get rid of the problem
by squaring the differences like this:

\begin{equation}
\prod_{i<j}{(x_i - x_j)^2}.
\end{equation}

We now would have

\[
(-1-2)^2(-1+3)^2(-1-4)^2(2+3)^2(2-4)^2(-3-4)^2,
\]

which is 4410000, and

\[
(2+1)^2(2+3)^2(2-4)^2(-1+3)^2(-1-4)^2(-3-4)^2,
\]

which is 4410000 as well.

Of course, we have to adapt our principles
to this new form:
there are non-real roots, if the discriminant
is negative and there are irrational roots
if the discriminant is not a perfect square.

You now may ask: is this discriminant actually
the same as the na\"ive one? Or, in other words,
is the na\"ive discriminant a special case
of this general form for second-degree polynomials?

Let us look at a second-degree polynomial, \eg:

\[
(x-2)(x+3) = x^2 + x - 6.
\]

The na\"ive discriminant ($b^2 - 4ac$) is 

\[
1^2 - 4\times (-6) = 1 + 24 = 25.
\]

The new discriminant is 

\[
(2+3)^2 = (-3-2)^2 = 25.
\]

This seems to be correct.
Let's try a non-monic polynomial, \eg\

\[
3(x-2)(x+3) = 3x^2 + 3x - 18.
\]

The na\"ive discriminant is

\[
3^2 - 4\times 3\times(-18) = 225.
\]

The roots are, of course, still 2 and $-3$.
So the new discriminant is still 25.
What are we missing?

Let's be practical and compare the two
numbers. What is their ratio? It is $225/25 = 9$.
9, however, is the leading coefficient squared:
$3^2$. So, if we multiplied the discriminant
by the square of the leading coefficient,
the results would be equal again.

Is this just by chance or can we prove that it is always the case?
We need to prove, for the case of a second-degree polynomial,
that

\begin{equation}
a^2(\alpha-\beta)^2 = b^2 - 4ac,
\end{equation}

where $a, b, c$ are the coefficients and
$\alpha, \beta$ are, as usual, the roots
of the polynomial.

First we observe that
$(\alpha-\beta)^2$
can be expressed as
$(\alpha+\beta)^2 - 4\alpha\beta$.
This is true because, when we multiply
$(\alpha+\beta)^2$ out, we get

\[
\alpha^2 + 2\alpha\beta + \beta^2.
\]

When we subtract $4\alpha\beta$, we obtain

\[
\alpha^2 - 2\alpha\beta + \beta^2,
\]

which clearly is $(\alpha-\beta)^2$.
Thus:

\begin{equation}
(\alpha-\beta)^2 = (\alpha+\beta)^2 - 4\alpha\beta.
\end{equation}

According to Vieta's formulas,
which we discussed in the previous section,
we have:

\begin{equation}
\alpha+\beta = -\frac{b}{a}
\end{equation}

and 

\begin{equation}
\alpha\beta = \frac{c}{a}.
\end{equation}

We therefore have

\begin{equation}
(\alpha-\beta)^2 = \left(-\frac{b}{a}\right)^2 - 4\frac{c}{a}.
\end{equation}

We multiply $a^2$ on both sides and get

\begin{equation}
a^2(\alpha-\beta)^2 = a^2\left(\left(-\frac{b}{a}\right)^2 - 4\frac{c}{a}\right).
\end{equation}

The right-hand side is 

\[
a^2\left(\frac{b^2}{a^2} - 4\frac{c}{a}\right).
\]

Distributing $a^2$ over the terms, we get

\[
\frac{a^2b^2}{a^2} - 4\frac{a^2c}{a}.
\]

and can now simplify to

\[
b^2 - 4ac,
\]

which leads to the desired result

\begin{equation}
a^2(\alpha-\beta)^2 = b^2 - 4ac.\qed
\end{equation}

In the general form,
which we won't prove here,
the discriminant can be computed as

\begin{equation}
a^{2d-2}\prod_{i<j}{(x_i - x_j)^2},
\end{equation}

where $a$ is the leading coefficient and $d$ the degree
of the polynomial. For $d=2$, the funny exponent
$2d-2$ is $2\times 2 - 2 = 2$.
For $d=3$, it would be $6-2=4$,
for $d=4$, we have $8-2=6$ and so on.

Of course, we again have to adapt our principles
to this new formula. To say something about
irrationality of the roots,
we need to divide the discriminant by $a^{2d-2}$.
If (and only if) the result is a perfect square,
the polynomial has only rational roots.
Note that we do not need to change anything
to decide whether there are non-real roots.
Since $2d-2$ is always even, $a$ raised to such a power
is always positive. It will, hence, not affect
the sign of the discriminant. Therefore, if (and only if)
the discriminant is negative, there are non-real roots. 

But isn't there a real issue?
The discriminant is supposed to tell us something
about the roots. But from what we see here,
we need to know the roots to compute the discriminant in the first place.
That is not very useful! The so called ``na\"ive''
discriminant is not too na\"ive after all! At least,
it has a function!

Well, here comes the esoteric part of this section.
There is in fact a way to compute the discriminant
without knowing the roots.
The discriminant can be computed from the \term{resultant}
of the polynomial and its derivative.
The resultant is a magic number
computed from two polynomials and
is often used to decide whether two polynomials
have a common root.

There are two ways to compute the resultant.
One comes from linear algebra and is very inefficient,
the other is related to polynomial arithmetic
and pretty efficient.
We will start with the first one.
It is inefficient, but the algorithm is quite interesting
and it introduces some concepts from linear algebra.
This way, the current section is also
a teaser for one of the next chapters
to come.

The resultant of two polynomials
can be computed as the \term{determinant}
of the \term{Sylvester matrix} of these polynomials.
Wow! That are two new concepts in one sentence!
We look at them one by one.
First, the \term{Sylvester matrix}.

We already met the concept of a matrix in the previous
part: a matrix is just a table of numbers.
The Sylvester matrix is a square matrix
(one with an equal number of rows and columns)
that contains
the coefficients of the two polynomials.
It has $d_1+d_2$ rows and columns,
where $d_1$ and $d_2$ are the degrees
of the polynomials.

It is constructed in the following way:
\begin{enumerate}
\item The first row of the matrix consists
      of the coefficients of the first polynomial
      and $d_2-1$ zeros on the right.
\item The next row contains one zero on the left
      followed by the coefficients of the first polynomial
      and $d_2-2$ zeros on the right.
\item We continue this way, incrementing the number
      of zeros on the left and decrementing it on the right
      until the number of zeros on the right is zero, that is,
      the last coefficient of the polynomial
      (the one of lowest degree) hits the end of the row.
\item Then we repeat the process with the second polynomial
      starting in the next row.
\end{enumerate}

Imagine the two polynomials
|P [4,3,2,1]| and |P [7,6,5]| (in mathematical notation:
$x^3+2x^2+3x+4$ and $5x^2+6x+7$)
which are of degree 3 and 2 respectively.
The Sylvester matrix, hence, is
the $5\times 5$ square matrix:

\[
\begin{pmatrix}
1 & 2 & 3 & 4 & 0 \\
0 & 1 & 2 & 3 & 4 \\
5 & 6 & 7 & 0 & 0 \\
0 & 5 & 6 & 7 & 0 \\
0 & 0 & 5 & 6 & 7 \\
\end{pmatrix}
\]

Here is an Haskell implementation
using the matrix type from the previous part:

\begin{minipage}{\textwidth}
\begin{code}
  sylvester :: (Num a) => Poly a -> Poly a -> L.Matrix a
  sylvester a b = L.M (go 0 xs ys)
    where  la  =  degree a
           lb  =  degree b
           ll  =  la + lb
           xs  =  (reverse $ coeffs a) ++ zeros (lb-1)
           ys  =  (reverse $ coeffs b) ++ zeros (la-1)
           go _ [] []               =  []
           go i l1 l2  | i == ll    =  []
                       | i >= lb    =  l2:go (i+1) [] (0:init l2) 
                       | otherwise  =  l1:go (i+1) (0:init l1) l2
\end{code}
\end{minipage}

When we call |sylvester| like this:

|sylvester (P [4,3,2,1]) (P [7,6,5])|

we get

|M [[1,2,3,4,0],[0,1,2,3,4],[5,6,7,0,0],[0,5,6,7,0],[0,0,5,6,7]]|

which corresponds to the matrix above.

The other concept mentioned above is the determinant of a matrix.
The determinant is defined only for square matrices and
can be seen as an encoding of certain \term{linear transformations}
described by the matrix. Those are all concepts from linear algebra
and will remain somewhat mysterious in this section.
However, we just want to compute the determinant and that is
an interesting recursive algorithm.

The determinant for a $2\times 2$ square matrix $m$ of the form

\[
\begin{pmatrix}
a & b \\
c & d \\
\end{pmatrix}
\]

also written $||m||$, is defined as

\begin{equation}
det(m) = ||m|| = ad - bc.
\end{equation}

It is, thus, a kind of ``cross product difference'', \ie\
the difference of the products resulting from
multiplying the elements in the square matrix
that share neither row nor column. In terms
of (row,column)-coordinates we have
$m[0,0]\times m[1,1] - m[0,1]\times m[1,0]$.

When we have an $n\times n$ square matrix
with $n>2$, we proceed as follows:

\begin{enumerate}
\item We cut off the first row.
\item For each element $x_i$ in that row
      (the subscript $i$ representing the column within that row),
      we compute $(-1)^ix_i||minor_i(m)||$
      where $minor_i$ is a square matrix formed
      from the original matrix without the first row
      and with the $i$th column removed.
\item We sum up the results.
\end{enumerate}

The determinant of the $3\times 3$ matrix $m$

\[
\begin{pmatrix}
a & b & c \\
d & e & f \\
g & h & i \\
\end{pmatrix}
\]

would be computed as

\begin{equation}
||m|| = a\left||\begin{smallmatrix}
          e & f \\ h & i
          \end{smallmatrix} \right||
      - b\left||\begin{smallmatrix}
          d & f \\ g & i
         \end{smallmatrix}\right||
      + c\left||\begin{smallmatrix}
          d & e \\ g & h
         \end{smallmatrix}\right||
\end{equation}

For $n\times n$ matrices with $n>3$,
the process repeats recursively on each
sub-determinant. The process is 
called \term{minor expansion formula} (\acronym{mef}),
since it expands into always more
minor matrices as it proceeds further.

To clarify how the \acronym{mef} proceeds,
here is a Haskell implementation.
We start with a simple function to
copy the columns of a matrix
cutting one column out:

\begin{minipage}{\textwidth}
\begin{code}
  copyWithout :: Int -> [a] -> [a]
  copyWithout p rs = go 0 rs
    where  go _ []                   = []
           go i (c:cs)  | i == p     = go (i+1) cs
                        | otherwise  = c : go (i+1) cs
\end{code}
\end{minipage}

which we use to create the rows and columns
of a minor matrix

\begin{minipage}{\textwidth}
\begin{code}
  minor :: Int -> [[a]] -> [[a]]
  minor  p []      = []
  minor  p (r:rs)  = copyWithout p r : minor p rs
\end{code}
\end{minipage}

Here is the \acronym{mef}:

\begin{minipage}{\textwidth}
\begin{code}
  mef :: (Num a) => [[a]] -> a
  mef []             = 0
  mef [[a]]          = a
  mef [[a,b],[c,d]]  = a*d - b*c
  mef (r:rs)         = sum [  (-1)^i*c*(go i rs) | (c,i) 
                              <- zip r [0..(length r)-1]]
    where go i = mef . minor i
\end{code}
\end{minipage}

Notice that we already cut out the first row in |mef|
and, this way, call |minor| with an $(n-1)\times n$ matrix,
which, in its turn, creates an $(n-1)\times (n-1)$ matrix.
The recursion enters in the |go| function,
which is called for each element in the sum.

Finally, we add a top-level function to compute the determinant:

\begin{minipage}{\textwidth}
\begin{code}
  det :: (Num a) => Matrix a -> a
  det m  | not (square m)  = error "not a square matrix"
         | otherwise       = mef (rows m)
\end{code}
\end{minipage}

It is one of the many amazing results of linear algebra
that the determinant of the Sylvester matrix
of two polynomials is the resultant
of these polynomials. Furthermore,
the discriminant of a polynomial
can be computed from the resultant of this polynomial
and its derivative. Concretely, it can be computed
as

\begin{equation}
dis(p) = (-1)^{d(d-1)/2}\times\frac{res(p,p')}{lc(p)},
\end{equation}

where $d$ is the degree of the polynomial
and $lc$ is the leading coefficient.
Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  dis :: (Num a, Integral a) => Poly a -> a 
  dis p = (-1)^x * (res p p') `div` l
    where  x   = d*(d-1) `div` 2
           d   = degree p
           p'  = derivative (*) p
           l   = lc p
\end{code}
\end{minipage}

Notice that we have not yet defined |res|.
We assume for the moment that |res| is
the determinant of the Sylvester Matrix.
We will implement it differently, however,
for reasons that we will understand in a minute.

But let us first test our implementation.
We start with our polynomial with the four roots
$-1, 2, -3, 4$, which is $x^4 - 2x^3 - 13x^2 + 14x + 24$.
Here is a Haskell session:

\begin{minipage}{\textwidth}
|let p = P [24,14,-13,-2,1]|\\
|let p' = derivative (*) p|\\
|let m = sylvester p p'|\\
|let d = det m|
\end{minipage}

What is $d$ right now?
Note that the degree of $p$ is 4.
The exponent of -1 in the computation of |dis|
above, hence, is $4\times3/2$, which is 6 and even.
The minus sign, therefore, disappears;
furthermore, the leading coefficient is 1.
The computation of the discriminant, this way,
reduces to the determinant:
$d$, therefore, shall be \num{4410000},
the discriminant of this polynomial.
Try it out!

Let us look at the second-degree polynomial
we already used above, namely
$(x-2)(x+3) = x^2 + x - 6$.

\begin{minipage}{\textwidth}
|let p = P [-6,1,1]|\\
|let p' = derivative (*) p|\\
|let m = sylvester p p'|\\
|let d = det m|
\end{minipage}

The determinant $d$, now is -25.
When we compute the discriminant,
the exponent will be $2\times 1/2$,
which is 1 and, as such, odd.
We will hence multiply the determinant
by -1 and arrive at the discriminant 25,
which we already computed above.

For the polynomial
$3(x-2)(x+3) = 3x^2 + 3x - 18$,
we get, following the same recipe,
-675. The minus sign will disappear and,
when we divide the result by the
leading coefficient, \ie\ $675/3$,
we get 225 as expected.

Well, we computed the determinant
with a recursive formula that expands
a minor for each column per recursion step.
For an $n\times n$ matrix with $n>2$,
we will compute $n$ minors in the first recursion step.
In the next recursion
step, if $n-1>2$, we will for each of those minors
compute $n-1$ minors and so on.
How many minors do we compute?

We compute $n$ for the first iteration,
$n-1$ for the second, $n-2$ for the third
and so on until $n=2$. We, hence, have
$n!/2$ recursion steps for an $n\times n$ square matrix.
For a degree-5 polynomial, since its derivative has
degree 4, we have a $9\times 9$ matrix and need
$9!/2 = 181440$ recursion steps.
For a degree-6 polynomial, we already have an
$11\times 11$ matrix and, thus, need
$11!/2 = 19958400$ recursion steps.
That is obviously inefficient already for polynomials
of moderate degree.
The algorithm is cute, but does not scale very well.

So, let us look at the alternative related
to polynomial arithmetic. It is based on the concept
of \term{pseudo remainders}. This concept is motivated
by the fact that, for polynomials over a ring
(like the integers), there is no Euclidean division,
\ie\ a division resulting in a quotient and a remainder.
In fact, when we divide polynomials, the coefficients
of the polynomials do not lie in the original ring
anymore. They are now fractional and, as such,
belong to a field ($\mathbb{Q}$ for the ring of integers)
and that is quite annoying since the \acronym{gcd}
does not make much sense in a field.

To deal with this, mathematicians invented pseudo remainders.
The pseudo remainder of two polynomials $p$ and $q$
is defined as

\begin{equation}
prem(p,q) = rem(lc(q)^{deg(p)-deg(q)+1}p, q).
\end{equation}

In plain English: we scale $p$ by the leading coefficient
of $q$ raised to the power of the difference of the
degrees of $p$ and $q$ plus one.
When you go back to the division formula,
it should be intuitively clear
that this guarantees that the coefficients
of the remainder are all integral and,
thus, still in the ring over which the polynomials
were defined.

We can define $prem$ in Haskell like this:

\begin{minipage}{\textwidth}
\begin{code}
  prem :: (Integral a) => Poly a -> Poly a -> Poly a 
  prem a b = poly $ map numerator (coeffs $ snd (divp x y))
    where  l   = lc b
           k   = l^(da-db+1)
           da  = fromIntegral $ degree a
           db  = fromIntegral $ degree b
           a'  = scale k a
           x   = P (map (%1) $ coeffs a')
           y   = P (map (%1) $ coeffs b)
\end{code}
\end{minipage}

This appears to be a lot of code for such a simple formula.
Most of the code, however, is related to convert the coefficients
back and forth between fractional (for which |divp| is defined)
and integral.

Of course, these remainders are now too big, since the
first polynomial has been scaled. Therefore mathematicians
invented yet another trick, namely to reduce the resulting
remainder by dividing by some number $\alpha$.

To compute the \acronym{gcd}, one replaces the equation
$r_{i+1} = rem(r_{i-1},r_i)$ by

\begin{equation}
r_{i+1} = \frac{prem(r_{i-1}, r_i)}{\alpha}
\end{equation}

Calling this recursively and preserving the intermediate results
leads to a \term{pseudo-remainder sequence}:

\begin{minipage}{\textwidth}
\begin{code}
  pgcd ::  (Integral a) =>
           (Poly a -> a) -> Poly a -> Poly a -> [Poly a]
  pgcd alpha p q  | zerop q = [p]
                  | otherwise =
                      let  r = prem p q
                           a = alpha r
                           x = P [numerator (c%a) | c <- coeffs r]
                      in if zerop r then [] else x : pgcd alpha q x
\end{code}
\end{minipage}

The function |pgcd| accepts, besides two polynomials,
a function that converts a polynomial
into an integral.
On each recursion step, it produces the pseudo remainder |r| and
an $\alpha$ for the remainder and then divides the coefficients
of the remainder by that number.

The trivial
pseudo-remainder sequence, for instance, results from
setting $\alpha = 1$:

\begin{minipage}{\textwidth}
\begin{code}
  tpgcd :: (Integral a) => Poly a -> Poly a -> [Poly a]
  tpgcd = pgcd (\_ -> 1)
\end{code}
\end{minipage}

The primitive pseudo-remainder sequence, by contrast,
uses the content of the polynomial resulting in
a sequence of primitive polynomials:

\begin{minipage}{\textwidth}
\begin{code}
  ppgcd :: (Integral a) => Poly a -> Poly a -> [Poly a]
  ppgcd = pgcd content
\end{code}
\end{minipage}

And now there is a pseudo-remainder sequence
whose final element by some weird magic
has the resultant of the two polynomials
as its only coefficient!
We could implement the sequence using
a second-order function similar to |pgcd|.
Unfortunately, we need state, since $\alpha$
does not only depend on the current remainder polynomial,
but also on the previous $\alpha$.
We, therefore, present a completely new implementation
(which is inspired by \term{sympy}, a Python package
for symbolic computing):

\begin{minipage}{\textwidth}
\begin{code}
  spgcd :: Poly Integer -> Poly Integer -> [Poly Integer]
  spgcd a b =  let  n = degree a
                    m = degree b
                    d = n - m
                    c = scale ((-1)^(d+1)) (prem a b)
                    l = lc b
                    z = -(l^d)
                    k = degree c
               in c:go z l b c k (m-k)
    where go c l f g n d  | degree(g) == 0  = []
                          | otherwise       =
                             let  y   = (-l) * c^d
                                  h   = P [x `div` y | x <- coeffs (prem f g)]
                                  m   = degree h
                                  l'  = lc g
                                  c'  | d > 1      = (-l')^d `div` (c^(d-1))
                                      | otherwise  = -l'
                             in h:go c' l' g h m (n-m)
\end{code}
\end{minipage}

\ignore{
discuss this code!
}

We can now define |res| (which we left out above) as:

\begin{minipage}{\textwidth}
\begin{code}
  res :: Poly Integer -> Poly Integer -> Integer
  res a b = head $ coeffs $ last $ spgcd a b
\end{code}
\end{minipage}

Now we can compute the discriminant of our polynomial
just using the |dis| function, which, in its turn,
computes the discriminant of the polynomial without
using the roots:

|dis (P [24,14,-13,-2,1])| yields 4410000,

|dis (P [-6,1,1])| yields 25 and

|dis (P [-18,3,3])| yields 225.

\ignore{
https://www.youtube.com/watch?v=AL5DdIJ9EQU
}
