\ignore{
\begin{code}
module Umbral
where
  import DMachine
\end{code}
}

We saw that the differences and the derivative
is not the same concept. Despite of many similarities,
the polynomial of degree $n-1$ that generates 
the differences of a given polynomial of degree $n$
is not necessarily the derivative of that polynomial.
There is a class of polynomials, however, for which
derivative, well, a very special kind of derivative,
and differences are actually the same.
Those are the \term{factorial polynomials}.

A factorial polynomial $x^{(n)}$ is a polynomial of the form

\begin{equation}
x^{(n)} = x(x-1)(x-2) \dots (x-n+1). 
\end{equation}

A factorial polynomial, hence, is generated by the
\term{falling factorial} of $x$.
The simplest factorial polynomial $x^{(1)}$ is

\begin{equation}
x^{(1)} = x. 
\end{equation}

The, arguably, even simpler than simplest factorial polynomial
$x^{(0)}$ is, according to the definition of the
factorials, 1.

Here is a Haskell function that shows the
factors of the $n^{th}$ factorial polynomial:

\begin{minipage}{\textwidth}
\begin{code}
  fpfacs :: (Integral a) => a -> [Poly a]
  fpfacs 0 = [P [1]]
  fpfacs n = [poly [-k,1] | k <- [0..n-1]]
\end{code}
\end{minipage}

Let us look at the first factorial polynomials:

\begin{minipage}{\textwidth}
|fpfacs 0|: |[P [1]]|\\
|fpfacs 1|: |[P [0,1]]|\\
|fpfacs 2|: |[P [0,1],P [-1,1]]|\\
|fpfacs 3|: |[P [0,1],P [-1,1],P [-2,1]]|\\
|fpfacs 4|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1]]|\\
|fpfacs 5|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1],P [-4,1]]|\\
|fpfacs 6|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1],P [-4,1],P [-5,1]]|\\
|fpfacs 7|: |[P [0,1],P [-1,1],P [-2,1],P [-3,1],P [-4,1],P [-5,1],P [-6,1]]|
\end{minipage}

This suggests that the factorial polynomials,
just as the factorials, can be defined recursively.
The following equation describes the recursive formula:

\begin{equation}
x^{(n+1)} = (x-n)x^{(n)},
\end{equation}

which we can translate to Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  rfacpoly :: (Integral a) => a -> Poly a
  rfacpoly 0 = P [1]
  rfacpoly n = mul (rfacpoly (n-1)) (P [-(n-1),1])
\end{code}
\end{minipage}

The recursive formula is, of course, not an efficient
computing tool. For the factorial polynomial $x^{(n)}$,
we would need $n$ recursive steps, namely
$x^{(n-1)}(x-n+1)$,
$x^{(n-2)}(x-n+2)$, \dots,
$x^{(0)}x$.
To compute, for instance, $n=3$,
we need to compute:

\[
\begin{array}{lclcl}
x^{(1)} & = & x^{(0)}x &=& x\\
x^{(2)} & = & x(x-1) &=& (x^2 - x)\\
x^{(3)} & = & (x^2-x)(x-2) &=& (x^3 - 3x^2 + 2x)
\end{array}
\]

A better way to compute the polynomial,
once we have its factors, is to just multiply
them out, like: |prodp mul|.
The following implementation first creates
the factors and then builds their product:

\begin{minipage}{\textwidth}
\begin{code}
  facpoly :: (Integral a) => a -> Poly a
  facpoly = prodp mul . fpfacs
\end{code}
\end{minipage}

The two functions, |rfacpoly| and |facpoly|,
create exactly the same result.
When we apply one of them to |[1..7]| 
as above we get

\begin{minipage}{\textwidth}
|facpoly 1|: |P [0,1]|\\
|facpoly 2|: |P [0,-1,1]|\\ 
|facpoly 3|: |P [0,2,-3,1]|\\ 
|facpoly 4|: |P [0,-6,11,-6,1]|\\ 
|facpoly 5|: |P [0,24,-50,35,-10,1]|\\ 
|facpoly 6|: |P [0,-120,274,-225,85,-15,1]|\\
|facpoly 7|: |P [0,720,-1764,1624,-735,175,-21,1]| 
\end{minipage}

which corresponds to the polynomials (in mathematical notation):

\begin{center}
\begin{tabular}{c}
$x$ \\
$x^2 - x$ \\
$x^3 - 3x^2 + 2x$ \\
$x^4 - 6x^3 + 11x^2 - 6x$ \\
$x^5 - 10x^4 + 35x^3 -50x^2 + 24x$ \\
$x^6 - 15x^5 + 85x^4 - 225x^3 + 274x^2 -120x$ \\
$ x^7 - 21x^6 + 175x^5 - 735x^4 + 1624x^3 - 1764x^2 + 720x$  
\end{tabular}
\end{center}

Note, by the way, the last coefficient in each polynomial.
Those are factorials. More precisely, the last coefficient
of $x^{(n)}$ is $(n-1)!$.
Does this pattern remind you of something?
Not? Don't worry, we will look into it later.

Let us now turn to differences.
Instead of just applying the polynomial to a sequence
of numbers and then compute the differences,
we could try to find a formula that expresses
the differences for a given polynomial.
When we take a formula like $x^{(3)}$,
we can compute its differences by
applying two consecutive values
and compute the difference of the results, \eg:

\[
\begin{array}{ccl}
&   & 3^{(3)} - 2^{(3)} \\
& = & (3^3 - 3\times 3^2 + 2\times 3) -
    (2^3 - 3\times 2^2 + 2\times 2)\\
& = & (27 - 27 + 6) - (8 - 12 + 4) \\
& = & 6 - 0\\
& = & 6.
\end{array}
\]

Instead of using concrete numbers, we can use
a placeholder like $a$:

\[
\begin{array}{ccl}
&   & (a+1)^{(3)} - a^{(3)}\\
& = & ((a+1)^3 - 3(a+1)^2 + 2(a+1)) -
      (a^3 - 3a^2 + 2a)\\
& = & ((a^3 +3a^2 + 3a + 1) - (3a^2+6a+3) + (2a+2)) - 
      (a^3 - 3a^2 + 2a)\\
& = & (a^3 - a) - 
      (a^3 - 3a^2 + 2a)\\
& = & 3a^2 - 3a
\end{array}
\]

Let us test this result.
We first apply $x^{(3)}$ on a sequence
and compute the differences:
|diffs (mapply (facpoly 3) [0..11]|.
From this we get

0, 0, 6, 18, 36, 60, 90, 126, 168, 216, 270.

Now we apply the polynomial $3x^2 - 3x$
on the same sequence (minus one,
because |diffs| has one element less
than the sequence it is applied to): 
|mapply (P [0,-3,3]) [0..10]|
and get

0, 0, 6, 18, 36, 60, 90, 126, 168, 216, 270.

The same sequence.

But what is so special about the result
$3x^2 - 3x$ in the first place?
Well, we can factor 3 out and get
$3(x^2 - x)$, whose second part is $x^{(2)}$
and whose first part is $n=3$. In other words,
what we see here is that the differences
of $x^{(n)}$ can be computed by
the polynomial $nx^{(n-1)}$ and that formula
is very similar to the concept of the derivative.
Of course, it is not really the derivative,
since the derivative of a polynomial
deals with powers. The derivative 
of the polynomial $x^n$ is,
according to the power rule, $nx^{n-1}$.
We see the same pattern here, but the
exponent is not really an exponent,
but a falling factorial.

A system that establishes a calculus
that follows the same rules as the
\term{infinitesimal calculus}, to which
the derivative belongs, is often called
\term{umbral calculus}. Most typical 
\term{umbral calculi} are systems of computations
based on \term{Bernoulli polynomials} and 
\term{Bernoulli numbers}.
But factorial polynomials, too, establish
an \term{umbral calculus}.

Here is a Haskell function to compute
the umbral derivative of the factorial polynomial
$x^{(n)}$:

\begin{minipage}{\textwidth}
\begin{code}
  uderivative :: (Integral a) => a -> Poly a
  uderivative n = scale n (facpoly (n-1))
\end{code}
\end{minipage}

But we are moving fast. We have just looked at
one special case, namely the differences of
$x^{(3)}$. To be sure that the equation

\begin{equation}
\Delta_{x^{(n)}} = nx^{(n-1)},
\end{equation}

holds for all factorial polynomials,
\ie\ that the differences of $x^{(n)}$
equal $nx^{(n-1)}$,
we first need to show it for the general case.

To do this, we start as above. We plug in
the ``value'' $a$ and compute the difference
$\Delta_{a^{(n)}} = (a+1)^{(n)} - a^{(n)}$.
When we expand the
formula for the falling factorial, we get

\[
\begin{array}{lcclll}
\Delta_{a^{(n)}} & = &   & (a+1) & a(a-1)\dots(a-n+2) & \\
                 &   & - &       & a(a-1)\dots(a-n+2) & (a-n+1)\\
\end{array}
\]

On the right-hand side of this equation
we see a middle part that is identical
in both lines, namely $a(a-1)\dots(a-n+2)$,
which is composed of the common factors of
$(a+1)^{(n)}$ and $a^{(n)}$.

We zoom out to get a better overview of the equation
by setting $b=a(a-1)\dots(a-n+2)$ and obtain:

\begin{equation}
  \Delta_{a^{(n)}} = (a+1)b - (a-n+1)b.
\end{equation}

By regrouping, we get $(a+1-a+n-1)b$.
In the sum, we have $a$ and $-a$ as well as 1 and $-1$.
These terms, hence, cancel out and we are left with
$\Delta_{a^{(n)}} = nb$.
But $b$ is $a(a-1)\dots(a-n+2)$, \ie\ the same
as the second line, but with one factor removed, namely
$(a-n+1)$. That, however, is $a^{(n-1)}$ and, thus,
we have

\begin{equation}
  \Delta_{a^{(n)}} = na^{(n-1)}.\qed
\end{equation}

This rule can be used to provide an elegant proof
for Pascal's rule, which, as you may remember,
states that

\begin{equation}
\binom{k+1}{n+1} = \binom{k}{n+1} + \binom{k}{n}.
\end{equation}

We start by subtracting $\binom{k}{n+1}$
from both sides, obtaining

\begin{equation}
\binom{k+1}{n+1} - \binom{k}{n+1} = \binom{k}{n}.
\end{equation}

This corresponds to

\begin{equation}
\frac{(k+1)^{(n+1)}}{(n+1)!} - \frac{k^{(n+1)}}{(n+1)!} = \binom{k}{n}.
\end{equation}

When we join the fractions on the left-hand side,
we get in the numerator the formula to compute 
the differences of $k^{(n+1)}$:

\[
\frac{(k+1)^{(n+1)} - k^{(n+1)}}{(n+1)!} =
\frac{\Delta_{k^{(n+1)}}}{(n+1)!}.
\]

We have shown that $\Delta_{k^{(n+1)}} = (n+1)k^{(n)}$.
If we substitute this back into the original equation,
we see

\begin{equation}
\binom{k+1}{n+1} - \binom{k}{n+1} =
\frac{(n+1){k^{(n)}}}{(n+1)!}.
\end{equation}

We now see in the fraction on the right-hand side 
that there is one factor
that appears in numerator and denominator,
namely $n+1$. When we cancel $n+1$ out
we need to
reduce $(n+1)!$ in the denominator by this factor.
$(n+1)!$, however, is $(n+1)n!$.
We therefore get:

\begin{equation}
\binom{k+1}{n+1} - \binom{k}{n+1} =
\frac{{k^{(n)}}}{n!} = 
\binom{k}{n}.\qed
\end{equation}

The difference between $x^{(n)}$ and $x^n$ is,
as already stated above, that the former is
a falling factorial, while the latter is
a power. Those are distinct concepts.
For instance, $x^2$ is $xx$, while
$x^{(2)}$ is $x(x-1)$. The falling factorial of $n$,
hence, is smaller than the corresponding power $n$.
We can even say precisely how much smaller it is.
We just have to look at the list of factorial
polynomials we have created above:

\begin{equation}
x^{(2)} = x(x-1) = x^2 - x.
\end{equation}

So, we could express $x^2$ as
$x^{(2)} + x$ adding the part that
we subtract from $x^2$ to get $x^{(2)}$.
If we wanted to express $x^2$ strictly
in terms of falling factorials, we could say: 

\begin{equation}
x^2 = x^{(2)} + x^{(1)}.
\end{equation}

With the same technique, we can establish 
what $x^3$ is in terms of factorial polynomials.
Since

\begin{equation}
x^{(3)} = x^3 - 3x^2 + 2x,
\end{equation}

we have

\begin{equation}
x^3 = x^{(3)} + 3x^2 - 2x.
\end{equation}

Using the previous result, we arrive at

\begin{equation}
x^3 = x^{(3)} + 3(x^{(2)} + x^{(1)}) - 2x^{(1)} =
      x^{(3)} + 3x^{(2)} + x^{(1)}.
\end{equation}

For $x^4$, we have

\begin{equation}
x^4 = x^{(4)} + 6x^3 - 11x^2 + 6x 
\end{equation}

and, hence,

\[
\begin{array}{lcl}
x^4 & = & x^{(4)} + 6(x^{(3)} + 3x^{(2)} + x^{(1)}) - 11(x^{(2)} + x^{(1)}) + 6x^{(1)} \\
    & = & x^{(4)} + 6x^{(3)} + 7x^{(2)} + x^{(1)}.
\end{array}
\]

In this way, we can go on and create formulas for all powers
(and, once we have shown that we can express powers by
factorial polynomials, we can show that we can represent
polynomials as factorial polynomials).
We can even show that each power has a unique representation
as sum of factorial polynomials, just as each number
has a unique representation as product of prime numbers.

To prove this, suppose that, for a power $x^n$,
there were two different representations as sums of
factorial polynomials, such that

\begin{equation}
\begin{array}{lcl}
x^n & = & A_1x^{(1)} + A_2x^{(2)} + \dots + A_nx^{(n)}\\
    & = & B_1x^{(1)} + B_2x^{(2)} + \dots + B_nx^{(n)}.
\end{array}
\end{equation}

When we subtract one representation from the other,
the result shall be zero, since both represent the
same value $x^n$. So, we have:

\begin{equation}
A_1x^{(1)} + A_2x^{(2)} + \dots + A_nx^{(n)} -
B_1x^{(1)} + B_2x^{(2)} + \dots + B_nx^{(n)} = 0.
\end{equation}

Regrouping we get

\begin{equation}
(A_1-B_1)x^{(1)} + (A_2-B_2)x^{(2)} + \dots + (A_n-B_n)x^{(n)} = 0.
\end{equation}

There are two ways for this sum to become zero.
Either the $A_n-B_n$ parts are all zero or
the $x^{(n)}$ parts are all zero (or, of course,
in some cases it is like this and in others
like that).
The value of $x^{(n)}$, however,
depends on the value
to which we apply the polynomial.
But the formula requires that the sum
is zero for any value we may fill in for $x$.
We are therefore left with the first option:
the $A_n-B_n$ parts must be zero.
These differences, however, are zero
only if $A_n = B_n$.
That shows that the two representations are equal.\qed

We have proved that powers can be represented
uniquely by factorial polynomials. Here is a list
of representations of powers (starting with $x^1$
in the first line) as factorial polynomials:

\begin{center}
\begin{tabular}{c}
$x^{(1)}$ \\
$x^{(2)} + x^{(1)}$ \\
$x^{(3)} + 3x^{(2)} + x^{(1)}$\\
$x^{(4)} + 6x^{(3)} + 7x^{(2)} + x^{(1)}$\\
$x^{(5)} + 10x^{(4)} + 25x^{(3)} +15x^{(2)} + x^{(1)}$ \\
$x^{(6)} + 15x^{(5)} + 65x^{(4)} + 90x^{(3)} + 31x^{(2)} + x^{(1)}$ \\
$x^{(7)} + 21x^{(6)} + 140x^{(5)} + 350x^{(4)} + 301x^{(3)} + 63x^{(2)} + x^{(1)}$  
\end{tabular}
\end{center}

Those of you who still suffer from triangle paranoia:
you have probably realised that this is already the second
triangle appearing in this section.
When you scroll back to certain triangle-intense chapters,
you will recognise the coefficients above as 
\term{Stirling numbers of the second kind}.
Of course the table above is inverted, because we start
with the largest $k$ in $x^{(k)}$ going down to $k=1$,
while the triangle for the Stirling numbers shows
the coefficients in the order 
$\stirlingTwo{n}{1} \dots \stirlingTwo{n}{n}$.
As a reminder, here they are:

\begin{tabular}{l c c c c c c c c c c c c c c c c c c c c}
1 &   &   &   &   &    &    &    &     &     &   1 &     &     &    &    &    &   &   &   &   &  \\
2 &   &   &   &   &    &    &    &     &   1 &     &   1 &     &    &    &    &   &   &   &   &  \\
3 &   &   &   &   &    &    &    &   1 &     &   3 &     &   1 &    &    &    &   &   &   &   &  \\
4 &   &   &   &   &    &    &  1 &     &   7 &     &   6 &     &  1 &    &    &   &   &   &   &  \\
5 &   &   &   &   &    &  1 &    &  15 &     &  25 &     &  10 &    &  1 &    &   &   &   &   &  \\
6 &   &   &   &   &  1 &    & 31 &     &  90 &     &  65 &     & 15 &    &  1 &   &   &   &   &  \\   
7 &   &   &   & 1 &    & 63 &    & 301 &     & 350 &     & 140 &    & 21 &    & 1 &   &   &   &  
\end{tabular}

Well, we see for some cases that the numbers by which
we scale factorial polynomials so that
they sum up to powers are
Stirling numbers. Can we prove it for all cases?

Let's give it a try with a proof by induction.
Any of the examples above serves as base case that shows that

\begin{equation}
x^n = \stirlingTwo{n}{n}x^{(n)} + \stirlingTwo{n}{n-1}x^{(n-1)} + \dots + \stirlingTwo{n}{1}x^{(1)}.
\end{equation}

We need to show that, if this equation holds for $x^n$, it holds for $x^{n+1}$ that

\begin{equation}
x^{n+1} = \stirlingTwo{n+1}{n+1}x^{(n+1)}
          \stirlingTwo{n+1}{n}x^{(n)} + \dots + 
          \stirlingTwo{n+1}{1}x^{(1)}
\end{equation}

We start with the base case and multiply $x$ on both sides.
On the left-hand side, we get $x^{n+1}$. But what do we get
on the right-hand side?
Well, for each term $x^{(k)}$, we get $xx^{(k)}$.
We have never really thought about what the result of $xx^{(k)}$ is.
We only know that $(x-k)x^{(k)} = x^{(k+1)}$.
So, let us stick to what we know and try to get it in.
A simple way is to express $x$ as an expression with a cameo of $x-k$,
for instance: $x = x-k+k$.
With this expression, we have $(x-k+k)x^{(k)}$.
We distribute $x^{(k)}$ over the sum and get

\[
(x-k)x^{(k)} + kx^{(k)} = x^{(k+1)} + kx^{(k)}.
\]

On the right-hand side, we, hence, get such a sum for each term:

\[
\stirlingTwo{n+1}{n+1}\left(x^{(n+1)} + nx^{(n)}\right) + 
\stirlingTwo{n+1}{n}\left(x^{(n)} + (n-1)x^{(n-1)}\right) + \dots + 
\stirlingTwo{n+1}{1}\left(x^{(2)} + x^{(1)}\right)
\]

We can now regroup the terms, so that the elements with equal
``exponents'' appear together. This yields pairs composed
of the $x^{(k)}$ that was already there and the new one
that we generated by multiplying by $x$:

\[
\begingroup
\renewcommand{\arraystretch}{2}
\begin{array}{rc}
\stirlingTwo{n}{n}x^{(n+1)} & + \\
n\stirlingTwo{n}{n}x^{(n)}  + 
\stirlingTwo{n}{n-1}x^{(n)} & + \\
(n-1)\stirlingTwo{n}{n-1}x^{(n-1)} + 
\stirlingTwo{n}{n-2}x^{(n-1)} & + \\
\dots & + \\
\stirlingTwo{n}{1}x^{(1)} &
\end{array}
\endgroup
\]

\ignore{
\begin{align*}
\stirlingTwo{n}{n}x^{(n+1)} & + \\
n\stirlingTwo{n}{n}x^{(n)}  + 
\stirlingTwo{n}{n-1}x^{(n)} & + \\
(n-1)\stirlingTwo{n}{n-1}x^{(n-1)} + 
\stirlingTwo{n}{n-2}x^{(n-1)} & + \\
\dots & + \\
\stirlingTwo{n}{1}x^{(1)} &
\end{align*}
}

We regroup a bit more, in particular, we
factor $x^{(k)}$ out, so that we obtain
factors that consist only of expressions containing
Stirling numbers in front of the $x$es:

\[
\begingroup
\renewcommand{\arraystretch}{2}
\begin{array}{rlc}
\stirlingTwo{n}{n} & x^{(n+1)} & + \\
\left(n\stirlingTwo{n}{n} + \stirlingTwo{n}{n-1}\right) & x^{(n)} & + \\
\left((n-1)\stirlingTwo{n}{n-1} + \stirlingTwo{n}{n-2}\right) & x^{(n-1)} & + \\ 
\dots & & + \\
\left(2\stirlingTwo{n}{2} + \stirlingTwo{n}{1}\right) & x^{2} & + \\ 
\stirlingTwo{n}{1} & x^{(1)} &
\end{array}
\endgroup
\]

\ignore{
\begin{align*}
\stirlingTwo{n}{n}x^{(n+1)} & + \\
\left(n\stirlingTwo{n}{n} + \stirlingTwo{n}{n-1}\right)x^{(n)} & + \\
\left((n-1)\stirlingTwo{n}{n-1} + \stirlingTwo{n}{n-2}\right)x^{(n-1)} & + \\ 
\dots & + \\
\stirlingTwo{n}{1}x^{(1)} &
\end{align*}
}

You might remember the identity

\begin{equation}
\stirlingTwo{n+1}{k+1} = k\stirlingTwo{n}{k+1} + \stirlingTwo{n}{k},
\end{equation}

which is ``Pascal's rule'' for Stirling numbers of the second kind.
This is exactly what we see in each group! Compare the factors
in front of the first Stirling number that read $n$, $n-1$ and so on
with what you see in the Stirling number in the place of $k$ (\ie\ in the bottom).
For instance, in the formula

\[
\left((n-1)\stirlingTwo{n}{n-1} + \stirlingTwo{n}{n-2}\right)x^{(n-1)}
\]

we have $k = n-1$.

Now, all terms that show this pattern,
can be simplified to

\[
\stirlingTwo{n+1}{k+1}
\]

leaving only the first and the last term.
But since the first and the last are $\stirlingTwo{n}{n}$ and
$\stirlingTwo{n}{1}$ respectively, which are both just 1,
that is not a problem. We get as desired

\begin{equation}
x^{n+1} = \stirlingTwo{n+1}{n+1}x^{(n+1)} +
          \stirlingTwo{n+1}{n}x^{(n)} + \dots + 
          \stirlingTwo{n+1}{1}x^{(1)}\qed
\end{equation}

and that completes the proof.

The following function exploits Stirling numbers
to compute powers by means of factorial polynomials:

\begin{minipage}{\textwidth}
\begin{code}
  stirpow :: Natural -> Poly Natural
  stirpow n = sump [scale (Perm.stirling2 n k) (facpoly k) | k <- [1..n]]
\end{code}
\end{minipage}

This is a lame function, of course.
Powers are not difficult to compute at all,
so why using factorial polynomials in the first place?
More interesting, at least from theoretical perspective,
is the opposite function that, for a given power,
shows the factorial polynomials and the coefficients that
indicate how often each factorial polynomial appears:

\begin{minipage}{\textwidth}
\begin{code}
  fpPowTerms :: Natural -> [(Natural, Poly Natural)]
  fpPowTerms 0  =  [(1,P [1])]
  fpPowTerms n  =  [(Perm.stirling2 n k, facpoly k) | k <- [1..n]]
\end{code}
\end{minipage}

The function, just like the previous one,
makes use of the |stirling2| function that we defined
in the first chapter and so we are obliged to use the concrete type
|Natural|.

Here is a function to test the results:

\begin{minipage}{\textwidth}
\begin{code}
  sumFpPolyTerms :: [(Integer, Poly Integer)] -> Poly Integer
  sumFpPolyTerms = sump . map (uncurry scale)
\end{code}
\end{minipage}

The function, basically, just sums up the list
we pass in scaling the polynomials by their coefficient.
Here is a test for the first 7 powers,\\
|map (sumFpPolyTerms . fpPowTerms) [0..6]|:

\begin{minipage}{\textwidth}
\begin{center}
|P [1]|\\
|P [0,1]|\\
|P [0,0,1]|\\
|P [0,0,0,1]|\\
|P [0,0,0,0,1]|\\
|P [0,0,0,0,0,1]|\\
|P [0,0,0,0,0,0,1]|\\
|P [0,0,0,0,0,0,0,1]|
\end{center}
\end{minipage}

Once we can represent powers by factorial polynomials,
we are able to represent any polynomial by factorial polynomials,
since polynomials are just sums of scaled powers of $x$.
Here is a function that does that:

\begin{minipage}{\textwidth}
\begin{code}
  fpPolyTerms :: Poly Natural -> [(Natural,Poly Natural)]
  fpPolyTerms (P cs)  = [foldl ab p0 p | p <- p2]
    where  p0         = (0,P[0])
           p1         = concat [map (s c) (fpPowTerms k) | (c,k) <- zip cs [0..]]
           p2         = groupBy ((==) `on` snd) (sortOn  (degree . snd) p1)
           ab a b     = (fst a + fst b, snd b)
           s c (n,p)  = (c*n, p)
\end{code}
\end{minipage}

The function looks a bit confusing on the first sight.
It is not too horrible, though.
We start by computing $p1$.
We apply |fpPowTerms| on the exponents
of the original polynomial (|[0..]|)
and multiply the 
coefficients of the original 
(|cs|)
and the coefficients
that tell us how often each factorial polynomial
occurs in the respective power.
The latter is done by function $s$ which is mapped
on the result of |fpPowTerms|.
The result is a list of lists of pairs $(n,p)$,
where $n$ is a |Natural| and $p$ a polynomial.
We concat this list, so we obtain a flat list
of such pairs.

In the next step, we compute $p2$ by sorting
and then grouping
this flat list by the degree of the polynomials.
The result is a list of lists of polynomials 
of equal degree with
differing coefficients.

In the final step we sum the coefficients of
each such groups starting with zero
|p0 = (0,P[0])|. 

We test this function by factoring
arbitrary polynomials into their terms and
summing the result together again:

\begin{minipage}{\textwidth}
|sumFpPolyTerms (fpPolyTerms (P [0,0,0,0,1]))|\\
|P [0,0,0,0,1]|\\
|sumFpPolyTerms (fpPolyTerms (P [1,1,1,1,1]))|\\
|P [1,1,1,1,1]|\\
|sumFpPolyTerms (fpPolyTerms (P [5,4,3,2,1]))|\\
|P [5,4,3,2,1]|\\
|sumFpPolyTerms (fpPolyTerms (P [1,2,3,4,5]))|\\
|P [1,2,3,4,5]|
\end{minipage}

In the next experiment we retrieve the coefficients
for polynomials of the form 

\[
x^n + x^{n-1} + \dots + 1,
\]

\ie\ polynomials with all coefficient equal to 1.

We apply
|map (map fst . fpPolyTerms)|
to the first 7 polynomials of that form,
\ie\ $1$, $x + 1$, $x^2 + x + 1$ and so on
and get

\begin{minipage}{\textwidth}
\begin{center}
|[1]|\\
|[1,1]|\\
|[1,2,1]|\\
|[1,3,4,1]|\\
|[1,4,11,7,1]|\\
|[1,5,26,32,11,1]|\\
|[1,6,57,122,76,16,1]|
\end{center}
\end{minipage}

This again is a triangle and it is the simplest
that we can obtain this way, since the input
coefficients are all 1.
One could think that other polynomials could now
be generated by means of these coefficients
just multiplying the coeficients of the polynomial
with these ones.
Unfortunately, that is too simple.
The coefficients here indicate only
how often each factorial polynomial appears
in the respective polynomial;
they are not coefficients of that polynomial
(which are all 1 anyway).

The sequence as such is the result of a matrix
multiplication (a topic we will study soon) with
one matrix being a lower-left triangle of ones
and the other a lower-left triangle containing
the Stirling numbers of the second kind:

\begin{equation}
\begin{pmatrix}
1 & 0 & 0 & 0 & 0 \\
1 & 1 & 0 & 0 & 0 \\
1 & 1 & 1 & 0 & 0 \\
1 & 1 & 1 & 1 & 0 \\
1 & 1 & 1 & 1 & 1 
\end{pmatrix}
\times
\begin{pmatrix}
1 &  0 &  0 &  0 &  0 \\
1 &  1 &  0 &  0 &  0 \\
1 &  3 &  1 &  0 &  0 \\
1 &  7 &  6 &  1 &  0 \\
1 & 15 & 25 & 10 &  1 
\end{pmatrix}
=
\begin{pmatrix}
1 &  0 &  0 &  0 &  0 \\
2 &  1 &  0 &  0 &  0 \\
3 &  4 &  1 &  0 &  0 \\
4 & 11 &  7 &  1 &  0 \\
5 & 26 & 32 & 11 &  1 
\end{pmatrix}
\end{equation}

\ignore{
with the first column of the triangle presented above missing.
why is it missing???
}

Meanwhile, you may have guessed or even verified
that the coefficients of factorial polynomials,
those appearing in the very first triangle in
this section, are Stirling numbers of the first kind.
But they are special: some are negative.
Indeed, there are two variants of
the Stirling numbers of the first kind:
signed and unsigned. Since we were discussing
combinatorial problems related to permutations,
when we first introduced Stirling numbers,
we did not consider the signed variety.
Here are the signed Stirling numbers 
of the first kind:

\begin{tabular}{l c c c c c c c c c c c c c c c c c c c c}
1 &   &   &   &   &    &      &      &       &     &   1 &      &      &     &    &    &   &   &   &   &  \\
2 &   &   &   &   &    &      &      &       &  -1 &     &    1 &      &     &    &    &   &   &   &   &  \\
3 &   &   &   &   &    &      &      &     2 &     &  -3 &      &    1 &     &    &    &   &   &   &   &  \\
4 &   &   &   &   &    &      &   -6 &       &  11 &     &   -6 &      &   1 &    &    &   &   &   &   &  \\
5 &   &   &   &   &    &   24 &      &   -50 &     &  35 &      &  -10 &     &  1 &    &   &   &   &   &  \\
6 &   &   &   &   &-120&      &  274 &       &-225 &     &   85 &      & -15 &    &  1 &   &   &   &   &  \\   
7 &   &   &   &720&    & -1764&      &  1624 &     &-735 &      &  175 &     &-21 &    & 1 &   &   &   &  
\end{tabular}

The recursive formula to compute these numbers is

\begin{equation}
\stirlingOne{n+1}{k+1} = -n\stirlingOne{n}{k+1} + \stirlingOne{n}{k}.
\end{equation}

Note that, when the first Stirling number, $\stirlingOne{n}{k}$,
on the right-hand side is positive,
then the second, $\stirlingOne{n}{k-1}$, 
is negative. Since we multiply the first by a negative
number, the first term becomes positive, when the Stirling number is
negative and negative otherwise. Therefore, both terms are either
negative or positive and the absolute value of the whole expression
does not change compared
to the unsigned Stirling number.

So, can we prove that the coefficients of factorial polynomials
are Stirling numbers of the first kind?
We prove by induction with any of the above given polynomials
as base case

\begin{equation}\label{eq:stir1fac1}
x^{(n)} = \stirlingOne{n}{n}x^n + 
          \stirlingOne{n}{n-1}x^{n-1} + \dots + 
          \stirlingOne{n}{1}x,
\end{equation}

where the Stirling numbers, here, are to be understood as signed.

We need to prove that, if that equation holds, 
then the following holds as well:

\begin{equation}\label{eq:stir1fac2}
x^{(n+1)} = \stirlingOne{n+1}{n+1}x^{n+1} + 
            \stirlingOne{n+1}{n}x^{n} + \dots + 
            \stirlingOne{n+1}{1}x.
\end{equation}

We start with the observation that

\begin{equation}
x^{(n+1)} = (x-n)x^{(n)}.
\end{equation}

So, we can go from \ref{eq:stir1fac1} to \ref{eq:stir1fac2}
by multiplying both sides of \ref{eq:stir1fac1} by $x-n$.
The right-hand side would then become:

\[
(x-n)\stirlingOne{n}{n}x^n + 
(x-n)\stirlingOne{n}{n-1}x^{n-1} +
      \dots + 
(x-n)\stirlingOne{n}{1}x.
\]

For each term, we distribute the factors over the sum $x-n$:

\[
\stirlingOne{n}{n}x^{n+1} - n\stirlingOne{n}{n}x^n        + \\
\stirlingOne{n}{n-1}x^n   - n\stirlingOne{n}{n-1}x^{n-1}  + \\
\dots                                                     + \\
\stirlingOne{n}{1}x^2     - n\stirlingOne{n}{1}x         
\]

and regroup so that we get pairs of terms 
with equal $x$es:

\[
\begingroup
\renewcommand{\arraystretch}{2}
\begin{array}{lclcclc}
  &\stirlingOne{n}{n}  &x^{n+1}& &                    &       & + \\
-n&\stirlingOne{n}{n}  &x^n    &+&\stirlingOne{n}{n-1}&x^n    & + \\
-n&\stirlingOne{n}{n-1}&x^{n-1}&+&\stirlingOne{n}{n-2}&x^{n-1}& + \\
  &                    &       & &\dots               &       & + \\
-n&\stirlingOne{n}{2}  &x^{2}  &+&\stirlingOne{n}{1}  &x^{2}  & + \\
-n&\stirlingOne{n}{1}  &x      & &                    &       & 
\end{array}
\endgroup
\]

When we factor the $x$es out again, we get

\[
\begingroup
\renewcommand{\arraystretch}{2}
\begin{array}{crlc}
  &\stirlingOne{n}{n}                                      &x^{n+1}& + \\
  &\left(-n\stirlingOne{n}{n} + \stirlingOne{n}{n-1}\right)&x^n    & + \\
  &\left(-n\stirlingOne{n}{n-1}+\stirlingOne{n}{n-2}\right)&x^{n-1}& + \\
  & \dots                                                  &       & + \\
  &\left(-n\stirlingOne{n}{2} + \stirlingOne{n}{1}\right)  &x^{2}  & + \\
  &-n\stirlingOne{n}{1}                                    &x      & 
\end{array}
\endgroup
\]

In each line but the first and the last, we now have
the formula to compute $\stirlingOne{n+1}{k+1}$ and
can simplify all these lines accordingly:

\[
\stirlingOne{n}{n}x^{n+1} + 
\stirlingOne{n+1}{n}x^n  + 
\stirlingOne{n+1}{n-1}x^{n-1} + 
\dots + 
\stirlingOne{n+1}{2}x^{2} 
-n \stirlingOne{n}{1}x
\]

For the first term, the same argument 
we already used before still holds:
$\stirlingOne{n}{n} = \stirlingOne{n+1}{n+1} = 1$.

For the last term, we know that 
$\stirlingOne{n}{1} = \pm(n-1)!$.
We hence see the product 
$(-n)(\pm((n-1)!))$, which is $-(\pm(n!))$.
If, for $n$, the factorial was positive,
it will now be negative. If it was negative,
it will now be positive.
This complies with the signed Stirling numbers
of the first kind and completes the proof.$\qed$

What have we learnt in the last sections?
Well, factorial polynomials have coefficients
that count the number of permutations
that can be expressed by a given number of cycles.
When factorial polynomials are used to represent
powers, we need to scale them by factors
that count the number of ways to partition a set
into a given number of distinct subsets.

Furthermore, we can express any polynomial
by combinations of scaled factorial polynomials
and the coefficients of those
are products of the differences and
the binomial coefficients which count
the number of ways to choose $k$ out of $n$.
``The Lord is subtle'' said Einstein,
``but he is not plain mean''.
That is a quantum of solace for us mere mortals!
Let us go on to see
what is there more to discover.

\ignore{
=> recursive formula:
?> more identities?
?> Taylor's theorem and Taylor's series
}
