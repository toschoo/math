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
any degree? It turns out, there is such a thing.
At its very heart, it is the product
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
factors!), \ie

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
1^2 - 4(-6) = 1 + 24 = 25.
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

Is this just by chance or can we prove that it is always like this?
We need to prove, for the case of a second-degree polynomial,
that

\begin{equation}
a^2(\alpha-\beta)^2 = b^2 - 4ac,
\end{equation}

where $a, b, c$ are the coefficients and
$\alpha, \beta$ are, as usual, the roots
the polynomial.

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

But now comes the hard question:
the discriminant shall tell us something
about the roots. But from what we see here,
we need to know the roots to compute the discriminant in the first place.
That is not very useful! The so called ``na\"ive''
discriminant is not too na\"ive after all! At least,
it has a function!

Well, here comes the esoteric part of this section.
There is in fact a way to compute the discriminant
without knowing the roots. There are, in fact,
several ways. What we need, however,
is the \term{resultant} of the polynomial
and its derivative.

\ignore{
- discriminants in general terms:
https://www.youtube.com/watch?v=AL5DdIJ9EQU
- some proofs with Vieta's formulas (see
  https://de.wikipedia.org/wiki/Diskriminante)
- Resultant
- how to compute the resultant?
- determinant of the sylvester matrix
- pseudo-remainder sequence
- subresultant sequence
- how to compute discriminants:
  (-1)^(d*(d-1)/2)*RES(f,f') / lc(f)
}
