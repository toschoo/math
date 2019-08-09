\ignore{
\begin{code}
module Vieta
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
how many real roots the given polynomial has.
Wouldn't it be nice to have a discriminant for
any degree? It turns out, there is such a thing.

At its very heart, it is the product
of the differences of the roots;
for instance, if we have the three
roots $\alpha, \beta, \gamma$, the
core of the discriminant, $d$, is

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
This version of the discriminant, thus, preserves
the property of the na\"ive discriminant for
second-degree polynomials that it is 0
if there a repeated roots.

If at least one of the roots is non-real,
then the product will be non-real too.
This corresponds to the fact that, with
non-real roots, the na\"ive discriminant
is negative.

But the new discriminant tells us even more:
if it is irrational, then all roots are real,
but at least one is irrational. The other way round,
if it is rational, then all roots are rational.

There is a snag though, which can be clearly seen
in the generalised formula: we assume a given
order of the roots -- but there is no such order.
Imagine a polynomial with the roots
$-1, 2, -3, 4$, which is

\[
(x+1)(x-2)(x+3)(x-4) = x^4 - 2x^3 - 13x^2 + 14x + 24.
\]

When I compute the differences of the roots
like this (and note that there are
$\binom{4}{2} = 6$
factors!)

\[
(-1-2)(-1+3)(-1-4)(2+3)(2-4)(-3-4),
\]

I get the result 2100.
But when I compute, just changing the order
of the roots to $2,-1,-3,4$,

\[
(2+1)(2+3)(2-4)(-1+3)(-1-4)(-3-4)
\]

I get -2100.
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

Of course, we have to adopt our principles
to this new form:
there are non-real roots, if the discriminant
is negative and there are irrational roots
if the discriminant is not a perfect square.

You now may ask: is this discriminant actually
the same as the na\"ive one? Or in other words
is the na\"ive discriminant a special case
of this general form?

Let us look at a second-degree polynomial,
for example

\[
(x-2)(x+3) = x^2 + x - 6.
\]

The na\"ive discriminant ($b2 - 4ac$) is 

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

Is this just by chance or can we prove it?
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
the latter out, we get

\[
\alpha^2 + 2\alpha\beta + \beta^2.
\]

When we subtract $4\alpha\beta$, we obtain

\[
\alpha^2 - 2\alpha\beta + \beta^2,
\]

which clearly is $(\alpha-\beta)^2$.

According to Vieta's formulas, however,
$\alpha+\beta$ is $-\frac{b}{a}$
and $\alpha\beta$ is $\frac{c}{a}$.
So, we have

\begin{equation}
(\alpha+\beta)^2 - 4\alpha\beta = \left(-\frac{b}{a}\right)^2 - 4\frac{c}{a}.
\end{equation}

Since the right-hand side is $\frac{b^2}{a^2} - 4\frac{c}{a}$,
we obtain the desired result just by multiplying both sides by $a^2$
(and bringing the left-hand side back to its original form):

\begin{equation}
a^2(\alpha-\beta)^2 = b^2 - 4ac.\qed
\end{equation}

In the general form, the discriminant can be computed as

\begin{equation}
a^{2d-2}\prod_{i<j}{(x_i - x_j)^2},
\end{equation}

where $a$ is the leading coefficient and $d$ the degree
of the polynomial. For $d=2$, this is $2\times 2 - 2 = 2$.
For higher degrees this must be adapted and the common
expression is that funny $2d-2$.

Of course, we again have to adapt our principles
to this new formula. To say anything about
irrationality of the roots,
we need to divide the discriminant by $a^{2d-2}$.
If (and only if) the result is a perfect square,
the polynomial has only rational roots.
Note that we do not need to adopt the principle
to decide whether there are non-real roots.
Since $2d-2$ is always even, $a$ raised to such a power
is always positive. It will, hence, not affect
the sign of the discriminant. Therefore, if (and only if)
the discriminant is negative, there are non-real roots. 

But now comes the hard question:
the use of the discriminant is to tell us something
about the roots. But from what we see here,
we need to know the roots to compute the discriminant.
That is not very useful! The so called ``na\"ive''
discriminant is not too na\"ive at the end! At least,
it has a function!

Well, here comes the esoteric part of this section.
There is in fact a way to compute the discriminant
without knowing the roots. What we need to do it
is to compute the \term{resultant} of the polynomial
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
