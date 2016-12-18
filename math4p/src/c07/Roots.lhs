\ignore{
module Roots
where
  import DMachine
}

In the previous sections, we looked at the results,
when applying polynomials to given values.
That is, we applied a polynomial $\pi(x)$ to
a given value (or sequence of values) for $x$ 
and studied the result $y = \pi(x)$.
Now we are turning this around. We will look at
a given $y$ and ask which value $x$ would
create that $y$. In other words, 
we look at polynomials as equations of the form:

\begin{equation}
a_nx^n + a_{n-1}x^{n-1} + \dots + a_0 = a
\end{equation}

and search for ways to solve such equations.
In the focus of this investigation is usually
the special case $a=0$, \ie\:

\begin{equation}
a_nx^n + a_{n-1}x^{n-1} + \dots + a_0 = 0.
\end{equation}

The values for $x$ fulfilling this equation
are called the \term{root}s of the polynomial.
A trivial example is $x^2$, whose root is 0.
A slightly less trivial example is $x^2 - 4$,
whose roots are $x_1 = -2$ and $x_2 = 2$, since

\[
(-2)^2 - 4 = 4 - 4 = 0
\]

and

\[
2^2 - 4 = 4 - 4 = 0.
\]

Note that these examples are polynomials of even degree.
Polynomials of even degree do not need to have any roots.
Since even powers are always positive (or zero), negative values
are turned into positive numbers and, since the term of highest
degree is even, the whole expression may always be positive.
This is true for the polynomial $x^2 + 1$. Since all negative values
are transformed into positive values by $x^2$, the smallest value
that we can reach is the result for $x=0$, which is $0+1=1$.

On the other hand, even polynomials may have negative values,
namely when they have terms with negative coefficients that, 
for smaller numbers,
result in numbers that are greater than those resulting from
the term of highest degree. 
The polynomial $x^2 - 4$, for instance, is negative
in the interval $]-2\dots 2[$. It, therefore, must have two roots:
one at -2, where the polynomial results become negative, and the other at 2,
where the polynomial results become positive again.

Odd polynomials, by contrast, usually have negative values, because
the term with the highest degree may result in a negative or a
positive number depending on the signedness of the input value
and that of the coefficient.
The trivial polynomial $x^3$, for instance, is negative for
negative values and positive for positive values. The slightly
less trivial polynomial $x^3 + 9$ has its root at -3, while
$x^3 - 9$ has its root at 3.

In summary, we can say that even polynomials do not necessarily
have negative values and, hence, do not need to have a root.
Odd polynomials, on the other hand, usually have both, negative
and positive values, and, hence, must have a root.

Those are strong claims. They are true, because polynomials
belong to a specific set of \term{functions}, 
namely \term{continuous} functions.
That, basically, means that they have no \emph{holes}, \ie\
for any value $x$ of a certain number type there is a result $y$
of that number type. For instance, when the coefficients of the
polynomial are all integers and the $x$-value is an integer,
then the result is an integer, too. When the polynomial is defined
over a field (all coefficients are part of that field and
the values to which we apply the polynomial lie in that field),
then the result is in that field, too. Rational polynomials,
for instance, have rational results. 
Real polynomials have real results.

Furthermore, the function does not ``jump'', \ie\ the results
grow with the input values -- not necessarily
at the same rate, in fact, for polynomials of degree greater than 1,
the result grows much faster than the input -- but the growth
is regular.

These properties appear to be ``natural'' at the first sight.
But there are functions that do not fulful these criteria.
In the next chapter, when we properly define the term \term{function},
we will actually see functions with holes and jumps.

The reason that polynomials behave regularily is that we only
use basic arithmetic functions in their definition: we add, multiply
and raise to powers. 
Those operations together with the integers form monoids and,
with rational and real numbers, they form groups.
Both, monoids and groups, are closed over their base set.
We can therefore be sure that, for any input value from the base set,
the result is in the same base set, too.

Furthermore, the form of polynomials guarantees that they develop
in a certain way. For very large numbers (negative or positive), 
it is the term with the greatest exponents, \ie\ the degree 
of the polynomial, that most significantly determines the
outcome, that is, the result for very large numbers
approaches the result for the term with the largest exponent. 
For smaller values, however, the terms of lower degree have
stronger impact. The terms ``large'' and ``small'', here, 
must be understood relative to the coefficients. If the coefficients
are very large, the values to which the polynomial is applied
must be even larger to approach the result for the first term.

There are also polynomials with a quite confusing behaviour that
make it hard to guess the roots, for instance, Wilkinson's polynomial
named for James Hardy Wilkinson (1919 -- 1986), an American mathematician
and computer scientist. The Wilkinson polynomial is defined as

\begin{equation}
w(x) = \prod_{i=1}^{20}{(x-i)}.
\end{equation}

We can generate it in terms of our polynomial type as

\begin{minipage}{\textwidth}
\begin{code}
  wilkinson :: (Num a, Enum a, Show a, Eq a) => Poly a
  wilkinson = prodp mul [P [-i,1] | i <- [1..20]]
\end{code}
\end{minipage}

It looks like this:

|P [|\\
|  2432902008176640000,-8752948036761600000,13803759753640704000,      |\\
|  -12870931245150988800,8037811822645051776,-3599979517947607200,     |\\
|  1206647803780373360,-311333643161390640,63030812099294896,          |\\
|  -10142299865511450,1307535010540395,-135585182899530,11310276995381,|\\
|  -756111184500,40171771630,-1672280820,53327946,-1256850,20615,-210,1]|

The first terms are

\[
x^{20} - 210x^{19} + 20615x^{18} - 1256850x^{17} \dots
\]

When we apply Wilkinson's polynomial to the integers $1\dots 10$, we see:

|0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2432902008176640000,|\\
|51090942171709440000,562000363888803840000,4308669456480829440000,|\\
|25852016738884976640000|,

which looks very confusing. When we try non-integers, we see

|apply wilkinson 0.9| is $1.7213\dots$\\
|apply wilkinson 1.1| is $-8.4600\dots$\\
|apply wilkinson 1.9| is $-8.1111\dots$\\
|apply wilkinson 2.1| is $4.9238\dots$\\

As we see, the results switch sign at the integers or,
more precisely, at the integers in the interval $[1\dots 20]$,
which are the roots of Wilkinson's polynomial.
Looking at the factors of the polynomial

\[
(x-1)(x-2)\dots (x-20),
\]

this result is much less surprising, since, obviously,
when any of these factors becomes 0, then the whole
expression becomes 0. So, for the value $x=3$, we would have

\[
2 \times 1 \times 0 \times \dots \times -17 = 0.
\]

The results when applying the polynomial, however,
look quite irregular and, on the first sight,
completely unrelated to the coefficients.
When we say that polynomials show a regular behaviour,
that must be taken with a grain of salt.
Anyway, that they behave like this 
gives rise to a number of simple
methods to find roots based on approximation,
at least when we start with a fair guess,
which requires some knowledge about the rough shape
of the polynomial in the first place.

These methods can be split into two major groups:
\term{bracketing} methods and \term{open} methods.
Bracketing methods start with two distinct values
somewhere on the ``left'' and the ``right'' of
the root. Bracketing methods, hence, require a
pre-knowledge about where, more or less, a root
is located. We then choose two values that limit
this interval on the lower and on the upper side.

The simplest variant of bracketing is the \term{bisect}
algorithm. It is very similar to Heron's method
to find the square root of a given number.
We start with two values $a$ and $b$ and, on each step,
we compute the average $(a+b)/2$ and substitute
either $a$ or $b$ by this value depending on the side
the value is located relative to the root.
Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  bisect ::  (Num a, Eq a, Ord a, Fractional a, Show a)
             => Poly a -> a -> a -> a -> a
  bisect p t a b  |  abs fc < abs t           =  c
                  |  signum fc == signum fa   =  bisect p t c b 
                  |  otherwise                =  bisect p t a c 
    where  fa  =  apply p a
           fb  =  apply p b
           fc  =  apply p c
           c   =  (a+b)/2
\end{code}
\end{minipage}

The function receives four arguments.
The first is the polynomial.
The second is a tolerance.
When we reach a result that is smaller
than the tolerance, we return the result.
$a$ and $b$ are the starting values.

We distinguish three cases:
\begin{itemize}
\item The result for the new value, $c$, 
      is below the tolerance threshold.
      In this case, $c$ is sufficiently close
      to the root and we yield this value.
\item the sign of the result for the new value
      equals the sign of $a$. Then we replace
      $a$ by $c$.
\item the sign of the result for the new value
      equals the sign of $b$. In this case,
      we replace $b$ by $c$.
\end{itemize}

We try |bisect| on the polynomial $x^2$ with
the initial guess $a=-1$ and $b=1$ (because we
assume that the root should be close to 0) and
a tolerance of 0.1:

|bisect (P [0,0,1]) 0.1 (-1) 1|

and see the correct result |0.0|.

For the polynomial $x^2 - 4$, which has two roots,
we try 

|bisect (P [-4,0,1]) 0.1 (-3) (-1)|,

which yields |-2| and 

|bisect (P [-4,0,1]) 0.1 1 3|,

which yields |2|.

With Wilkinson's polynomial, however,
we get a surprise:

|bisect wilkinson 0.1 0.5 1.5|,

for which we expect to find the root 1.
But the function does not return.
Indeed, when we try |apply wilkinson 1.0|, we see

|1148.0|,

a somewhat surprising result.
Wilkinson used this polynomial to demonstrate
the sensivity of coefficients to small differences
in the input values. Using Haskell real numbers,
The computation leads to a loss of precision
in representing the terms. Indeed, considering
terms raised to the $20^{th}$ power and multiplied
by large coefficients, the number 1148 appears to
be a tiny inprecision.

We can work around this, using rational numbers:

|apply wilkinson (1%1)|

gives without any surprise |0%1|.
So, we try

|bisect wilkinson (1%10) (1%2) (3%2)|

and get the correct result |1%1|.
The function with this parameters
returns almost instantanious. That is
because the average of 0.5 and 1.5 is already 1.
The function finds the root in the first step.
A more serious challenge is

|bisect wilkinson (1%10) (1%3) (3%2)|,

which needs more than one recursion.
The function, now, runs for a short while and
comes up with the result

|1729382256910270463 % 1729382256910270464|,

which is pretty close to 1 and, hence,
the correct result.

The most widely known open method is Newton's method,
also called Newton-Raphson method.
It was first developed by Newton in about 1670
and, then, by Joseph Raphson in 1690.
Newton's version was probably not known to Raphson,
since Newton did not publish his work.
Raphson's version, on the other hand, is
simpler and, therefore, usually preferred.

Anyway, the method starts with only one approximation
and is therefore not a bracketing method.
The approximation is then applied to the polynomial $\pi$
and the derivative of that polynomial, $\pi'$.
Then, the quotient of the results, $\frac{\pi(x)}{\pi'(x)}$ 
is computed and subtracted from the initial guess.
Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  newguess ::  (Num a, Eq a, Ord a, Enum a, Fractional a) 
               => Poly a -> Natural -> a -> a -> a
  newguess p m t a  |  abs pa < t  =  a
                    |  m <= 0      =  a
                    |  otherwise   =  newguess p (m-1) t (a-pa/p'a)
    where  p'   =  derivative (*) p
           pa   =  apply p a
           p'a  =  apply p' a
\end{code}
\end{minipage}

The function receives four parameters.
The polynomial |p|, the natural number |m|,
the tolerance |t| and the initial guess |a|.
The natural number |m| is a delimiter.
It is not guaranteed that the value increases
in precision with always more repetitions.
It may get worse at some point.
It is therefore useful to restrict the number
of iterations.

The function terminates when we have 
reached either the intended precision or 
the number of repetitions, $m$.
Otherwise, we repeat with $m-1$ and with
$a - \frac{\pi(a)}{\pi'(a)}$.

For the polynomial $x^2 - 4$, we call first

|newguess (P [-4,0,1]) 10 0.1 1|

and get $2.00069\dots$, which is very close
to the known root 2.
For the other root we call

|newguess (P [-4,0,1]) 10 0.1 (-1)|

and get the equally close result $-2.00069\dots$
For the Wilkinson polynomial, we call

|newguess wilkinson 10 (0.0001) 1.5|

and get $1.99999\dots$, which is very close
to the real root 2. We can further improve
precision by increasing the number of iterations:

|newguess wilkinson 20 (0.0001) 1.5|

The difference is at the $12^{th}$ decimal digit.

Note that the Newton-Raphson method
is not only more precise (that is: converges earlier
with a good result), but also more robust against
real representation imprecision.

To understand why this method works at all,
we need to better understand what the derivative is.
We will come back to this issue in the next chapter.
In the strict sense, the derivative does not belong
here anyway, since the concept of derivative is
analysis, not algebra. Both kinds of methods,
the bracketing and the open methods, in fact, come
from numerical analysis.
They do not have the ``look and feel'' of algebraic
methods. So, how would an algebraist tackle the
problem of finding the roots of a polynomial?

One possibility is factoring.
Polynomials may be represented as the product
of their factors (just like integers).
We have experienced with Wilkinson's polynomial
that the factor representation may be much more
convenient that the usual representation with
coefficients. Wilkinson's polynomial expressed
as a product was just

\begin{equation}
w(x) = \prod_{i=1}^{20}{(x-i)},
\end{equation}

\ie: $(x-1)(x-2)\dots (x-20)$.

As for all products, when one of the factors
is zero, then the whole product becomes zero.
For the root problem, this means that, 
when we have the factors, we can find a value
for $x$, so that any of the factors becomes zero
and this value is then a root, because it makes
the product and, as such, the whole polynomial zero.
Any integer in the range $[1\dots 20]$ would make
one of the factors of Wilkinson's polynomial zero.
The integers $[1\dots 20]$ are therefore the roots
of Wilkinson's polynomial.

Factoring polynomials, however, is an advanced
problem in its own right and we will dedicate
the next section to its study. Anyway, what
algebraists did for centuries was to find
formulas that would yield the roots for any kind
of polynomials. In some cases they succeeded,
in particular for polynomials of degrees less
than 5. For higher degrees, there are not such
formulas. This dicovery is perhaps much more
important than the single formulas developed
over the centuries for polynomials of the first
four degrees. In fact, the concepts that led
to the discovery are the foundations of modern
(and postmodern) algebra. 

But first things first. To understand why there
cannot be general formulas for solving polynomials
higher degrees, we need to understand polynomials
much better. First, we will look at the formula
to solve polynomials of the second degree.
Note that we skip the first degree, since 
finding the roots of polynomials
of the first degree is just solving linear
equations.
