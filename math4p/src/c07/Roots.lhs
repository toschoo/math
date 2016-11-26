\ignore{
module Roots
where

  import DMachine

}

In the previous sections, we looked at the results,
when applying polynomials to given values.
That is, we applied a polynomial $\pi(x)$ to
a given value (or sequence of values)for $x$ 
and studied the result $y$: $y = \pi(x)$.
Now we are turning this around. We will look at
a given $y$ and ask which value $x$ would
create that $y$. In other words, 
we look at polynomials as equations of the form:

\begin{equation}
a_nx^n + a_{n-1}x^{n-1} + \dots + a_0 = a
\end{equation}

and try to find ways to solve such equations.
In the focus of such equations, usually
is the special case $a=$, \ie\:

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
Since even powers are always positive, negative values
are turned into positive numbers and, since the term of highest
degree is even, the whole expression may always be positive.
This is true for the polynomial $x^2 + 1$. Since all negative values
are transformed into positive values by $x^2$, the smallest value
that we can reach is the result for $x=0$, which is 0 for $x^2$.
When we then add 1, the result is positive.

On the other hand, even polynomials may have negative values,
namely when they have terms with negative coefficients that, 
for smaller numbers,
result in numbers that are greater than those resulting from
the term of highest degree. 
The polynomial $x^2 - 4$, for instance, is negative
in the interval $]-2\dots 2[$. It, therefore, must have two roots:
one, where the polynomial results become negative, and the other,
where the polynomial results become positive again.

Odd polynomials, by contrast, usually have negative values, because
the term with the highest degree may result in a negative or a
positive number depending on the signedness of the input value.
The trivial polynomial $x^3$, for instance, is negative for
negative values and positive for positive values. The slightly
less trivial polynomial $x^3 + 9$ has its root at -3, while
$x^3 - 9$ has its root at 3.

In summary, we can say that even polynomials do not necessarily
have negative values and, hence, do not need to have a root.
Odd polynomials, on the other hand, usually have both, negative
and positive values, and, hence, must have a root.

Those are strong claims. They are true, because polynomials
are specific type of \term{functions}; they are \term{continuous}.
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

Furthermore, the function does not ``jump'', \ie\ the growth
of the results growth with the input values -- not necessarily
at the same rate, in fact, for polynomials of degree greater 1,
the result grows much faster than the input -- but the growth
is regular.
These properties appear to be ``natural'' at the first sight.
There are functions that do not fulful these criteria.
In the next chapter, when we properly define the term \term{function},
we will actually see functions with holes and jumps.

The reason that polynomials behave regularily is that we only
use basic arithmetic functions in their definition: we add, multiply
and raise to powers. Those operations are possible for all numbers
in a given field and will therefore always yield a result in that field.
The fact that they behave like this gives rise to a set of simple
methods to find roots based on approximation.

\ignore{
 - most root-finding algorithms are analysis not algebra
   overview (https://en.wikipedia.org/wiki/Root-finding_algorithm):
   + bracketing
   + newton and similar

 - factoring (https://en.wikipedia.org/wiki/Factorization_of_polynomials)
   + simple methods
 - quadratic formula
}
