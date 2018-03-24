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
namely when they have terms with coefficients that, 
for small absolute values,
result in negative numbers whose absolute value
is greater than those resulting from
the term of highest degree. 
The polynomial $x^2 - 4$, once again, is negative
in the interval $]-2\dots 2[$. It, therefore, must have two roots:
one at -2, where the polynomial results become negative, and the other at 2,
where the polynomial results become positive again.

Odd polynomials, by contrast, usually have negative values, because
the term with the highest degree may result in a negative or a
positive number depending on the signedness of the input value
and that of the coefficient.
The trivial polynomial $x^3$, for instance, is negative for
negative values and positive for positive values. The slightly
less trivial polynomial $x^3 + 27$ has a root at -3, while
$x^3 - 27$ has a root at 3.

In summary, we can say that even polynomials do not necessarily
have negative values and, hence, do not need to have a root.
Odd polynomials, on the other hand, usually have both, negative
and positive values, and, hence, must have a root.

Those are strong claims. They are true, because polynomials
belong to a specific class of \term{functions}, 
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
But there are functions that do not fulfil these criteria.
In the next chapter, when we properly define the term \term{function},
we will actually see functions with holes and jumps.

The reason that polynomials behave regularily is that we only
use basic arithmetic operations in their definition: we add, multiply
and raise to powers. 
All those operations are closed, \ie\ their results lie
in the same fields as their inputs. 

Furthermore, the form of polynomials guarantees that they develop
in a certain way. For very large numbers (negative or positive), 
it is the term with the greatest exponent, \ie\ the degree 
of the polynomial, that most significantly determines the
outcome, that is, the result for very large numbers
approaches the result for the term with the largest exponent. 
For smaller values, however, the terms of lower degree have
stronger impact. The terms ``large'' and ``small'', here, 
must be understood relative to the coefficients. If the coefficients
are very large, the values to which the polynomial is applied
must be even larger to approach the result for the first term.

There are polynomials whose behaviour is hard to predict,
for instance, \term{Wilkinson's polynomial}
named for James Hardy Wilkinson (1919 -- 1986), an American mathematician
and computer scientist. The Wilkinson polynomial is defined as

\begin{equation}
w(x) = \prod_{i=1}^{20}{(x-i)}.
\end{equation}

It is thus a factorial polynomial, namely $x^{(21)}$.
We can generate it in terms of our polynomial type as

\begin{minipage}{\textwidth}
\begin{code}
  wilkinson :: (Num a, Enum a, Show a, Eq a) => Poly a
  wilkinson = prodp mul [P [-i,1] | i <- [1..20]]
\end{code}
\end{minipage}

It looks like this:

\begin{minipage}{\textwidth}
|P [|\\
|  2432902008176640000,-8752948036761600000,13803759753640704000,      |\\
|  -12870931245150988800,8037811822645051776,-3599979517947607200,     |\\
|  1206647803780373360,-311333643161390640,63030812099294896,          |\\
|  -10142299865511450,1307535010540395,-135585182899530,11310276995381,|\\
|  -756111184500,40171771630,-1672280820,53327946,-1256850,20615,-210,1]|
\end{minipage}

The first terms are

\[
x^{20} - 210x^{19} + 20615x^{18} - 1256850x^{17} \dots
\]

When we apply Wilkinson's polynomial to the integers $1\dots 25$, we see:

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

When we look at the coefficients, however,
the results
look quite irregular and, on the first sight,
completely unrelated.
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
is located. 

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
When, on applying the polynomial,
we get a result that is smaller
than the tolerance, we return the obtained $x$ value.
$a$ and $b$ are the starting values.

\begin{minipage}{\textwidth}
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
\end{minipage}

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
be a tiny imprecision.

We can work around this, using rational numbers:

|apply wilkinson (1%1)|

gives without any surprise |0%1|.
So, we try

|bisect wilkinson (1%10) (1%2) (3%2)|

and get the correct result |1%1|.
The function with this parameters
returns almost instantly. That is
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

Open methods need only one value.
The most widely known open method is Newton's method,
also called Newton-Raphson method.
It was first developed by Newton in about 1670
and then, in 1690, again by Joseph Raphson.
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
It is therefore useful --
and a lesson learnt from experimenting with
|bisect| -- to restrict the number
of iterations.

The function terminates when we have 
reached either the intended precision or 
the number of repetitions, $m$.
Otherwise, we repeat with $m-1$ and
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
convenient than the usual representation with
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
and this value is then a root.
Any integer in the range $[1\dots 20]$ would make
one of the factors of Wilkinson's polynomial zero.
The integers $[1\dots 20]$ are therefore the roots
of this polynomial.

Factoring polynomials, however, is an advanced
problem in its own right and we will dedicate
some of the next sections to its study. Anyway, what
algebraists did for centuries was searching
formulas that would yield the roots for any kind
of polynomials. In some cases they succeeded,
in particular for polynomials of degrees less
than 5. For higher degrees, there are no such
formulas. This discovery is perhaps much more
important than the single formulas developed
over the centuries for polynomials of the first
four degrees. In fact, the concepts that led
to the discovery are the foundations of modern
(and \term{postmodern}) algebra. 

But first things first. To understand why there
cannot be general formulas for solving polynomials
of higher degrees, we need to understand polynomials
much better. First, we will look at the formula
to solve polynomials of the second degree.

Polynomials of the first degree are just 
linear equations of the form

\begin{equation}
ax + b = 0.
\end{equation}

We can easily solve by subtracting $b$ and dividing
by $a$:

\begin{equation}
x = -\frac{b}{a}.
\end{equation}

In Haskell, this is just:

\begin{minipage}{\textwidth}
\begin{code}
  solve1 :: (Fractional a) => Poly a -> [a]
  solve1 (P [b,a]) = [-b/a]
\end{code}
\end{minipage}

Note the order of $a$ and $b$ in the definition of the polynomial.
This is consistent with the equation we gave above,
since, in our definition of polynomials in Haskell,
the head of the list of the coefficients is the coefficient
of $x^0$.

Polynomials of the second degree can be solved with a technique
we already used in the previous chapter, namely
\term{completing the square}. We will now apply this technique
on symbols and, as a result, will obtain a formula that can
be applied on any polynomial of second degree.
We start with the equation

\begin{equation}
ax^2 + bx + c = 0.
\end{equation}

We subtract $c$ and divide by $a$ obtaining:

\begin{equation}
x^2 + \frac{b}{a}x = -\frac{c}{a}.
\end{equation}

Now, we want to get a binomial formula on the left-hand side
of the equation. A binomial formula has the form:

\begin{equation}
(\alpha + \beta)^2 = \alpha^2 + 2\alpha\beta + \beta^2.
\end{equation}

When we set $\alpha = x$, we have on the right-hand side:

\[
x^2 + 2\beta x + \beta^2.
\]

In our equation, we see the term $\frac{b}{a}x$ at the position
where, here, we have $2\beta x$.
We, therefore, have $\frac{b}{a} = 2\beta$ 
and $\beta = \frac{b}{2a}$.
The missing term, hence, is 
$\left(\frac{b}{2a}\right)^2 = \frac{b^2}{4a^2}$.
We add this term to both sides of the equation:

\begin{equation}
x^2 + \frac{b}{a}x + \frac{b^2}{4a^2} = -\frac{c}{a} + \frac{b^2}{4a^2}.
\end{equation}

We can simplify the right-hand side of the equation a bit:

\begin{equation}
x^2 + \frac{b}{a}x + \frac{b^2}{4a^2} = \frac{b^2-4ac}{4a^2}.
\end{equation}

To get rid of all the squares, we now take the square root
on both sides of the equation. Since we have a binomial
formula on the left-hand side, we get:

\begin{equation}
x + \frac{b}{2a} = \frac{\pm\sqrt{b^2-4ac}}{2a}.
\end{equation}

When we solve this equation for $x$, we get

\begin{equation}
x = \frac{-b \pm\sqrt{b^2-4ac}}{2a}.
\end{equation}

Voilà, this is the formula for solving polynomials of
the second degree.

We immediately see that polynomials with rational coefficients
may have irrational roots, because the solution involves
a square root, which leads either to an integer or
an irrational number.

We also see that polynomials of the second degree
may have two roots, namely the result of the expression
on the right-hand side, when we take the positive root,
\ie\ 

\[
\frac{-b+\sqrt{b^2-4ac}}{2a},
\]

and the one, when we take the negative root, \ie

\[
\frac{-b-\sqrt{b^2-4ac}}{2a}.
\]

However, when the square root is zero
then it makes no difference whether we add
or subtract. The square root becomes zero, when
the expression
$b^2-4ac$ is zero. So, when this expression
is zero, there is only one root.

But there is one more thing:
When the expression $b^2-4ac$ is negative,
then we will try to take a square root from
a negative term and that is not defined,
since a number multiplied by itself is always
positive, independent of that number itself being
positive or negative.

Well, it is not defined for \emph{real} numbers.
When we assume that $\sqrt{-1}$
is actually a legal expression, we could
\term{extend} the field of the real numbers
to another, more complex field that
includes this beast. 
We have already looked at how to extend fields
in the previous chapter and we will indeed
do this extension for $\mathbb{R}$ to create
the \term{complex numbers}, $\mathbb{C}$.
In that field, the root of a negative number
is indeed defined and we have a valid result
in both cases.

For instance the polynomial $x^2 + 1$ is never
negative and, therefore, has no roots in $\mathbb{R}$.
But when we assume that there is a number, say, $i$,
for which $i^2=-1$, then this value $i$ would
make the polynomial zero: $i^2 + 1 = -1 + 1 = 0$.

But, again, first things first.
The expression $b^2-4ac$ is called the
\term{discriminant} of the polynomial,
because it determines how many roots
there are: 2, 1 or (in $\mathbb{R}$) none.
The discriminant for polynomials
with real coefficients
may be implemented in Haskell as follows:

\begin{minipage}{\textwidth}
\begin{code}
  dis :: (Num a) => Poly a -> a
  dis (P [c,b,a]) = b^2 - 4*a*c
\end{code}
\end{minipage}

On top of this we implement a \term{root counter}:

\begin{minipage}{\textwidth}
\begin{code}
  countRoots :: (Num a, Ord a) => Poly a -> Int
  countRoots p  |  dis p > 0  = 2
                |  dis p < 0  = 0
                |  otherwise  = 1
\end{code}
\end{minipage}

The polynomial $x^2 + 4$, for instance,
has no roots in $\mathbb{R}$, since

|countRoots (P [4,0,1])|

gives 0.
Indeed $0^2 - 4\times 1\times 4$ is negative.

The polynomial $x^2 - 4$, by contrast has

|countRoots (P [-4,0,1])|,

2 roots.
Indeed, $0^2 - 4\times 1\times -4$ is
$0 + 16$ and, hence, positive.

The polynomial $x^2$ has 1 root,
since

|countRoots (P [0,0,1])|

is 1.
Indeed, $0^2 - 4\times 1 \times 0$ is 0.

We finally implement the solution for
polynomials of the second degree:

\begin{minipage}{\textwidth}
\begin{code}
  solve2 :: (Floating a, Fractional a, Real a) => Poly a -> [a]
  solve2 p@(P [c,b,a])  |  dis p < 0  = []
                        |  x1 /= x2   = [x1,x2]
                        |  otherwise  = [x1]
    where  d   = sqrt (dis p)
           x1  = (-b + d) / 2*a
           x2  = (-b - d) / 2*a
\end{code}
\end{minipage}

When we call |solve2 (P [0,0,1])|,
that is, we solve the polynomial $x^2$,
we get the root |[0]|, which is one root
as predicted.

To solve the polynomial $x^2 + 4$, we call
|solve2 (P [4,0,1])| and get |[]|; as predicted,
this polynomials has no roots. 
It is everywhere positive.

The polynomial $x^2 - 4$, by contrast,
shall have two roots. We call
|solve2 (P [-4,0,1])| and get |[2,-2]|.
When we check this by applying the polynomial
to 2 and -2 like |map (apply (P [-4,0,1])) [2,-2]|,
we get |[0,0]|.

What about the polynomial $-x^2 - x + 1$, which
we factored in the previous chapter?
We try |solve2 (P [1,-1,-1])| and get

|[-1.618033988749895,0.6180339887498949]|,

which is $-\Phi$ and $-\Psi$, just as we saw before.

Which polynomial has the roots $\Phi$ and $\Psi$?
Well, let us try:

|mul (P [-phi,1]) (P [-psi,1])|

yields: 

|P [1.0,-2.23606797749979,1.0]|,

which corresponds to $x^2 -\sqrt{5} + 1$.
The coefficients are
1 for $x^2$, $-\sqrt{5}$ for $(-\Phi-\Psi)x$
and 1 for $(-\Phi)(-\Psi)$.

What is the result for the ``simple''
polynomial $x^2 + x + 1$?
We try with |solve2 (P [1,1,1])| and get
|[]| -- the empty list.
Indeed, $1^2 - 4\times 1\times 1$ is negative!

Let us pretend to be optimistic like the ``reckoning masters''
in the 15 and 16 hundreds. We already have a formula to compute
the roots for polynomials of the first two degrees.
It will certainly be easy to find formulas for the remaining
(infinitely many) degrees. We can then define a function
of the form:

\begin{minipage}{\textwidth}
\begin{code}
  solve :: (Fractional a, Floating a, Real a) => Poly a -> [a]
  solve p  |  degree p == 0 = []
           |  degree p == 1 = solve1 p
           |  degree p == 2 = solve2 p
           |  degree p == 3 = undefined
           |  degree p == 4 = undefined
           |  degree p == 5 = undefined
\end{code}
\end{minipage}

and so on.
With this optimism, our goal is to replace
the |undefined| implementations by functions
of the form |solve3|, |solve4|, \etc\
We come back to this endevour in a later chapter.

