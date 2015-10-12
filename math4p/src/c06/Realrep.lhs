\ignore{
\begin{code}
module Realrep
where
  import Natural
  import Zahl
  import Quoz
\end{code}
}

The set of real numbers $\mathbb{R}$ is
the union of the rational and the irrational numbers.
When we write real numbers on paper,
we use the decimal notation.
A number in decimal notation corresponds to 
an ordinary integer terminated by a dot
called the decimal point;
this integer corresponds to the part of the real number
greater 1 or 0 of course.
After the dot a stream of digits follows,
which is not necessarily a number in the common sense,
since it may start with zeros, \eg\ 0.0001.
In fact, one could say that the part after the dot
corresponds to a a reversed integer,
since the zero following this number
have no impact on the value of the whole expression,
\ie\ $0.10 = 0.1$.

Any rational number can be expressed in this system.
An integer corresponds just to the part before the dot:
$1.0 = 1$. A fraction like $\frac{1}{2}$ is written
as 0.5. We will later look at 
how this is computed concretely.
Rationals in decimal notation can be easily identified:
all numbers in decimal notation with a finite part
after the dot are rational: 
0.25 is $\frac{1}{4}$,
0.75 is $\frac{3}{4}$,
0.2  is $\frac{1}{5}$ and so on.

There are some rational numbers that are infinite.
For example, $\frac{1}{3}$ is $0.333333\dots$,
which we encode as $0.\overline{3}$.
Such periodic decimals are easy to convert
to fractions. We just have to multiply them
by a power of 10, such that there is a part
greater 0 before the decimal point and
that the first number
of the the repeating period is aligned to it.
For $0.\overline{3}$, 
this is just $10\times 0.\overline{3} = 3.\overline{3}$.
For $0.1\overline{6}$,
it would be $10\times 0.1\overline{6} = 1.\overline{6}$.
For $0.\overline{09}$,
it would be $10^2\times 0.\overline{09} = 9.\overline{09}$.
We then subtract the original number from the result.
If the original number is $x$, we now have 
$10^nx - x = (10^n-1)x$.
For $x=0.\overline{3}$ this is $9x$;
for $x=0.1\overline{6}$ this, too, is $9x$ and
for $x=0.\overline{09}$ this is $99x$.
The results are 3, 1.5 and 9 respectively.
We now build a fraction of this result as numerator
and the factor (9 or 99) in the denominator.
Hence, $0.\overline{3} = \frac{3}{9} = \frac{1}{3}$,
$0.1\overline{6} = \frac{1.5}{9} = \frac{3}{18} = \frac{1}{6}$ and
$0.\overline{09} = \frac{9}{99} = \frac{1}{11}$.

A curiosity resulting from this calculations
is that $9\times 1/3 = 3$ and 
$9 \times 0.\overline{3} = 2.\overline{9}$
are actually the same! A correct implementation
must take this into account.
Try it with Haskell, you will see that
|9*0.3333333333333333| is indeed 3.
It will not work if you forget a three.
The precision of the |Double| number type is 16
digits after the decimal point. With only 15 threes,
the result will be 2.9999999999999997.

Irrational numbers in the decimal notation
have infinite many digits after the decimal point.
With this said, it is obvious that we cannot
represent irrational numbers in this system.
We can of course represent any number by some
kind of formula like $\sqrt{5}$ and do some
math in this way such as $\frac{1+\sqrt{5}}{2}$,
\etc\ But often, when we are dealing with
applied mathematics, such formulas are not
very useful. We need an explicit number.
But, unfortunately or not, we have only
limited resources in paper, brainpower and
time. That is, at some point we have to 
abandon the calculations and work with what
can be achieved with the limited resources
we have at our disposal.

The point in time at which we take
the decision that we now have calculated
enough is the measure for the precision
of the real number type in question.
On paper, we would hence say that we
write only a limited number of digits
after the decimal point. In most day-to-day
situations where real numbers play a role,
like in dealing with money, cooking,
medication or travelling distances,
we calculate up to one or two decimal places.
Prices, for instances are often given 
as 4.99 or something, but hardly 4.998.
Recipes would tell that we need 2.5 pounds
or whatever of something, using one decimal place.
One would say that it is about 1.5km 
to somewhere, but hardly that it is 1.49km.
In other areas, especially in science 
much more precision is needed.
We therefore need a flexible datatype.

A nice and clean format to represent
real numbers uses two integers or,
as the following definition,
two natural numbers:

\begin{minipage}{\textwidth}
\begin{code}
  data RealN = R Natural Natural
\end{code}
\end{minipage}

The first number represents the integral part.
You will remember that a number is a list
of digits where every digit is multiplied
by a power of ten according to the place
of the digit. The digit, counted from the right,
is multiplied by $10^0=1$. The second is 
multiplied by $10^1$, the third by $10^2$
and so on. The digit multiplied by $10^0$,
is the last digit before the decimal point.
If we wanted to push it to the right
of the decimal point, we would need
to reduce the exponent. So, we multiplied it
not by $10^0$, but by $10^{-1}$ to push it
to the first decimal place.
This is the function of the second number
in the datatype above.
It represents the value of the 
least significant bit in terms of
the exponent to which we have to raise 10 
to obtain the number represented by this datatype.
Since our datatype uses a natural number,
we have to negate it to find the exponent we need.

For instance, the number |R 25 2|
corresponds to $25\times 10^{-2}$,
which we can reduce stepwise to
$2.5 \times 10^{-1}$ and $0.25 \times 10^0$.
A meaningful way to show this datatype would
therefore be:

\begin{minipage}{\textwidth}
\begin{code}
  instance Show RealN where
    show (R a e) = show a ++ "*10^(-" ++ show e ++ ")"
\end{code}
\end{minipage}

There are obviously many ways to represent the same
number with this number type. 
1, for instance, can be represented as

|one = R 1    0|\\
|one = R 10   1|\\
|one = R 100  2|\\
|one = R 1000 3|\\
$\dots$

To keap numbers as concise as possible, we define a function
to simplify numbers with redundant zeros:

\begin{minipage}{\textwidth}
\begin{code}
  simplify :: RealN -> RealN
  simplify (R a e) | e > 0 &&
                     a `rem` 10 == 0 = simplify (R (a `div` 10) (e-1))
                   | otherwise       = R a e
\end{code}
\end{minipage}

As long as the exponent is greater 0 and the base $a$
is divisible by 10, we reduce the exponent by one
and divide a by 10. In other words, we remove unnecessary
zeros.
The following constructor uses |simplify| to create
clean real numbers:

\begin{minipage}{\textwidth}
\begin{code}
  real :: Natural -> Natural -> RealN
  real i e = simplify (R i e)
\end{code}
\end{minipage}
