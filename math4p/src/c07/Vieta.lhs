\ignore{
\begin{code}
module Vieta
where
  import Roots
\end{code}
}

The binomial theorem describes regularities 
in the coefficients that turn up
when multiplying a polynomial (repeatedly) by it itself.
For the simple case $(a+b)(a+b)$, we get the result
$(a^2 + 2ab + b^2)$. The linear factors of polynomials
have a similar structure: sums of two numbers that are
multiplied with each other, \eg:

\begin{equation}
x^2 - 1 = (x+1)(x-1).
\end{equation}

Should we not expect similar regularities
with the coefficients of the resulting polynomials
in those cases? When we look at this in an algebraic
way, we would see:

\begin{equation}
(x+a)(x+b) = x^2+xb+xa+ab = x^2 + (a+b)x + ab.
\end{equation}

The coefficients of the resulting polynomial are
1, $a+b$ and $ab$. We immediately see the relation
to the binomial theorem:
if $a=b$, we would have $2a$ and $a^2$, where,
in the binomial theorem, the final coefficent is
interpreted as 1, for the number of occurrences of
$a^2$. We, hence, get $1, 2, 1$.

Let us check the theoretic result against
the concrete example $(x+1)(x-1)$. We set
$a=1$ and $b=-1$ and see:

\begin{equation}
x^2 + (1-1)x + (1\times (-1)) = x^2 - 1.
\end{equation}

That appears to be correct. But who are 
those $a$ and $b$ guys that appear in
the formula? Well, those are the additive
inverses of the roots of the polynomial in
question, since, if $(x+a)(x+b)\dots$ are the linear
factors, then the polynomial becomes 0 if any
of those factors becomes 0. The factor $(x+a)$,
obviously, becomes 0 if $x=-a$. $-a$ is therefore
a root of the polynomial. It follows that we
have a direct relation between the roots and
the coefficients.

As a first approximation (which is wrong!),
we could describe a second degree polynomial 
with the roots
$\alpha$ and $\beta$ as:

\[
x^2 + (-\alpha-\beta)x + \alpha\beta,
\]

We check again with $\alpha = -1$ and $\beta=1$:

\begin{equation}
x^2 + (1-1)x + (-1\times 1) = x^2 - 1.
\end{equation}

Correct until here.
What about other examples, for instance: 
$x^2 + x - 1$.
We already know the roots are $-\Phi$ and $-\Psi$.
So, we set $\alpha=-\Phi$ and $\beta=-\Psi$:

\begin{equation}
x^2 + (\Phi+\Psi)x + ((-\Phi)\times(-\Psi)) = x^2 + x - 1.
\end{equation}

The polynomial $x^2 - 4$ has the roots 2 and -2:

\begin{equation}
x^2 + (-2+2)x + (2\times(-2)) = x^2 - 4.
\end{equation}

The polynomial $x^2 + 5 + 6$ has the roots
-2 and -3:

\begin{equation}
x^2 + (2+3)x + (-2\times(-3)) = x^2 + 5x + 6.
\end{equation}

Note, by the way, the multiplication $12\times 13 = 156$.
Once again, this is a nice illustration of the similarity 
of numbers and polynomials.

Now, what about the polynomial
$-x^2 - x + 1$.
We know it has the same roots as
the polynomial
$x^2 + x - 1$.
But how can we get the coefficients from the roots
with the same formula? Something seems to be wrong\dots

Well, until now, we have looked only at \term{monic}
polynomials, that is polynomials with the first coefficient
being 1. But the polynomial $-x^2 - x + 1$ is not monic.
The first coefficient is -1. In fact, the complete
factorisation of this polynomial is

\[
-1(x+\Phi)(x+\Psi).
\]

We have to adjust our formula above to this case,
\ie:

\[
ax^2 + \frac{-\alpha-\beta}{a}x + \frac{\alpha\beta}{a},
\]

which can be simplified to

\[
ax^2 - \frac{\alpha+\beta}{a}x + \frac{\alpha\beta}{a}.
\]

When we now test with roots $\alpha=-\Phi$ and $\beta=-\Psi$
and coefficient $a=-1$,
we get

\[
-x^2 - \frac{-\Phi-\Psi}{-1}x + \frac{(-\Phi)(-\Psi)}{-1} = 
-x^2 - \frac{-1}{-1}x + \frac{-1}{-1} =
-x^2 - x + 1
\]

and everything appears to be in joint again.

When we advance beyond degree 2,
how should these formulas evolve?
Let us look at roots in terms of linear factors.
For a polynomial of degree $n$, we have up to $n$ factors
of the form

\[
(x+\alpha)(x+\beta)(x+\gamma)\dots
\]

When we multiply that out, we get combinations
as products and sums of products
of the coefficients of the linear factors
$\alpha, \beta, \gamma, \dots$
(which are the inverses of the roots 
of the resulting polynomial):

\[
(x^2 + \beta x + \alpha x + \alpha\beta)(x+\gamma),
\]

which is

\[
x^3 + 
(\alpha + \beta + \gamma) x^2 + 
(\alpha\beta + \alpha\gamma + \beta\gamma)x +
\alpha\beta\gamma.
\]

This already begins to reveal a pattern.
The first coefficient of the resulting polynomial
(counting without the coefficient of $x^3$)
is the sum of all the linear coefficients;
the second coefficient is the sum of all their
tuple products; the third is a triple product.
We could suspect that the third, in a 
four-degree polynomial, would be the sum
of all triple products and the fourth a
single quadruple product. Let us check:
we compute

\[
(x^3 + 
(\alpha + \beta + \gamma) x^2 + 
(\alpha\beta + \alpha\gamma + \beta\gamma)x +
\alpha\beta\gamma)(x + \delta)
\]

and get

\[
\begin{array}{cll}
    &  & x^4   \\
  + & (\alpha + \beta + \gamma + \delta) & x^3   \\
  + & (\alpha\beta + \alpha\gamma + \alpha\delta +
     \beta\gamma + \beta\delta + \gamma\delta) & x^2   \\
  + & (\alpha\beta\gamma + \alpha\beta\delta + \alpha\gamma\delta +
     \beta\gamma\delta) & x   \\
  + & \alpha\beta\gamma\delta. &
\end{array}
\]

The result, indeed, continues the pattern we saw above.
For the first coefficient we see the simple sum 
of all the linear coefficients;
for the second one, we see the sum of all tuple products;
for the third one, we see the sum of all triple products
and then we see a single quadruple product.

When we now bring the negative sign of the roots in
(we used their additive inverses) and
the first coefficient, the we get the following
sequence of formulas:

\begin{subequations}\label{eq:vieta1}
\begin{align}
x_1 + x_2 + \dots + x_n & = & -\frac{a_{n-1}}{a_n}\\
(x_1x_2 + \dots + x_1x_n) + 
(x_2x_3 + \dots + x_2x_n) + \dots + 
x_{n-1}x_n & = & \frac{a_{n-2}}{a_n}\\
\dots & = & \dots\\
(x_1x_2 \dots x_n) & = & (-1)^n\frac{a_0}{a_n}
\end{align}
\end{subequations}

to describe the relation of roots and coefficients
of a polynomial of the form

\[
a_nx^n + a_{n-1}x^{n-1} + \dots + a_0
\]

with roots $x_1, x_2, \dots, x_n$.

The equations \ref{eq:vieta1} are known as
\term{Vieta's formulas}, after the French lawyer
and mathematician François Viète (1540 -- 1603)
whom we already know as author of an elegant
formula to express $\pi$.

But what are those constructs on the left-hand
side of the formulas? One answer is:
those are \term{elementary symmetric polynomials},
which are building blocks for \term{symmetric polynomials}.
Symmetric polynomials will be very important for us
further down the road. At the moment, they only delay
a good answer to the question\dots

A better answer at this stage is
that those beasts are the sums of
all \emph{distinct} combinations
of the roots in 1-tuples, 2-tuples, 3-tuples
and so on.
For the first case, the `1-tuples',
that is just the sum of all the roots;
for the second case, the `2-tuples',
we have all combinations of 
\emph{2 elements out of $n$}, where $n$
is the number of roots;
for the third case, we have all combinations
of \emph{3 elements out of $n$} and so on.

You probably guess where this is leading us.
When we have four roots,
the first coefficient,
the one in front of $x^{n-1}$,
is basically the sum
of $\binom{4}{1} = 4$ terms;
the second coefficient is the sum of
$\binom{4}{2} = 6$ terms;
the third coefficient is the sum of
$\binom{4}{3} = 4$ terms and
the last coefficient,
the one without an $x$, is the sum of
only $\binom{4}{4} = 1$ term.

In general, for $n$ roots, we get,
for the $k^{th}$ coefficient,
$\binom{n}{k}$ terms
of products of $k$ roots.
Those are $\sum_{k=0}^n{\binom{n}{k}} = 2^n$
terms in total 
(including the coefficient in front of $x^n$,
which corresponds to $\binom{n}{0}=1$).
Once again, algebra boils down to combinatorial
problems induced by the distributive law.

Let us devise a function 
that gives us the right-hand sides of Vieta's formula,
when we provide the left-hand sides.
That is, we write a function that receives the list
of roots of the polynomial and that returns the list
of the coefficients divided by the first coefficient.

On the first sight, it seems to be tricky to get
the sums of products right. But, in fact, we already
know everything we need. 
What we want to do is to generate
all possible $k$-combinations for $k=1\dots n$
of the $n$ elements, but without duplicates, \ie\
$ab$ is the same as $ba$ (since multiplication
is commutative).
This, however, is the structure of the powerset,
which, for a set with $n$ elements, contains indeed
$2^n$ subsets -- just the number of all possibilities
to choose $k$ out of $n$ for $k=0\dots n$.

For instance, the set of roots 
$\lbrace\alpha,\beta,\gamma,\delta\rbrace$
has the powerset (ordered according
to the size of the subsets):

\begin{minipage}{\textwidth}
\begin{gather*}
\lbrace
\varnothing,\\
\lbrace\alpha\rbrace,
\lbrace\beta \rbrace,
\lbrace\gamma\rbrace,
\lbrace\delta\rbrace,\\
\lbrace\alpha,\beta\rbrace,
\lbrace\alpha,\gamma\rbrace,
\lbrace\alpha,\delta\rbrace,
\lbrace\beta,\gamma\rbrace,
\lbrace\beta,\delta\rbrace,
\lbrace\delta,\gamma\rbrace,\\
\lbrace\alpha,\beta,\gamma\rbrace,
\lbrace\alpha,\beta,\delta\rbrace,
\lbrace\alpha,\gamma,\delta\rbrace,
\lbrace\beta,\gamma,\delta\rbrace,\\
\lbrace\alpha,\beta,\gamma,\delta\rbrace
\rbrace
\end{gather*}
\end{minipage}

We can transform the powerset into 
the coefficients by
dropping $\varnothing$ (which 
represents $a$ in a monic polynomial)
and then adding up the products of
the subsets of the same size. 
The following function
does that:

\begin{minipage}{\textwidth}
\begin{code}
  vieta :: (Real a) => [a] -> [a]
  vieta = c . g . d . s . Perm.ps
    where  d    =  drop 1
           g    =  groupBy (\x y -> length x == length y)
           s    =  sortBy (\x y -> length x `compare` length y)
           c p  =  [(-1)^n * sum (map product x) | (x,n) <- zip p [1..]] 
\end{code}
\end{minipage}

We first create the powerset (|Perm.ps|).
We then sort it by the lengths of the subsets (that is
the \term{cardinalities} in set theory jargon) and
drop the first one (the empty set).
We then introduce one more level of separation,
\ie\ we group the subsets by their size.
From this result, we create a new set
by zipping the result with the natural numbers
starting from 1 so that each
group of equal length gets paired with a
number $n$.
We, then, map |product| on these lists and 
add the resulting products together.
Finally, we multiply this number by -1
raised to the power of $n$. 

This last step
takes care of signedness.
Since, in the linear factors, we use the
additive inverses of the roots,
the effect of the signs of the roots
must be flipped around.
Therefore, we turn
the sign of every second result, namely
those with an odd number of factors.
The negative signs of the roots
that enter products with an even number of factors
cancel out by themselves.

Let us look at some examples.
We start with our favourite: $x^2 + x - 1$.
We call |vieta [-phi, -psi]| and get

|[1.0,-1.0]|.

That are the coefficient of $x$ and the constant -1.
To complicate, we check some variants of
those roots:

\begin{itemize}
\item |vieta [phi, -psi]| gives $-\sqrt{5}, 1$ and, hence, the polynomial
$x^2 - \sqrt{5} + 1$, whose roots are indeed $\Phi$ and $-\Psi$.

\item |vieta [-phi, psi]| gives $\sqrt{5}, 1$ and that is the polynomial
$x^2 + \sqrt{5} + 1$, whose roots are $-\Phi$ and $\Psi$.

\item |vieta [phi, psi]| gives $-1, -1$, the polynomial
$x^2 - x - 1$, whose roots are $\Phi$ and $\Psi$.
\end{itemize}

A simpler example that shows the signedness of roots and coefficients
is $x^2 - 1$. |vieta [1,-1]| gives |[0,-1]|, which, indeed,
corresponds to $x^2  - 1$.

What about a third-degree polynomial,
\eg\ $(x+1)(x+1)(x+1) = x^3 + 3x^2 + 3x + 1$?
We call |vieta [-1,-1,-1]| and see |[3,3,1]|.

Another experiment: we compute
|mul (P [1,1]) (mul (P [2,1]) (P [3,1]))|,
which is $(x+1)(x+2)(x+3)$ and get
|P [6,11,6,1]|, which represents the polynomial
$x^3 + 6x^2 + 11x + 6$.
We call |vieta [-1,-2,-3]| and get |[6,11,6]|.

A fifth-degree polynomial:
|prodp mul [P [1,1], P [2,1], P [3,1], P [4,1], P [5,1]]|:
|P [120,274,225,85,15,1]|, that is

\[
x^5 + 15x^4 + 85x^3 + 225x^2 + 274x + 120.
\]

|vieta [-1,-2,-3,-4,-5]|: |[15,85,225,274,120]|.

Well, we can go on playing around like this forever.
The point of Vieta's formulas, however, is not so
much practical. It is not an efficient way to compute
roots from coefficients or coefficients from roots.
That should be clear immediately, when we look at
the Haskell function |vieta|. It generates the
powerset of the set of roots -- and that cannot be
efficient at least for large (or better worded perhaps:
unsmall) numbers.
Vieta's formulas, instead, are a theoretical device.
They help to understand the relation between
coefficients and roots and they will play an important
role in our further investigations.

