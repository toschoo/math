\ignore{
\begin{code}
module Vieta
where
  import Roots
\end{code}
}

\ignore{
- previous chapter we saw (x - phi)(x - psi) leads to
  1x + (-phi-psi)x + (-phi)(-psi)
- also: (x+2)(x-2)
  1x + (2-2)x + -2*2
- indeed, this is a kind of generalised binomial theorem:
  instead of (x+a)(x+a) -> 1 2ax aa,
  we have    (x+a)(x+b) -> 1 (a+b)x ab
- role of "a", i.e. the coefficient of the highest degree
- role of negation
- once again, everything boils down to the possible combinations
  of factors in multiplication...
- (x+a)(x+b)(x+c)
  xxx + xxc + xxb + xbc + axx + axc + ...
- elementary symmetric polynomials
  => all possible distinct combinations
- this is the powerset (without the empty set)
- each "type" (defined by size of the set) is sum'd up
- implementation
- Lagrange's formula
}

The binomial theorem describes regularities 
in the coefficients that turn up
when multiplying equal polynomials with each other.
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
the concete example $(x+1)(x-1)$. We set
$a=1$ and $b=-1$ and see:

\begin{equation}
x^2 + (1-1)b + (1\times -1) = x^ -1.
\end{equation}

That appears to be correct. But who are 
those $a$ and $b$ guys that appear in
the formula? Well, those are the additive
inverses of the roots of the polynomial in
question, since, if $(x+a)\dots$ are the linear
factors, then the polynomial becomes 0 if any
of those factors becomes 0. The factor $(x+a)$,
obviously, becomes 0 if $x=-a$. $-a$ is therefore
a root of the polynomial. It follows that we
have direct relation between the roots and
the coefficients.

As a first approximiation (which is wrong!),
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

Note, by the way, the multiplication $12\times 13 = 156$,
which, once again, shows the similarity of numbers and
polynomials.

Now, what about the polynomial
$-x^2 - x + 1$.
We know that it has the same roots as
the polynomial
$x^2 + x - 1$.
But how can we get the coefficients from the roots
with the same formula? Something seems to be wrong\dots

Well, until now, we have looked only at \term{monic}
polynomials, that is polynomials with the first coefficient
being 1. But the polynomial $-x^2 - x + 1$ is not monic.
The first coefficient is -1. In fact, the complete
factorisation of this polynimoal is

\[
-1(x+\Phi)(x-\Psi).
\]

We have to adjust our formula above to this case,
\ie:

\[
ax^2 + \frac{(-\alpha-\beta)}{a}x + \frac{\alpha\beta}{a}.
\]








\begin{minipage}{\textwidth}
\begin{code}
  vieta :: (Real a) => [a] -> [a]
  vieta = c . g . d . s . Perm.ps
    where  d    =  drop 1
           g    =  groupBy (\x y -> length x == length y)
           s    =  sortBy (\x y -> length x `compare` length y)
           c p  =  [(-1)^n * sum (map product x) | (x,n) <- zip p [0..]] 
\end{code}
\end{minipage}
