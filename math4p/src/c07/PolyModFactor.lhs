\ignore{
\begin{code}
module PolyModFactor
where
  import PolyFactor
\end{code}
}

Famous factorisation algorithms using modular
arithmetic are \term{Berlekamp's algorithm}
developed by the American mathematician 
Elwyn Berlekamp in the late Sixties
and the \term{Cantor-Zassenhaus algorithm}
developed in the late Seventies and early Eighties
by David Cantor, an American methematician,
not to be confused with Georg Cantor, and
Hans Zassenhaus (1912 -- 1991), a German-American
mathematician.
We will here focus on Cantor-Zassenhaus,
which is by today probably the most-used algorithm
implemented in many computer algebra systems.

The contribution of Cantor-Zassenhaus, strictly speaking,
is just one of several pieces. The whole approach is
based on Euler's theorem,
which, as you may remember, states that

\begin{equation}
a^{\varphi(n)} \equiv 1 \pmod{n},
\end{equation}

where $\varphi(n)$ is the totient function
of $n$ counting the numbers $1\dots n-1$ that
are coprime to $n$, \ie that share no divisors with $n$.

Euler's theorem is defined as theorem over the ring
of integers, which, by modular arithmetic, transforms
into the finite field of the integers $0\dots n-1$.
Polynomial rings can be seen as extensions
of the underlying ring (of integers).
When we introduce modular arithmetic,
that is, when we build polynomials on 
a finite field, they still
constitute a ring, but now a ring built on top of a finite field.
Notationally, this is usually expressed as $K[x]$,
where $K$ is a field and $K[x]$ the polynomial ring
defined on top of $K$.

When we now take polynomials modulo a polynomial,
we again get a finite field, this time a polynomial field
of the form $K[x]/m$ (pronounced ``over'' $m$),
where $m$ is a polynomial.
The point in doing this is that many properties of
the original field $K$ are preserved in $K[x]/m$ and
Euler's theorem is an example thereof.

However, we need to redefine Euler's theorem
to make clear what is meant by it in the new context.
First, we are dealing with the polynomial field $K[x]$
and a polynomial $m \in K[x]$.
Then we define the totient function as

\[
\varphi(m) = ||\lbrace f \in K[x] : 0 \le f \le m \wedge \gcd(m,f) = 1\rbrace||,
\]

\ie\ the cardinality of the set of all polynomials $f$
less or equal than $m$ that do not share
divisors with $m$. For any such field 
and any $f \in K[x] : \gcd(m,f) = 1$,
the following holds:

\begin{equation}
f^{\varphi(m)} \equiv 1 \pmod{m}.
\end{equation}

This is easy to prove.
The resulting structure $K[x]/(m)$
has a multiplicative
group $K_m^*$ (just as the integers $\pmod{n}$).
The members of this group are all polynomials
that do not share divisors with $m$ and $\varphi(m)$
is the cardinality of this group.
From $gcd(m,f) = 1$, it follows that
$f_m = f \mod{m} \in K_m^*$.
We, for sure, have 
$f_m^{\varphi(m)} \equiv 1 \pmod{m}$, since 
$\varphi(m)$ is the size of the group.
This equivalence may hold also for other numbers, $a$,
such that $f^a \equiv 1 \pmod{m}$, but
according to Lagrange's theorem 
(that the cardinality of subgroups of $G$ divides
the cardinality of $G$) all these numbers $a$
must divide $\varphi(m)$, the size of the group,
and we unmistakenly have
$f^{\varphi(m)} \equiv 1 \pmod{m}\qed$.

From this theorem, Fermat's little theorem
follows. Let $K$ be a field with $q$ elements; when using
arithmetic modulo a prime $p$, then $K_m^*$ is the group
of numbers $1\dots p-1$, which has $q=p-1$ elements.
Note that, when we refer to the multiplicative group
of this field, we usually refer only to the numbers
$1\dots p-1$, which contains only $p-1$ elements.
Now, let $g$ be an \term{irreducible} polynomial,
\ie\ a non-constant polynomial that cannot be 
further factored and, hence, a ``prime'' in our polynomial ring,
with degree $d$, $d>0$. Then it holds for any polynomial $f$
from this field

\begin{equation}\label{eq:polyFacFermat}
f^{q^d} \equiv f \pmod{g}.
\end{equation}

We can prove this easily:
We know that $K$ has $q$ elements.
From this $q$ elements we can create 
a limited number of polynomials.
When you look at our Haskell representation of polynomials,
you will easily convince yourself that the number of valid
polynomials of a given degree $d$ equals the number of valid
numbers that can be presented in the numeral system base $q$
with $d+1$ digits. If, for instance, $q=2$, then we have
(without the zero-polynomial |P [0]|)

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||r||r||c||}\hline
degree & size & polynomials\\\hline\hline
0      & 1    & |P [1]|\\\hline
1      & 2    & |P [0,1], P [1,1]|\\\hline
2      & 4    & |P [0,0,1], P [1,0,1], P [0,1,1], P [1,1,1]|\\\hline
3      & 8    & |P [0,0,1,1], P [1,0,1,1], P [0,1,1,1], P [1,1,1,1]|\\
       &      & |P [0,0,0,1], P [1,0,0,1], P [0,1,0,1], P [1,1,0,1]|\\\hline
$\dots$&$\dots$&$\dots$\\\hline
\end{tabular}
\endgroup
\end{center}

We, hence, can precisely say how many polynomials 
of degree $<d$ there are, namely $r=q^d$.
For the example $q=2$, we see that there are 16 polynomials
with degree less than 4, which is $2^4$.
One of those polynomials, however, is |P [0]|,
which we must exclude, when asking for $\varphi(g)$,
since this polynomial is zero 
for which division is not defined.
For the irreducible polynomial $g$, we therefore
have $r-1$ elements that do not share divisors with $g$,
\ie\ $\varphi(g) = r-1$. So, according to Euler's theorem,
we have 

\begin{equation}
f^{r-1} \equiv 1 \pmod{g}. 
\end{equation}

Multiplying both sides by $f$, we get

\begin{equation}
f^{r} \equiv f \pmod{g}. 
\end{equation}

Since $r=q^d$, this is equivalent to \ref{eq:polyFacFermat}
and this concludes the proof.$\qed$

From Fermat's theorem, we can derive a nice and useful corollary.
Note that when we subtract $f$ from both sides of the equivalence,
then we would get 0 on the right-hand side, which means that
$g$ divides the expression on the left-hand side.
Set $x=f$, then we have:

\begin{equation}\label{eq:polyFacIrrTest}
x^{q^d} - x \equiv 0 \pmod{g}. 
\end{equation}

This is the basis for 
a nice test for irreducibility.
Since the group established by a non-irreducible
polynomial of degree $d$ has less than $p^d - 1$ elements,
it will divide $x^{p^c} - x$ for some $c<d$, but an irreducible
polynomial will not.
\ignore{TODO: why are there no subgroups?}
Here is a Haskell implementation:

\begin{minipage}{\textwidth}
\begin{code}
  irreducible :: Natural -> Poly Natural -> Bool
  irreducible p u  |  d < 2      = False
                   |  otherwise  = go 1 x
    where  d       =  degree u
           x       =  P [0,1]
           go i z  =  let z' = powmp p p z
                      in  case pmmod p (addp p z' (P [0,p-1])) u of
                          P [_]  ->  i == d
                          g      ->  if i < d  then go (i+1) (pmmod p z' u)
                                               else False
\end{code}
\end{minipage}

The function receives two arguments: the modulus and 
the polynomial we want to check.
First, we compute the degree of the polynomial.
When the polynomial is of degree 0 or 1, 
there are by definition only trivial, \ie\ constant factors.
It is, hence, not irreducible (it is not reducible either, 
it is just uninteresting).
Then we start the algorithm beginning with values 1 and
$x$, where $x$ is the simple polynomial $x$.
In |go|, we raise this polynomial to the power of $p$,
and subtract it from the result.
Note that we add $p-1$, which,
in modular arithmetic, is the same as subtracting 1. 
We take the result modulo the input polynomial |u|. 
This corresponds to
$x^{p^d} - x$ for degree $d=1$.

If the result is a constant polynomial 
and the degree counter $i$ equals $d$,
then equation \ref{eq:polyFacIrrTest} is fulfilled.
(Note that we consider any constant polynomial as zero,
since a constant polynomial is just the content,
which usually should have been removed before we start
to search factors.)
Otherwise, if the degree counter does not equal $d$,
this polynomial fulfils the equation with a ``wrong'' degree.
This is possible only if the input was not irreducible
in the first place.

Finally, if we have a remainder that is not constant,
we either continue (if we have not yet reached the degree
in question) or, if we had already reached the final degree,
we return with False, since the polynomial 
is certainly not irreducible.

Note that we continue with |pmmod p z' u|, that is,
with the previous power modulo $u$. This is an important
optimisation measure. If we did not do that,
we would create gigantic polynomials. Imagine
a polynomial of degree 8 modulo 11. To check that polynomial
we would need to raise $x$ to the power of $11^8$,
which would result in a polynomial of degree \num{214358881}.
Since the only thing we want to know is
a value modulo $u$, we can reduce the overhead of taking powers
by taking them modulo $u$ in the first place.

Let us look at an example.
We generate a random polynomial of degree 3 modulo 7:

|g <- randomPoly 7 4|\\

I get the polynomial |P [3,3,3,4]|.
(Note that you may get another one!)
Calling |irreducible 7 g| says: |False|.

When we raise the polynomial |P [0,1]| 
to the power of $7^3 = 343$,
we get a polynomial of degree 343
with the leading coefficient 1.
When we subtract |P [0,1]| from it,
it will have -1, which is 6 in this case,
as last but one coefficient.
Taking this modulo to the random polynomial $g$,
we get the polynomial |P [0,3,6]|, which is 
$6x^2 + 3x$ and certainly not constant.
$g$ is therefore not irreducible.

Let us try another one:

|g <- randomPoly 7 4|\\

This time, I get |P [3,1,4,4]|.
Calling |irreducible 7 g| says: |True|.
When we take $x^{7^3} - x$ modulo $g$,
we get |P [0]|. |P [3,1,4,4]|, hence,
is irreducible.

The formula, however, is not only interesting
for testing irreducibility.
What the formula states is in fact that
all irreducible polynomials of degree $d$
are factors of $x^{q^d} - x$.
Whenever we construct this expression,
we have created the product of all 
irreducible polynomials of degree $d$.
The irreducible factors of our polynomial
are part of this product and we can get them out
just by asking for the greatest common divisor.
This would give us the product of all factors
of our polynomial of a given degree.

We need to add one more qualification however.
Since we are searching for a \term{unique}
factorisation, we should make sure that
we always make the polynomial \term{monic},
that is, we should remove the leading coefficient
by dividing all coefficients by it.
This corresponds to content-and-primitive-part factorisation
as already discussed above, but in the case of modular
arithmetic it is much simpler. Whatever the leading coefficients
is, we can just multiply all coefficients by its inverse
without worrying about coefficients becoming fractions.
Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  monicp :: Integer -> Poly Integer -> Poly Integer
  monicp p u =  let  cs  = coeffs u
                     k   = last cs `M.inverse` p
                in P (map (modmul p k) cs)
\end{code}
\end{minipage}

The following function, obtains the products of the factors
of a given (monic) polynomial degree by degree. Note
that we give the result back as a monic
polynomial again. Each result is a tuple of the degree and
the corresponding factor product.

\begin{minipage}{\textwidth}
\begin{code}
  ddfac :: Natural -> Poly Natural -> [(Int, Poly Natural)]
  ddfac p u   = go 1 u (P [0,1])
    where  n  = degree u
           go d v x  |  degree v <= 0  = []
                     |  otherwise      = 
                        let  x'        = powmp p p x 
                             t         = addp p x' (P [0,p-1])
                             g         = gcdmp p t v
                             (v',_)    = divmp p v g
                             r         = (d,monicp p g)
                        in  case g of
                            P [_]  ->      go (d+1) v' (pmmod p x' u) 
                            _      -> r :  go (d+1) v' (pmmod p x' u) 
\end{code}
\end{minipage}

The real work is done by function |go|.
It starts with degree $d=1$, the polynomial $u$
we want to factor and, again, the simple polynomial $x$.
We then raise $x$ to the power $p^1$ for the first degree,
subtract $x$ from the result and compute the |gcd|.
If the result is a constant polynomial,
there are no non-trivial factors of this degree and we continue.
Otherwise, we store the result with the degree,
making $g$ monic. 

We continue with the next degree, $d+1$,
the quotient of the polynomial we started with and
the power of |x'| reduced to the modulo $u$.
The latter is again an optimisation.
The former, however, is essential to avoid
generating the same factor product over and over again.
By dividing the input polynomial by $g$, we make sure
that the factors we have already found are taken out.
This works only if the polynomial is squarefree of course.
(You might remember the discussion of squarefree
numbers in the context of Euler's theorem where we found
that, if $n$ is squarefree, then 
$\varphi(n) = \prod_{p||n}{p-1}$, \ie\ the totient number
of $n$ is the product of the primes in the factorisation
of $n$ all reduced by 1.)
We need to come back to this topic and, for the moment,
make sure that we only apply polynomials that are
squarefree and monic.

We try |ddfac| on the 4-degree polynomial 
$u(x) = x^4 + x^3 + 3x^2 + 4 x + 5$ modulo 7 and call
|ddfac 7 u| and obtain the result

|[(1,P [2,4,1]),(2,P [6,4,1])]|,

\ie\ the factor product $x^2 + 4x + 2$ for degree 1
and the factor product $x^2 + 4x + 6$ for degree 2.
First, we make sure that these are really factors
of $u$ by calling |divmp 7 (P [2,4,1])|, which shows

|(P [6,4,1],P [0])|.

We can conclude that these are indeed all the factors
of $u$. But, obviously, |P [2,4,1]| or $x^2 + 4x + 2$ is
not irreducible, since it is a second-degree polynomial,
but it was obtained for the irreducible factors of degree 1.
|P [6,4,1]|, on the other hand, was obtained for degree 2
and is itself of degree 2. We can therefore assume that
it is already irreducible, but let us check: 
|irreducible 7 (P [6,4,1])|, indeed, yields |True|.

But what about the other one? How can we get the irreducible
factors out of that one? Here Cantor and Zassenhaus come in.
They proposed a simple algorithm with the following logic.
We, again, use the magic polynomial $x^{p^d} - x$, but choose 
a specific polynomial for $x$, say $t$. We already have
that chunk of irreducible polynomials hidden in |(P [2,4,1])|,
let us call it $u$,
and know that those polynomials are factors of both,
$t^{p^d} - t$ and and $u$. 
The approach of Cantor and Zassenhaus is to split the factors
so that the problem reduces significantly. We can split $t$
into three parts using the equality

\begin{equation}
t^{p^d} - t = t(t^{(p^d-1)/2} + 1)(t^{(p^d-1)/2} - 1).
\end{equation}

Make sure that the equality holds 
by multiplying the right-hand side out.
By a careful choice of $t$, we can make sure that the factors
are likely to be more or less equally distributed among the
latter two factors. That, indeed, would reduce the problem significantly.

Since $u$ and $t^{p^d} - t$ share factors, we can transform
the equality into the following variant:

\begin{equation}\label{eq:polyFac_CZ1}
u = \gcd(u,t)\times\gcd(u,(t^{(p^d-1)/2} + 1))\times\gcd(u,((t^{(p^d-1)/2} - 1))
\end{equation}

A reasonable choice for $t$ is a polynomial
of degree $2d-1$, since in this case there is high probability
that the factors are spread equally among the three factors
of equality \ref{eq:polyFac_CZ1}. 
With high probability, we reduce the problem significantly,
when we compute one of the $\gcd$s and continue splitting
this $\gcd$ and the quotient of $u$ and the $\gcd$ further.
Should we be unlucky (the $\gcd$ contains either no or all
of the factors), we just try again with another polynomial.
After some tries (less than three according to common wisdom), 
we will hit a common factor.

There is an issue, however, for $p=2$.
Because in that case, 
$t^{(p^d-1)/2} - 1 = t^{(p^d-1)/2} + 1$.
Consider, to illustrate that, a polynomial modulo 2, for instance
|P [0,1,1]| and $d=3$. Then we have 

\[
(p^d-1)/2 = (2^3-1)/2 = 7/2 = 3.
\]

We raise the polynomial to the power of 3 and get |[0,0,0,1,1,1,1]|.
When we add |P [1]|, we get |[1,0,0,1,1,1,1]|. 
But what do we subtract? Let us try |modp 2 (P [-1])|.
We get back |P [1]|. 
Adding and subtracting 1 is just the same thing here.

But that would mean that our formula would be much poorer.
We would not have three different factors, but only two, namely
$t$ and $t^{(p^d-1)/2} + 1$. Unfortunately, it is very likely
that all the factors end up in the second one and with this,
we would not simplify the problem.

The fact that we are now working modulo 2
may help. We first observe that, modulo 2, there is
no difference between the polynomials 
$t^{2^d} - t$ (the magic one with $p=2$) and 
$t^{2^d} + t$. 
The second one, however, is easy to split,
when we set 

\[
w = t + t^2 + t^4 + \dots + t^{2^{d-1}}.
\]

Then, $w^2$ would be 

\[
t^2 + t^4 + \dots + t^{2d}.
\]

This may shock you on the first sight.
But remember, we are still working modulo p 
and we have (\term{freshman's dream}):

\[
(a+b)^p \equiv a^p + b^p \pmod{p}.
\]

When multiplying $w$ by itself, we would get

\[
t^2 + 2t^3 + t^4 + 2t^5 + 2t^6 + t^8.
\]

Since we are working modulo 2,
all terms with even coefficients cancel out,
we, hence, get

\[
t^2 + t^4 + t^8.
\]

Now, observe that 

\[
w^2 + w = t^2 + t^4 + \dots + t^{2^d} + t + t^2 + \dots + t^{2^{d-1}},
\]

when we rearrange according to exponents, we again get pairs of equal terms:

\[
w^2 + w = t + 2t^2 + 2t^4 + \dots +  2t^{2^{d-1}} + t^{2^d}.
\]

When we compute this modulo 2, again all terms with even coefficients
fall away and we finally get

\begin{equation}
w^2 + w = t^{2^d} + t.
\end{equation}

The point of all this is that we can split the expression $w^2 + w$
into two more or less equal parts, just by factoring $w$ out:
$w(w+1)$. Now, it is again very probable that we find common divisors
in both of the factors, $w$ or $w+1$ making it likely that we can
reduce the problem by taking the $\gcd$ with one of them.
Here is the implementation of the Cantor-Zassenhaus algorithm:

\begin{minipage}{\textwidth}
\begin{code}
  cz :: Natural -> Int -> Poly Natural -> IO [Poly Natural]
  cz p d u  | n <= d     = return [monicp p u]
            | otherwise  = do 
    x <- monicp p <$> randomPoly p (2*d)
    let t  | p == 2     = addsquares (d-1) p x u
           | otherwise  = addp p (powmodp p m x u) (P [p-1])
    let r = gcdmp p u t
    if degree r == 0 || degree r == n then cz p d u
      else do  r1 <- cz p d r 
               r2 <- cz p d (fst $ divmp p u r) 
               return (r1 ++ r2)
    where  n = degree u
           m = (p^d-1) `div` 2
\end{code}
\end{minipage}

The function receives a natural number,
that is the modulus $p$, an |Int|, $d$, for the degree
and the polynomial $u$, the factor product,
which we both obtained from |ddfac|.
When the degree is equal or greater than $n$,
the degree of $u$, we are done: we already have
a factor of the predicted degree.
Otherwise, we generate a random monic polynomial
of degree $2d-1$. Note that, since |randomPoly|
expects the number of coefficients, we just pass $2d$.

Then we calculate $t$. If $p$ is 2, we use |addsquares|,
at which we will look in a moment. Otherwise,
we raise the random polynomial to the power of $(p^d-1)/2$
and subtract 1. That is the third factor of equation
\ref{eq:polyFac_CZ1}. We compute the $\gcd$ and,
if the result has either degree 0 (no factor was found)
or degree equal to $u$ (all factors are in this one),
we just try again with another random polynomial.
Otherwise, we continue with the $\gcd$ and the quotient
$u/\gcd$.

Let us try this for the result |(1,P [2,4,1])| we obtained
earlier from applying |ddfac| on |P [5,4,3,1,1]|.
|cz 7 1 (P [2,4,1])| gives 

|[P [6,1],P [5,1]]|

two irreducible polynomials of degree 1.
The complete factorisation of |P [5,4,3,1,1]|, hence, is

|[P [6,1],P [5,1],P [6,4,1]]|,

which we can test by calling 
|prodp (mulmp 7) [P [6,1],P [5,1],P [6,4,1]]|
and we, indeed, get |P [5,4,3,1,1]| back.

For the case where $p=2$, we use the function addsquares:

\begin{minipage}{\textwidth}
\begin{code}
  addsquares :: Int -> Natural -> Poly Natural -> Poly Natural -> Poly Natural
  addsquares i p x u = go i x x
    where  go 0 w _  = w
           go k w t  =  let  t'  =  pmmod p (powmp p p t) u
                             w'  =  addp p w t'
                        in go (k-1) w' t'
\end{code}
\end{minipage}

which just computes $w$ as $t + t^2 + t^4 + \dots t^{2^{d-1}}$.

Let us try |ddfac| and |cz| with a polynomial modulo 2,
\eg\ |P [0,1,1,1,0,0,1,1,1]|, which is of degree 8 and
is squarefree and irreducible (and, per definition, monic).
The call |ddfac 2 (P [0,1,1,1,0,0,1,1,1])| gives
us three chunks of factors:

|[(1,P [0,1,1]),(2,P [1,1,1]),(4,P [1,1,1,1,1])]|.

We see at once that the second and third polynomials
are already irreducible, since they already have 
the specified degree. The first one, however, is
of degree 2, but shall contain factors of degree 1.
So, let us see what |cz 2 1 (P [0,1,1])| will yield:

|[P [0,1],P [1,1]]|.

The complete factorisation of |P [0,1,1,1,0,0,1,1,1]|,
hence, is

|[P [0,1],P [1,1],P [1,1,1],P [1,1,1,1,1]]|.

Now, we still have to solve the problem of polynomials
containing squared factors, \ie\ repeated roots.
There is in fact a method to find such factors
adopted from calculus and, again, related to the
derivative. The point is that a polynomial and
its derivative share only those divisors that
appear more than once in the factorisation.
We have not enough knowledge on derivatives to
prove that here rigorously, but we can give an
intuition.

Consider a polynomial with the factorisation

\[
(x+a)(x+b)\dots
\]

This is a product and, to find the derivative
of this polynomial, we need to apply the \term{product rule}
(which we will study in part 3). The product rule states that

\begin{equation}
(f \times g)' = fg' + f'g,
\end{equation}

\ie\ the derivative of the product of $f$ and $g$
is the sum of the product of $f$ and the derivative of $g$
and the product of the derivative of $f$ and $g$.

The derivatives of the individual factors $(x+a)(x+b)$
all reduce to 1, since for $f = x^1 + a$,
$f' = 1\times x^0 = 1$. The product of factors, hence,
turns into a sum of factors:

\[
1\times(x+a) + 1\times(x+b) = (x+a) + (x+b).
\]

Let us compute the polynomial
with the factors $(x+a)(x+b)$. The polynomial is
$x^2 + ax + bx + ab$. Its derivative is
$2x + a + b$. When we apply the product rule to
the factors, we get $(x+a)+(x+b)= x+a+x+b = 2x+a+b$,
which is indeed the same result.

It is intuitively clear that the sum of the factors
is not the same as the product of those same factors
and, even further, that none of the factors is preserved.
They all disappear in favour of others they do not
share divisors with, because, since the factors are
coprime to each other, they do not share divisors
with their sum either.

To elaborate on this, consider now polynomials
with more than two factors of the form

\[
abc\dots,
\]

where $a$, $b$ and $c$ stand for irreducible polynomials like
$(x+\alpha)$, $(x+\beta)$ and so on.

We apply the product rule on the first two factors
and get:

\[
(a'b + ab')\dots
\]

When we now apply the product rule once again,
we would multiply $c$ with the derivative of $ab$
(which is $a'b + ab'$) and the derivative of $c$,
$c'$, with the original $ab$ and get:

\[
(a'b + ab')c + abc' = a'bc + ab'c + abc'.
\]

We see that we end up with the sum of the products
of the original factors, with the current factor $i$
substituted by something else, namely the derivative
of this factor. This can be represented nicely as:

\begin{equation}\label{eq:polyFacProductRule}
\left(\prod_{i=0}^k{a_i}\right)' = 
\sum_{i=0}^k{\left(a_i'\prod_{j\neq i}{a_j}\right)}
\end{equation}

There is a remarkable similarity to the structure
we found in analysing the Chinese remainder theorem,
when we divided the product of all remainders by
the current remainder. Just as in the Chinese remainder
theorem, each of the terms resulting from the product
rule is coprime to the original factor at the same position,
since it is the product of irreducible factors 
(and, hence, coprime to each other) and the derivative
of that factor, which, for sure, does not share
divisors with the original factor at that position.

When we have a repeated factor, however, as in
the following polynomial

\[
(x+a)(x+a)(x+b)\dots,
\]

then this factor is preserved. The product rule
will create the factor $x+a+x+a=2x+2a$, which
is a multiple of the original factor, which,
in its turn, is therefore preserved.

Suppose we want to compute the factorisation
of 

\begin{equation}
f = a_1a_2^2a_3^3\dots a_k^k,
\end{equation}

where the $a$s represent the products of all
the factors raised to the indicated exponent,
then, since the derivative preserves the factors,
the $\gcd$ of $f$ and its derivative $f'$
(whatever that looks like) is:

\begin{equation}\label{eq:polyFacYun1}
\gcd(f,f') = a_2^1a_3^2\dots a_k^{k-1},
\end{equation}

\ie\, the repeated factors with the exponent
decreased by one. Then $f$ divided by the $\gcd$
gives us

\begin{equation}\label{eq:polyFacYun2}
\frac{f}{\gcd(f,f')} = a_1a_2a_3\dots a_k,
\end{equation}

all the factors reduced to their first power.
Now, if we continue this scheme using
the $\gcd(f,f')$ and $f/\gcd(f,f')$ as input,
we would get \ref{eq:polyFacYun1} 
reduced one more ($a_3a_4^2\dots$) and
\ref{eq:polyFacYun2} with the head chopped off
($a_2a_3\dots$). The quotient of the two
versions of \ref{eq:polyFacYun2}, \ie\

\[
\frac{a_1a_2a_3\dots}{a_2a_3\dots},
\]

would give us the head. The head, however,
is the product of all factors with a given exponent.

In a finite field, this, unfortunately does not
work in all cases. Problematic are all coefficients
with exponents that are multiples of the modulus.
When we compute $nc^{n-1}$, for $n$ an exponent
in the original polynomial that is a multiple of
the modulus, the coefficient itself becomes zero.
If we are unlucky, the derivative \term{disappears},
\ie\ it becomes zero. A simple example is the polynomial
$x^4 \pmod{2}$. When we compute the derivative, we get
$4x^3$. Unfortunately, 4 is a multiple of 2 and,
therefore, the only nonzero coefficient we had
in the original polynomial becomes zero and the entire
derivative disappears.

What we can do, however, is to keep the coefficients
with exponents that are multiples of the modulus
separated from those that are not.
As in the algorithm for infinte fields, we would
iteratively compute two sequences of values,
namely $T_{k+1} = T_k/V_{k+1}$ with $T_1 = \gcd(f,f')$
and $V_{k+1} = \gcd(T_k,V_k)$ with $V_1 = f/T_1$.
But we would now deviate for all $k$ that are
multiples of $p$, \viz\: 

\[
V_{k+1} = \begin{cases}
            \gcd(T_k,V_k) & if~p\nmid k~\textrm{(as before)}\\
            V_k & if~p\mid k
          \end{cases}
\]

At each step, we have

\begin{equation}
V_k = \prod_{i\ge k, p\nmid i}{a_i},
\end{equation}

\ie, the product of all $a$s with exponents greater than those
that we have already processed and that do not divide $p$, and

\begin{equation}
T_k = \prod_{i\ge k, p\nmid i}{a_i^{i-k}}
      \prod_{i\ge k, p\mid i}{a_i^i},
\end{equation}

\ie, the product of the powers greater than those 
we have already processed for both cases $p\mid k$ and $p\nmid k$.
For the cases $p\nmid k$, everything is as before.
For the cases $p\nmid k$, we will end up, when we have reduced
$V_k$ to a constant polynomial, with a product of all the powers
of the $a$s with exponents that multiples of $p$.

To get $a$s out, we divide all all exponents by $p$ 
and repeat the whole algorithm. For the return value, \ie\ the
factors, we need to remember the original exponent,
but that is easily done as shown below.

Note that for polynomials with many coefficients,
this recursion step will occur more than once.
The exponents that are multiples of $p$ 
in such a polynomial have the form

\[
0p,p,2p,3p,4p,\dots
\]

Dividing by $p$, we get 

\[
0,1,p,2p,3p,\dots
\]

So, we need to repeat, until there are no more multiples of $p$.
Here is the algorithm:

\begin{minipage}{\textwidth}
\begin{code}
  sqmp :: Integer -> Integer -> Poly Integer -> [(Integer, Poly Integer)]
  sqmp p e u  | degree u < 1  = []
              | otherwise     =  let  u'  = derivative (modmul p) u
                                      t   = gcdmp p u u'
                                      v   = fst (divmp p u t)
                                 in go 1 t v
    where  go k tk vk =  let  vk'  | k `rem` p /= 0 = gcdmp p tk vk
                                   | otherwise      = vk
                              tk'  = fst (divmp p tk vk')
                              k'   = k + 1
                         in case divmp p vk vk' of
                              (P [_],_)  ->              nextStep k' tk' vk'
                              (f,_)      -> (k*p^e,f) :  nextStep k' tk' vk'
           nextStep k tk vk  |  degree vk > 0  = go k tk vk
                             |  degree tk > 0  = sqmp p (e+1) (dividedTk tk)
                             |  otherwise      = []
           dividedTk tk = poly (divExp 0 (coeffs tk))
           divExp _ [] = []
           divExp i (c:cs)  | i `rem` p == 0  = c :  divExp (i+1) cs
                            | otherwise       =      divExp (i+1) cs
\end{code}
\end{minipage}

As usual, the hard work is done in the local function |go|,
which takes three arguments, $k$, $t_k$ and $v_k$.
We initialise $k=1$, $t_k = \gcd(u,u')$ and $v_k = u/t_k$.
We set $v_{k+1} = \gcd(t_k,v_k)$, if $p\nmid k$,
and $v_{k+1}=v_k$, otherwise.
We further set $t_{k+1} = t_k/v_{k+1}$ and $k = k+1$.
If $v_k / v_{k+1}$ is not constant (otherwise it is irrelevant),
we remember the result as the product of factors with this
exponent. Note that the overall result is a list of tuples,
where the first element represents the exponent and
the second the factor product. The exponent is calculated
as $k\times p^e$. The number $e$, here, is not the
Euler-Napier constant, but a variable passed in to |sqmp|.
We would start the algorithm with $e=0$. We, hence, get
$k\times p^0 = k\times 1 = k$ for the first recursion.

The function |nextStep| is just a convenient wrapper
for the decision of how to continue.
If $v_k$ is not yet constant, we continue with 
$go~(k+1)~t_{k+1}~v_{k+1}$.
Otherwise, if $t_k$ is not yet constant, we continue
with |sqmp| with $e+1$ and $t_k$ with exponents
that are multiples of $p$ divided by $p$.

For bootstrapping the algorithm, we can define
a simple function with a reasonable name that
calls |sqmp| with $e=0$:

\begin{minipage}{\textwidth}
\begin{code}
  squarefactormod :: Integer -> Poly Integer -> [(Integer, Poly Integer)]
  squarefactormod p = sqmp p 0 
\end{code}
\end{minipage}

Finally, we are ready to put everything together:

\begin{minipage}{\textwidth}
\begin{code}
  cantorzassenhaus :: Integer -> Poly Integer -> IO [(Integer, Poly Integer)] 
  cantorzassenhaus p u  | irreducible p m  = return [(1,m)]
                        | otherwise        = 
                             concat <$>   mapM mexpcz [(e, ddfac p f) | 
                                          (e,f) <- squarefactormod p m]
    where  m = monicp p u
           expcz e (d,v)   =  map (\f -> (e,f)) <$> cz p d v
           mexpcz (e,dds)  =  concat <$> mapM (expcz e) dds
\end{code}
\end{minipage}

Hans Zassenhaus worked most of his life as a computeralgeabrist and
was important for the development of this area of mathematics and
computer science. He was born in Germany before the second world war
and studied mathematics under Emil Artin, one of the founders of
modern algebra. Zassenhaus' father was strongly influenced by
Albert Schweitzer and, as such, opposed to Nazi ideology.
Hans shared this antipathy and, to avoid being drafted to
a significant war effort like, as it would appear natural
for an algebraist, cryptography, he left university and volonteered for 
the army weather forecast where he survived the war.
Later, he would follow invitations first to the UK and later to the USA,
where he remained until his death.

His sister Hiltgunt (who, after emigrating to the USA,
preferred to use her second name Margret) studied Scandinavistics.
During the war, she worked as translator for censorship in camps
for Norwegian and Danish prisoners. She undermined censorship in
this position, maintained contact between 
prisioners and helped smuggling medicine, tobacco and food
into the prisons. For her efforts during and after the war,
she was nominated for the Nobel Peace Prize in 1974. 
Unfortunately, the societal engagement of the Zassenhaus siblings
is hardly remembered today.

\ignore{
TODO:
- why does x^p^c, with c dividing d is not divided by g?
- application to integers: hensel's lemma
}

