\ignore{
\begin{code}
module PolyFactor
where
  import Roots
\end{code}
}
Polynomials can be factored in different contexts,
for instance a field or the integers (which, as
you may remember, do not form a field, but a ring).
These contexts can be generalised to what is called
a \term{unique factorisation domain}.
A unique factorisation domain is a commutative ring $R$,
where

\begin{itemize}
\item $uv \neq 0$, whenever $u,v \in R$ and 
      $u \neq 0$ and $v \neq 0$;
\item every nonzero element is either a \term{unit}
      or a \term{prime} or can be uniquely represented 
      as a product of primes;
\item every unit $u$ has an inverse $v$, such that
      $uv = 1$.
\item a prime $p$ is a nonunit element for which an
      equation of the form $p = qr$ can be true, only 
      if either $q$ or $r$ is a unit.
\end{itemize} 

The integers form a unique factorisation domain,
with the units 1 and -1 and the primes are
$\pm 2, \pm 3, \pm 5, \pm 7, \dots$.
We can easily verify that 1 and -1 obey to
the definition of unit, 
when we assume that each one is its own inverse.
We can also agree that the primes are primes
in the sense of the above definition: for any prime
in $p \in \mathbb{Z}$, if $p = qr$, then either 
$q$ or $r$ must be a unit and the other must equal $p$.
That is the definition of primes.

A field is trivially a unique factorisation domain
without primes where all elements are units.

The simplest notion of factoring in such a domain
is the factoring into \term{primitive part} and
\term{content}. This, basically, splits a polynomial
into a number (in the domain we are dealing with) and
a \term{primitive polynomial}. 

With the integers, the content is the \acronym{gcd} of the
coefficients. For instance, the \acronym{gcd} 
of the coefficients of the polynomial 
$9x^5 + 27x^2 + 81$ is 9. 
When we divide the polynomial by 9 we get
$x^5 + 3x^2 + 9$.

For rational numbers, we would choose a fraction
that turns all coefficients into integers that do
not share divisors. The polynomial

\[
\frac{1}{3}x^5 + \frac{7}{2}x^2 + 2x + 1,
\]

for instance, can be factored dividing 
all coefficients by $\frac{1}{6}$:

\[
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{array}{rcrcr}
\frac{1}{3} & \times & 6 & = & 2\\
\frac{7}{2} & \times & 6 & = & 21\\
2 & \times & 6 & = & 12\\
1 & \times & 6 & = & 6
\end{array}
\endgroup
\]

We, hence, get the product
$\frac{1}{6}(2x^5 + 21x^2 + 12x + 6)$.

This, however, is not the end of the story.
Consider the polynomial 

\[
3x^2 - 27.
\]

We can factor this one into
$3(x^2 - 9)$, with the second part being
primitve: the \acronym{gcd} of its coefficients
is 1. But we can factor it further.
Obviously, we have

\begin{equation}
x^2 - 9 = (x - 3)(x + 3).
\end{equation}

The complete factorisation of the polynomial
$3x^2 - 27$, hence, is $3(x-3)(x+3)$.

For factoring primitive polynomials manually, there
are many different methods (most of which have a
video on youtube). They share one property:
they are highly inefficient, when it comes
to polynomials of larger degrees or with big
coefficients. They, basically, all use integer
factorisation of which we know that it is extremely
expensive in terms of computation complexity.
Instead of going through all of them, we will here
present a typical classical method, namely Kronecker's
method.

Kronecker's method is a distinct-degree approach. 
That is, it searches for the factors of a given degree.
We start by applying the polynomial to $n$ distinct values,
for $n$ the degree of the polynomial plus 1.
That is because, to represent a polynomial of degree $d$,
we need $d+1$ coefficients, \eg\ |P [0,0,1]| has three
coefficients and represents the polynomial $x^2$, which is
of degree two.

The rationale of applying the polynomial is the following:
When the polynomial we want to factor generates
a given set of values, then the product of the factors
of that polynomial must generate the same values.
Any factor must, hence, consist of divisors of those values. 
The number of integer divisors
of those values, however, is limited.
We can therefore afford, at least for small polynomials
with small coefficients, trying all the combinations
of the divisors.

We have already defined a function
to find the divisors of a given number,
when we discussed Euler's totient function.
However, that function dealt with natural numbers
only. We now need a variant that is able to
compute negative divisors.
It would be also nice if that function
could give us not only the divisors,
but additionally the additive inverse,
\ie\ the negation of the divisors, because,
in many cases, we need to look at the negative
alternatives too. Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  divs :: Zahl -> [Zahl]
  divs i  |  i < 0      = divs (-i) 
          |  otherwise  = ds ++ map negate ds
    where ds = [d | d <- [1..i], rem i d == 0] 
\end{code}
\end{minipage}

The divisors are now combined to yield
$n$-tuples, $n$, still the degree of the factor
plus one, where each divisor represents
one coefficient of the resulting polynomial.
But before we can convert the $n$-tuples
into polynomials, we need to create all
possible permutations, since the polynomial
|P [a,b]| is not the same as |P [b,a]| if
$a \neq b$.
From this we obtain a (potentially very large)
list of $n$-tuples that we then convert
into polynomials. From that list,
we finally filter those polynomials
for which |p `divp` k == 0|, where $p$ is the
input polynomial and $k$ the candidate in 
the list of polynomials. Here is an implementation
(using lists instead of $n$-tuples):

\begin{minipage}{\textwidth}
\begin{code}
  kronecker :: Poly Zahl -> [Zahl] -> [Poly Quoz]
  kronecker (P cs) is = nub [a | a <- as, snd (r `divp` a) == P [0]]
    where  ds  =  map divs is
           ps  =  concatMap perms (listcombine ds)
           as  =  map (P . map fromInteger) ps
           r   =  P [c%1 | c <- cs]
\end{code}
\end{minipage}

Note that, since we use |divp|, we need to convert
the integer polynomial to a rational polynomial.

There are two combinatorial functions, |perms| and |listcombine|.
We have already defined |perms|, when discussing
permutations. The function generates all permutations
of a given list. The other function, |listcombine|,
however, is new. It creates all possible combinations
of a list of lists. Here is a possible implementation:

\begin{minipage}{\textwidth}
\begin{code}
  listcombine :: [[a]] -> [[a]]
  listcombine  []      =  []
  listcombine  ([]:_)  =  []
  listcombine  (x:xs)  =  inshead (head x) (listcombine xs) ++ 
                          listcombine ((tail x):xs)

  inshead :: a -> [[a]] -> [[a]]
  inshead x []  =  [[x]]
  inshead x zs  =  map (x:) zs
\end{code}
\end{minipage}

Let us try |kronecker| on some polynomials.
First, we need to apply the input polynomial
to get $n$ results. For instance, we know
that the polynomial $x^2 - 9$ has factors
of first degree. We, therefore, apply it on
two values: |let vs = mapply (P [-9,0,1]) [0,1]|
and get for |vs|: |[-9,-8]|.
Now we call |kronecker (P [-9,0,1]) [-9,-8]|
and get:

|P [3 % 1,1 % 1]|\\
|P [3 % 1,(-1) % 1]|\\
|P [(-3) % 1,1 % 1]|\\
|P [(-3) % 1,(-1) % 1]|

Those are the polynomials $x+3$, $-x+3$, $x-3$ and $-x-3$.
By convention, we exclude the polynomials starting with
a negative coefficients by factoring out -1 first.
However, we can easily see that all of them are actually
factors of $x^2 - 9$, since

\begin{equation}
(x+3)(x-3) = (x^2-9)
\end{equation}

and

\begin{equation}
(-x+3)(-x-3) = (x^2-9).
\end{equation}

Here is another example: $x^5 + x^4 + x^2 + x + 2$.
We want to find a factor of degree 2,
so we apply the polynomial to three values, say,
|[-1,0,1]|. The result is |[2,2,6]|.
We run |kronecker (P [2,1,1,0,1,1]) [2,2,6]| 
and, after a short while, we get:

|P [1 % 1,1 % 1,1 % 1]|\\
|P [2 % 1,2 % 1,2 % 1]|\\
|P [(-1) % 1,(-1) % 1,(-1) % 1]|\\
|P [(-2) % 1,(-2) % 1,(-2) % 1]|,

which corresponds to the polynomials
$x^2+x+1$, $2x^2+2x+2$,
$-x^2-x -1$ and $-2x^2-2x-2$.
Ony the first one is a primitive polynomial.
We can factor out 2 from the second one,
leaving just the first one;
polynomials three and four, simply, are 
the negative counterparts of one and two,
so we can factor out -1 and -2, respectively,
to obtain again the first one.

To check if the first one is really a factor
of the input polynomial we divide:

|P [2,1,1,0,1,1] `divp` P [1,1,1]|\\
 and get
|P [2,-1,0,1]|, which corresponds to 
$x^3 - x + 2$. Indeed:

\begin{equation}
(x^2 + x + 1)(x^3 - x + 2) = x^5 + x^4 + x^2 + x + 2.
\end{equation}

Kronecker's method is just a brute force search.
It is obvious that it is not efficient and it fails
with growing degrees and coefficients.
Modern methods to factor polynomials use
much more sophisticated techniques.

They are, in particular, based on modular arithmetic
of polynomials and make use of theorems that we have
already discussed in the ring of integers.
Polynomials with coefficients in a ring (or field)
form a ring too, a polynomial ring.
Theorems that hold in any ring, hence, hold also
in a polynomial ring. We, therefore, do not need
to prove them here again.

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
is just one over several pieces. The main parts are
ring and field theory and are based on Euler's theorem,
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
constitute a ring, this time on top of the finite field.
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
It follows from Lagrange's theorem 
(that the cardinality of subgroups of $G$ divide
the cardinality of $G$) that
$f^{\varphi(m)} \equiv f_m^{\varphi(m)} \equiv 1 \pmod{m}\qed$.

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

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||r||r||c||}\hline
degree & size & polynomials\\\hline\hline
0      & 2    & |P [0], P [1]|\\\hline
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
since this polynomial is the zero polynomial
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
                      in  case pmmod p (add z' (P [0,p-1])) u of
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
that we give the result, again, back as a monic
polynomial. Each result is a tuple of the degree and
the corresponding factor product.

\begin{minipage}{\textwidth}
\begin{code}
  ddfac :: Natural -> Poly Natural -> [(Int, Poly Natural)]
  ddfac p u   = go 1 u (P [0,1])
    where  n  = degree u
           go d v x  |  degree v <= 0  = []
                     |  otherwise      = 
                        let  x'        = powmp p p x 
                             t         = add x' (P [0,p-1])
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
The algorithm presented here, in fact,
works properly only with squarefree polynomials.
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

We can conclude that these are indeed the factors
of $u$ and that they contain all the factors.
But obviously, |P [2,4,1]| or $x^2 + 4x + 2$ is
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
$t$ and and $u$. The only thing we have to do is to split them,
so that the problem reduces significantly. We can split $t$
into three parts using the equality

\begin{equation}
t^{p^d} - t = t(t^{(p^d-1)/2} + 1)((t^{(p^d-1)/2} - 1)
\end{equation}

and by a careful choice of $t$, we can make sure that the factors
are likely to be more or less equally distributed among the
latter two factors. That would reduce the problem significantly.

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
the $\gcd$ and the quotient of $u$ and the $\gcd$ further.
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

When we compute this modulo 2, again, all terms with even coefficients
fall away and we finally get

\begin{equation}
w^2 + w = t^{2^d} + t.
\end{equation}

The point of all this is that we can split the expression $w^2 + w$
into two more or less equal parts, just by factoring $w$ out:
$w(w+1)$. Now, it is again very probable that we find common divisors
in one of the factors, $w$ or $w+1$.
Here is the implementation of the Cantor-Zassenhaus algorithm:

\begin{minipage}{\textwidth}
\begin{code}
  cz :: Natural -> Int -> Poly Natural -> IO [Poly Natural]
  cz p d u  | n <= d     = return [monicp p u]
            | otherwise  = do 
    x <- monicp p <$> randomPoly p (2*d) -- 2*d-1
    let t  | p == 2     = addsquares (d-1) p x u
           | otherwise  = add (powmodp p m x u) (P [p-1])
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
we raise the random polynomial to the power of $p^d-1/2$
and subtract 1. That is the third factor of equation
\ref{eq:polyFac_CZ1}. We compute the $\gcd$ and,
if the result has either degree 0 (no factor was found)
or degree equal to $u$ (all factors are in this one),
then we just try again with another random polynomial.
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
                             w'  =  modp p (add w t')
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


\ignore{
TODO:
- derivative modulo p
- squarefactor
- Euler's theorem: why does f = fm = 1 mod(m)?
- why does x^p^c, with c dividing d is not divided by g?
- why do g and g' do not share divisors if squarefree
- application to integers: hensel's lemma
}

