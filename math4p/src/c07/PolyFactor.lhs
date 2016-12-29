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

Cantor-Zassenhaus makes use of Euler's theorem,
which, as you may remember, states that

\begin{equation}
a^{\varphi(n)} \equiv 1 \pmod{n},
\end{equation}

where $\varphi(n)$ is the totient function
of $n$ counting the numbers $1\dots n-1$ that
are coprime to $n$, \ie that share no divisors with $n$.
For a polynomial ring, we need to interpret $a$ and $n$ as 
being polynomials, not natural numbers.
To stress this fact, we use different letters $f$ and $m$:

\begin{equation}
f^{\varphi(m)} \equiv 1 \pmod{m}.
\end{equation}

From this theorem (which we will not prove, but just assume
that it holds in any field), Fermat's little theorem
follows. Let $K$ be a field with $q$ elements; when using
arithmetic modulo a prime $p$, then $K$ is the field
of numbers $0\dots p-1$, which has $q=p$ elements.
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
When you look at our Haskell definition of polynomials,
you will easily convince yourself that the number of valid
polynomials of a given degree $d$ equals the number of valid
numbers that can be presented in the numeral system base $q$
with $d+1$ digits. If, for instance, $q=2$, then we have

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||r||r||c||}\hline
0     & 2 & |P [0], P [1]|\\\hline
1     & 2 & |P [0,1], P [1,1]|\\\hline
2     & 4 & |P [0,0,1], P [1,0,1], P [0,1,1], P [1,1,1]|\\\hline
3     & 8 & |P [0,0,1,1], P [1,0,1,1], P [0,1,1,1], P [1,1,1,1]|\\
      &   & |P [0,0,0,1], P [1,0,0,1], P [0,1,0,1], P [1,1,0,1]|\\\hline
$\dots$&$\dots$&$\dots$\\\hline
\end{tabular}
\endgroup
\end{center}

We, hence, can precisely say how many polynomials 
of degree $<d$ there are, namely $r=q^d$.
For the example $q=2$, we see that there 16 polynomials
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

This is a nice criterion to build a test for irreducibility.
Here is a Haskell implementation:

\begin{minipage}{\textwidth}
\begin{code}
  irreducible :: Natural -> Poly Natural -> Bool
  irreducible p poly  |  d < 1      = False
                      |  otherwise  = go 1 x
    where  d       =  degree poly
           x       =  P [0,1]
           go i z  =  let z' = powmp p p z
                      in  case pmmod p (sub z' x) poly of
                          P [_]  ->  i == d
                          g      ->  if i < d  then go (i+1) z'
                                               else False
\end{code}
\end{minipage}

The function receives two arguments: the modulus and 
the polynomial we want to check.
First, we compute the degree of the polynomial.
When the polynomial is of degree zero, it is by definition
not irreducible (it is not reducible either, 
it is just constant and as such uninteresting).
Then we start the algorithm beginning with the values 1 and
$x$, where $x$ is the simple polynomial $x$.
In |go|, we raise this polynomial to the power of $p$,
the modulus and subtract it from the result 
taking it modulo the input polynomial |poly|. 
The result of this operation
is $x^{p^d} - x$ for degree $d=1$.

If the result is a constant polynomial 
and the degree counter $i$ equals $d$,
then equation \ref{eq:polyFacIrrTest} is fulfilled.
(Note that we consider any constant polynomial as zero,
since a constant polynomial is just the content,
which usually should have been removed before we start
to search factor.)
Otherwise, if the degree counter does not equal $d$,
this polynomial fulfils the equation with a ``wrong'' degree.
This is possible only if the input was not irreducible
in the first place.

Finally, if we have a remainder that is not constant,
we either continue (if we have not yet reached the degree
in question) with the next degree and the polynomial raised
to $p^i$. Otherwise, if we had already reached our degree,
the polynomial is certainly not irreducible.

Let us look at an example.
We generate a random polynomial of degree 3 modulo 7
(note that, since our implementation of numbers and
polynomials is far from optimal, using greater primes
and higher degrees would take a lot of time!):

|g <- randomPoly 7 4|\\

I get the polynomial |P [3,3,3,4]|.
(Note that you may get another one!)
Calling |irreducible 7 g| says: |False|.

When we raise the polynomial |P [0,1]| 
to the power of $7^3 = 343$,
we get a polynomial of degree 343
with the leading coefficient 1.
When we subtract |P [0,1]| from it,
it will have -1 as last but one coefficient.
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

Can we make anything out of the number
that that we got back as remainder 
for the polynomial that was not
irreducible, \ie\ |P [0,3,6]|?
Let us think: it is a remainder that
does not fulfil the equation 
$x^{q^d} - x \equiv 0 \pmod{g}$.
This should always be true if $g$ is irreducible.
If $g$ is not irreducible, it shares divisors
with some polynomials of lower degrees --
and this is the point!
The polynomial we saw, when taking
$x^{q^d} - x$ modulo $g$ is a polynomial
that shares a common divisor with $g$.
We, hence, can compute the greatest common divisor
calling |gcdmp 7 (P [0,3,6]) (P [3,3,3,4])|
and we get |P [3,6]|.
When we divide |P [3,3,3,4]| by this one,
we expect to get a result without remainder:

|divmp 7 (P [3,3,3,4]) (P [3,6])|\\

and we get: |P [1,6,3]|, 
which is an irreducible polynomial.
We, hence, can factor the polynomial
$4x^3 + 3x^2 + 3x + 3$ modulo 7 into
$3x^2 + 6x + 1$ and $6x + 3$.

Let us verify this result:

\[
(3x^2 + 6x + 1)(6x + 3) = 
(18x^3 + 36x^2 + 6x) + 
(9x^2 + 18x + 3) =
18x^3 + 45x^2 + 24x + 3,
\]

which, modulo 7, is $4x^3 + 3x^2 + 3x + 3$ and,
thus, the correct result.












\ignore{
$q(x) divides  x^p^d - x$ mod p
Cantor+Zassenhaus factoring the product of factors
application to integers: hensel's lemma
}


