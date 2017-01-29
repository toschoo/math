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

\ignore{
TODO:
- application to integers: hensel's lemma
}

