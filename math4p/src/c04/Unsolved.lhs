\ignore{
\begin{code}
module Unsolved
where
  import Natural
  import Modular
\end{code}
}

In the summer of 1742, 
Christian Goldbach (1690 -- 1764), 
a mathematician and diplomat in the service
of the Russian Czar,
wrote a letter to Leonhard Euler.
In this letter he told Euler of an observation he made:
every even natural number greater than 2 can be represented
as a sum of two primes.
He had tested this with a lot of numbers --
basically, with any number he could find --
but was unable to prove the conjecture.
Euler was excited about the observation and
answered that he was sure that 
the conjecture must be true but that he was
unable to demonstrate it either.
This, basically, is still the state of affairs today:
There is a lot of evidence 
that Goldbach's conjecture is true,
much more than in the times of Euler and Goldbach,
but there is no rigorous proof.

Let us see some examples:

\begin{minipage}{\textwidth}
$4 = 2 + 2$,\\
$6 = 3+3$,\\
$8 = 3 + 5$,\\
$10 = 3 + 7 = 5 + 5$, \\
$12 = 5 + 7$,\\
$14 = 7 + 7 = 3 + 11$, \\
$16 = 3 + 13$
\end{minipage}

and so on.

Here is a function to compute 
for any number $n$ one of the sums of two primes
that equal $n$:

\begin{minipage}{\textwidth}\begin{code}
  goldbach :: Natural -> (Natural, Natural)
  goldbach n  |  odd n      = error "not even"
              |  n == 2     = error "not greater 2"
              |  otherwise  = go allprimes
    where  go []  = error "The end is ny!"
           go (p:ps)  | p > n - 2  = error ("disproved: " ++ show n)
                      | otherwise  =  let q = n - p 
                                      in if prime q  then (p,q)
                                                     else go ps
\end{code}\end{minipage}

The really interesting point 
is the second error in the |go| function.
When you can find a number, for which this error occurs,
there is certainly some mathematical honour you can earn.

One can think of many ways to find numbers
to which to apply the |goldbach| function, the result
is always a prime tuple, \eg\ powers of 2:

$(2,2),(3,5),(3,13),(3,29),(3,61),(19,109),(5,251),(3,509),(3,1021),$

the year of the first
known formulation of the conjecture: |goldbach 1742 = (19,1723)|,
safe primes: |map goldbach (map (+1) safeprimes)|:

$(7,257),(11,337),(7,353),(5,379),(5,463),(13,467),(5,499),(7,557),(11,577),\dots$

or the \acronym{ascii} code of the word ``goldbach'':\\
|goldbach $ read $ concatMap (show . ord) "goldbach"|: $\dots$
(This will take a while to compute, so let us continue $\dots$)

There is so much evidence in favour of the correctness
of the conjecture that it is considered to be true 
by most mathematicians today.
But there is still no rigorous proof.
We cannot claim the truth of the conjecture
just by applying it to a finite set of numbers,
even if that set is incredible large.
The point is of course that there are infinitely many numbers.
For any number $n$ up to which we may have tested the property,
there may be a larger number $n+k$ for which the property
does not hold. By just computing concrete results for given numbers,
we can therefore not prove the theorem;
we can only disprove it this way by finding a number $n$
for which the property does not hold.

The Goldbach property, \ie\ the property of a number
to be representable by the sum of two primes,
is so prototypical for many problems in mathematics
that properties of this kind are often called
\term{Goldbach-like} in mathematical logic.
Goldbach-like properties can be easily
calculated for any given number $n$, 
but there is no way of proving 
that such a property holds for all numbers other
than going through all of them and actually
calculating it for each and every one. 
Since we cannot go through all numbers
in a finite number of steps, this kind of statements,
even though calculable for any given instance, 
cannot be calculated from the axioms 
of a given formal system.

There is nothing special about the Goldbach conjecture
itself that would yield this characteristic.
In fact, many unproven conjectures share it.
That every number can be factored
into primes, for instance, is a Goldbach-like statement too.
For this statement, we actually have a simple proof.
We even have a proof 
for the much stronger fundamental theorem of arithmetic
that states that every number
has a \emph{unique} prime factorisation 
(which is not a Goldbach-like statement). 
If we did not have a proof of the fact
that every number can be factored into primes,
the only technique we would have at hand
to prove or disprove the statement
would be to find a counterexample.
Testing numbers for this property as such would be quite simple,
since we just have to factor given numbers
and say whether they can be factored into primes or not.
Until now, however,
the property has turned out to be true
for any number we have looked at.
If we not had the proof of the theorem,
we could imagine that 
there might be a counterexample lurking
among the infintely many numbers
we have not yet examined.
But there are not enough computing resources
to look at all of them in finite time.

A simple property
that is not a Goldbach-like statement
is the twin prime conjecture,
which we already encountered, stating
that there is an infinite number of pairs of primes
$p$ and $q$, such that $q = p + 2$. 
Examples are 3 and 5, 5 and 7,
11 and 13, 17 and 19, 29 and 31 and many
other number pairs. Unlike the Goldbach conjecture,
we cannot disprove the twin prime conjecture
by finding a counterexample.
The conjecture states that there are infinitely
many twin primes. There is thus
no criterion to conclude from the fact
that a pair of numbers $(n,m)$ 
does not have the property of being twin primes
that the conjecture is false.
We could, for instance, get excited
about the fact that 23 has no twin
and declare 17 and 19 the last pair of primes.
However, when we go on, we will find 29 and 31.
The only way to disprove
the twin prime conjecture is therefore
to go through all numbers, which,
again, is not possible in a finite
number of steps.

A similar is true for the fundamental theorem of arithmetic.
We cannot decide this theorem even for a single number.
For any number we factor, we additionally have to compare
the obtained factorisation with those of all other numbers
(including those we have not yet examined)
to decide if the factorisation is unique.
Establishing the property for a single number
already includes infinitely many steps and is
therefore not possible in practical terms.
Apparently, there is no way to prove this theorem
but in an indirect fashion.

Another complex of open problems is 
factoring itself for which,
as we have seen, no efficient algorithm
is known. There are ways to find 
the prime factors of a given number, of course.
But with large numbers, factoring becomes
resource-intensive. We need a lot of steps
to find the factors. 

The factoring problem is tightly coupled
with the problem of solving the discrete logarithm.
We got an idea of this coupling already, 
when we looked at the
Carmichael theorem: when we know the factors
of a number, we can easily find a number $t$,
such that $a^t \equiv 1 \pmod{n}$, 
where $a$ and $n$ are coprime.
Shor's quantum factoring algorithm, actually,
exploits this coupling the other way round.
It finds the number $t$ and uses $t$
to find two prime factors of $n$.

In the world of classic, non-quantum computing, 
we know of only
one way to find $t$, the order of the group
generated by $a$, namely
to raise $a$ to the sequence of numbers 
$1,2,\dots, n-1$ until $a$ raised to one
of these numbers is $1 \pmod{n}$.
Here is a Haskell function that
uses this logic to find $t$ for a given $a$:

\begin{minipage}{\textwidth}\begin{code}
  order :: Natural -> Natural -> Natural
  order n a = go 2 (a*a)
    where go t a'  | a' `rem` n == 1  =  t
                   | t >= n           =  error "group exhausted"
                   | otherwise        =  go (t+1) (a'*a)
\end{code}\end{minipage}

Note that we introduce a guard 
that saves us from looping eternally:
when we reach $t = n$, we abandon
the function.
We will see why we do this in a second.

Once we have found $t$,
it is easy to find the factors.
We start -- once again -- with the equation

\begin{equation}
a^t \equiv 1 \pmod{n}
\end{equation}

and subtract 1 from both sides yielding

\begin{equation}
a^t - 1 \equiv 0 \pmod{n}.
\end{equation}

Then we factor the left-hand side of the equation:

\begin{equation}
(a^{\frac{t}{2}} - 1)(a^{\frac{t}{2}} + 1) \equiv 0 \pmod{n}.
\end{equation}

In other words,
the product of $a^{\frac{t}{2}} - 1$ and $a^{\frac{t}{2}} + 1$
is congruent to 0 modulo $n$, \ie\
this product is a multiple of $n$.
That, in its turn, means that $n$ and this product
must have common factors. Consequently,
$\gcd(a^{\frac{t}{2}} - 1, n)$ or 
$\gcd(a^{\frac{t}{2}} + 1, n)$
will produce at least one factor of $n$.

This does not work in all cases, however.
First, $t$ must be even;
otherwise $\frac{t}{2}$ would not be natural number.
If $t$ is odd, we therefore have to look for another $a$.
Second, $a$ must be coprime to $n$.
On the other hand, If $a$ is not coprime to $n$,
then we have already found a factor of $n$,
namely the $\gcd$ of $n$ and $a$.
Third, $a^{\frac{t}{2}}$ should not be
$n-1$, since in that case
$a^t$ is trivially $1 \pmod{n}$.
Finally, $n$ should be squarefree.
If $n$ is not squarefree,
some numbers $a$ will fulfil the equation
$a^t \equiv 0 \pmod{n}$.
That is, there are remainders of $n$
that, multiplied by themselves,
will yield a multiple of $n$.
In that case, we will hit the error
``group exhausted'' in the |order| function above.
The property of a number 
being squarefree or not, however,
is hard to establish.
If we introduce a test 
at the beginning of the algorithm,
we are back where we started:
an algorithm that takes 
an exponential number of steps.
We therefore have to accept
that an error may occur for any number
on which we apply Shor's algorithm.

Here is a simple Haskell implementation
of the classical part of Shor's algorithm
using the order function defined above:

\begin{minipage}{\textwidth}\begin{code}
  shorfact :: Natural -> IO [Natural]
  shorfact 0  = return []
  shorfact 1  = return []
  shorfact 2  = return [2]
  shorfact n  | even n     =  do  fs <- shorfact (n `div` 2) 
                                  return (2:fs)
              | otherwise  =  do  p <- rabinMiller 16 n
                                  if p then return [n] else loop
    where  loop = do  a <- randomNatural (3,n-2)
                      case  gcd n a of 
                            1  ->  check a (order n a)
                            f  ->  do  fs1 <- shorfact (n `div` f)
                                       fs2 <- shorfact f
                                       return (fs1 ++ fs2)
           check a t  | odd t = loop
                      | (a^(t `div` 2)) `rem` n == n-1 = loop
                      | otherwise =  let  f1 = gcd n (a^(t `div` 2) - 1) 
                                          f2 = gcd n (a^(t `div` 2) + 1) 
                                          f  | f1 == 1    =  f2
                                             | otherwise  =  f1
                                     in do  fs2 <- shorfact (n `div` f)
                                            fs1 <- shorfact f
                                            return (fs1 ++ fs2)
\end{code}\end{minipage}

As usual, we start with some trivial base cases:
0 and 1 have no factors; 2 is the first prime
and has only one factor, namely 2 itself.
Then, if $n$ is even, we apply the algorithm
to half of $n$ and add 2 to the resulting list of factors.
Otherwise, we first check if $n$ is prime
using the Rabin-Miller test (with a relaxed repetition value).
If it is prime, there is only one factor, namely $n$.
Otherwise, we enter |loop|.

Here, we start by choosing a random number
that is not in one of the trivial groups of $n$
and test if we have been lucky 
by checking if $\gcd(n,a) = 1$.
If it is not 1, then we have found a factor by chance.
We continue with |shorfact| on 
$n$ dividied by $f$ and on $f$,
which still could be a composite factor of $n$.
Finally, we merge the two resulting lists
of factors.

Otherwise, we call |check|.
This function decides how to continue
depending on the result of |order|:
we may need to start again,
if $t$ does not fulfil the preconditions,
or we continue with the $\gcd$s.
If $t$ is odd, $a$ turns out to be useless
and we start again with another $a$.
If $a^{\frac{t}{2}}$ is $n-1$,
$a$ is again useless and we start with another $a$.
Otherwise, we compute the $\gcd$s of
$a^{\frac{t}{2}} \pm 1$. 
We take one of the results to continue
just making sure that, should one of the values be 1,
we take the other one.
That is a bit sloppy of course, since
the case that both results are 1 is not handled.
If that happens, however, something must be wrong 
in our math and, then, we cannot guarantee
that the result is correct at all.

The quantum magic enters in the |order| subroutine.
The algorithm that finds the order of $a$
uses techniques that are far from what we have
learnt so far, in particular \term{Fourier analysis}.
Fourier analysis is a technique 
that represents complicated functions
in terms of simpler and well-understood functions,
namely trigonometric functions.
The idea is that the quantum processor
is initialised with a \term{superposition}
of the period of the function 
$f(x) = x^t \bmod{n}$,
which is then simplified reducing $f$ 
by Fourier analysis.

This sounds like alchemy -- and, yes,
basically it is alchemy,
quantum alchemy to be specific.
We will look at Fourier analysis later,
when we have studied the calculus.
But I cannot promise that you will understand
quantum alchemy when you will have understood Fourier.
I am not sure, in fact,
if even quantum alchemists 
understand quantum alchemy.

The result of the quantum Fourier analysis,
as always in quantum computing,
is correct with a certain probability.
The whole algorithm, therefore,
must be repeated and we must ensure
that there is one result that appears
significantly more often than others. 
The reasoning here is similar to the reasoning
we have already applied to the Rabin-Miller test.
So, this part of the algorithm 
(which we have left out above)
should not be shocking.
Shocking is rather the fact
that there actually is a quantum algorithm
that solves mathematical problems that cannot be solved (yet)
on classic computers.
This may be a hard boundary that cannot be passed,
so that we have to accept that 
there are problems that can be solved
in the classic world and others than can be solved
only in the quantum world.
This is an open and, indeed, very deep question
in modern mathematics and computer science.
But let us come back to primes, which,
for my taste, are spooky enough.

The most fundamental unsolved problems
deal with the distribution of primes
and the \term{prime number theorem}.
To investigate the distribution of primes,
we can start by counting 
primes up to a given number $n$,
a function usually called $\Pi(n)$.
We can implement $\Pi$ in Haskell as:

\begin{minipage}{\textwidth}\begin{code}
  countp :: Natural -> Int
  countp n = length (takeWhile (<n) allprimes)
\end{code}\end{minipage}

$\Pi(10)$ or, in Haskell, |countp 10|
gives 4, since there are 4 primes less than 10:
2, 3, 5 and 7. Here are the values for 
some small powers of 10:

\begin{align*}
10^1 & & = & & 4\\
10^2 & & = & & 25\\
10^3 & & = & & 168\\
10^4 & & = & & 1229\\
10^5 & & = & & 9592
\end{align*}

Such tables of values of $\Pi$
were already compiled in the early $18^{th}$ century.
From some of such tables Legendre conjectured
that $\Pi$ is somehow related to the
natural logarithm (a beast we already met
without much explanations in the first chapter).
Specifically, he proposed that 
$\Pi(n) = \frac{n}{\ln(n) + c}$,
where $c$ is some constant.
Gauss and later his student 
Peter Gustav Lejeune Dirichlet (1805 -- 1859)
improved this conjecture with the 
\term{logarithmic integral}, 
a concept from calculus and,
as such, far ahead on our way.
Intuitively, the integral yields
the area under a curve;
the logarithmic integral 
is the area under the curve described by
the function $\frac{1}{\ln(n)}$,
which looks like this:

\begin{center}
\begin{tikzpicture}[trim axis left]
\begin{axis}[
  domain=1:10,
  samples=100,
  % enlarge x limits=true,
  ymin=0,ymax=10,
  xmin=1.2,xmax=10,
  %grid=both,
  %axis equal,
  no markers]
\addplot +[thick] {1/ln(x)};
\end{axis}
\end{tikzpicture}
\end{center}

Let us compare the precision of these two approximations.
The following table lists the values for $\Pi(n)$
and the differences $\frac{n}{\ln(n)} - \Pi(n)$ and
$li(n) - \Pi(n)$, where $li(n)$ is the logarithmic integral of $n$:

\begin{center}
\begin{tabular}{||l||r||r||r||}
\hline
$n$     & $\Pi(n)$ & $n/\ln(n)$ & $li(n)$\\\hline\hline
10      &        4 &        0 &    2 \\\hline
$10^2$  &       25 &       -3 &    5 \\\hline
$10^3$  &      168 &      -23 &   10 \\\hline
$10^4$  &     1229 &     -143 &   17 \\\hline
$10^5$  &     9592 &     -906 &   38 \\\hline
$10^6$  &    78498 &    -6116 &  130 \\\hline
$10^7$  &   664579 &   -44158 &  339 \\\hline
$10^8$  &  5761455 &  -332774 &  754 \\\hline
$10^9$  & 50847534 & -2592592 & 1701 \\\hline
$\dots$ &  $\dots$ &  $\dots$ & $\dots$
\end{tabular}
\end{center}

The error of the logarithmic integral grows much slower
than that of Legendre's conjecture.
For small numbers, the error of the $li$ variant
is still greater. But already for $n = 10^3$, 
Legendre overtakes $li$ and is then increasing orders
of magnitude faster than $li$.

This is not the last word, however.
There is a function that is still more precise
in most cases.
In his famous paper, 
``On the Number of Primes less than a given Magnitude'',
the ingenious mathematician Bernhard Riemann (1826 -- 1866)
not only introduced the most tantalizing of all
unsolved mathematical problems,
the \term{Riemann Hypothesis},
but he also proposed a refinement to Gauss/Dirichlet's 
$\Pi$-approximation. He conjectured that $\Pi$ 
corresponds roughly to the infinite series

\begin{equation}
R(n) = li(n) - \frac{1}{2} li(n^{\frac{1}{2}})
             - \frac{1}{3} li(n^{\frac{1}{3}})
             - \frac{1}{5} li(n^{\frac{1}{5}})
             + \frac{1}{6} li(n^{\frac{1}{6}})
             - \dots
\end{equation}

That the number of primes is related to the natural logarithm
is already an astonishing fact.
But, now, Riemann goes even further.
To see what it is that is so surprising 
about Riemann's improvement, we present the equation above
in a more compact form:

\begin{equation}
R(n) = \sum_{k=1}^{\infty}{\frac{\mu(n)}{k}li(n^{\frac{1}{k}}})
\end{equation}

The function $\mu(n)$ appearing in the formula
is the Möbius function, which yields 0, 1 or -1
depending on $n$ being squarefree and 
the number of prime factors of $n$ being
even or odd.
To remind you of the first values 
of the Moebius function:
$\mu(1) = 1$, $\mu(2) = -1$, $\mu(3) = -1$,
$\mu(4) = 0$, $\mu(5) = -1$ and $\mu(6) = 1$
leading to the first terms of the summation above:
$\frac{1}{1}li(n^{\frac{1}{1}}) = li(n)$, 
$\frac{-1}{2}li(n^{\frac{1}{2}})$,
$\frac{-1}{3}li(n^{\frac{1}{3}})$,
$\frac{0}{4}li(n^{\frac{1}{4}}) = 0$,
$\frac{-1}{5}li(n^{\frac{1}{5}})$ and
$\frac{1}{6}li(n^{\frac{1}{6}})$.
In other words:
the Möbius function is intimately related
to the concept of the distribution of primes
through the prime number theory,
even if it does not reveal any regularity 
at the first and even the second sight.

For many values of $n$, the Riemann refinement
is much closer to the real value of $\Pi$
than either Legendre's try and Gauss/Dirichlet's
improvement. It is esitmated that it is better
99\% of the time. Occasionally, however,
Riemann's value is worse than that of Gauss/Dirichlet.
The latter fact is known only theoretically,
no specific $n$ is known that makes Riemann
worse. In most cases, Riemann's value
is even much better, for instance:
for $10^9$, Gauss/Dirichlet's deviation is
1701 while Riemann's difference is only -79;
The difference of Gauss/Dirichlet for $10^{16}$
is more than 3 million; Riemann's difference is
\num{327052} and thus 10 times better.

The prime number theorem is usually stated
as an asymptotic law of the form:

\begin{equation}
\lim_{n \to \infty} \frac{\Pi(n)}{n/\ln(n)} = 1,
\end{equation}

which means that for large $n$,
the relation of $\Pi(n)$ and $\frac{n}{\ln(n)}$
approximates 1.
A first attempt to prove the theorem
was made by the Russian mathematician 
Pafnuty Lvovich Chebyshev (1821 -- 1894).
Even though his paper failed to strictly prove
the theorem, he could prove Bertrand's postulate,
which states that there is at least one prime number
between $n$ and $2n$ for $n \ge 2$
and on which we have already relied
when looking at cryptography.
A proof was finally given at the the end
of the $19^{th}$ century 
independently by the French mathematicians
Jacques Hadamard (1865 -- 1963) and
Charles Jean de la Vallée-Poussin (1866 -- 1962).

It is unknown until today, though,
if strict error margins for the approximation
of $\Pi$ can be given and if the available
approximations can be further improved.
Many of these questions are related to
Riemann's Hypothesis. 
But mathematicians today appear to be far
from producing a proof (or refutation) of
this mega-hypothesis.
