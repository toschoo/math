\ignore{
\begin{code}
module Gamma
where
  import Natural
  import Quoz
\end{code}
}

The \term{harmonic series} is defined as

\begin{equation}
\sum_{n=1}^{\infty}{\frac{1}{n}} = 1 + 
  \frac{1}{2} + 
  \frac{1}{3} + 
  \frac{1}{4} + \dots
\end{equation}

A harmonoic series with respect to a given number $k$,
called the \term{harmonic number} $H_k$, then is

\begin{equation}
\sum_{n=1}^k{\frac{1}{n}} = 1 + 
  \frac{1}{2} + \dots +
  \frac{1}{k} 
\end{equation}

This is easily implemented in Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  harmonic :: Natural -> Double
  harmonic n = sum [1/d | d <- map fromIntegral [1..n]]
\end{code}
\end{minipage}

Some harmonic numbers are (|map harmonic [1..10]|):

$1, 1.5, 1.8\overline{3}, 2.08\overline{3}, 2.28\overline{3}, 
 2.44\overline{9}, 2.5928,2.7178, 2.8289, 2.9289.$

The harmonic series is an interesting object of study
in its own right. Here, however, we are interested in 
something else. Namely, the difference of the harmonic series
and the natural logarithm:

\begin{equation}
\gamma = \lim_{n \to \infty} H_n - \ln(n).
\end{equation}

We can implement this equation as

\begin{minipage}{\textwidth}
\begin{code}
  harmonatural :: Natural -> Double
  harmonatural n = harmonic n - ln n
    where ln = log . fromIntegral
\end{code}
\end{minipage}

Applied on the first numbers with
|map harmonatural [1..10]|, the function
does not show interesting results:

$1.0, 0.8068,0.7347,0.697,0.6738,0.6582,0.6469,0.6384,0.6317,0.6263,\dots$

Applied to greater numbers, however, the results approach a constant value:

|harmonatural    100 = 0.58220|\\
|harmonatural   1000 = 0.57771|\\
|harmonatural  10000 = 0.57726|\\
|harmonatural 100000 = 0.57722|

With even greater numbers, the difference converges to \num{0.57721}.
This number, $\gamma$, was first mentioned by -- surprise -- 
Leonhard Euler and some years later 
by Italian mathematician Lorenzo Mascheroni (1750 -- 1800) and
is therefore called the Euler-Mascheroni constant.

This mysterious number appears in different contexts and, apparently,
quite often as a difference or average.
An ingenious investigation was carried out by Belgian mathematician
Charles Jean de la Vallée-Poussin (1866 -- 1962) who is famous
for his proof of the Prime number theorem.
Vallée-Poussin studied the quotients of a number $n$ 
and the primes up to that number. If $n$ is not prime itself,
then there are some prime numbers $p$, 
namely those of the prime factorisation of $n$,
such that $\frac{n}{p}$ is an integer. 
For others, this quotient is a rational number,
which falls short of the next natural number.
For instance, there are four prime numbers less than 10:
2, 3, 5 and 7. The quotients are

$5, 3.\overline{3}, 2, 1.\overline{428571}$.

5 and 2, the quotients of 2 and 5, respectively,
are integers and there, hence, is no difference.
The quotient $\frac{10}{3} = 3.\overline{3}$, however,
falls short of 4 by $0.\overline{6}$ and
$\frac{10}{7} = 1.\overline{428571}$ falls short of 2 by
$0.\overline{57142828}$.

Vallée-Poussin asked what the average of this difference is.
For the example 10, the average is about
$0.3\overline{095238}$. 
One might think that this average, computed for many numbers
or for very big numbers, is about $0.5$, so that the probability
for the quotient of $n$ and a random prime number 
to fall into the first or the second half of the rational numbers
between two integers is equal, \ie\ $50\%$ for both cases.
It turns out it is not.
For huge numbers, de la Vallée-Poussin's average converges
to \num{0.57721}, the Euler-Mascheroni constant.

It converges quite slow, however.
If we implement the prime quotient as

\begin{minipage}{\textwidth}
\begin{code}
  pquoz :: Natural -> [Double]
  pquoz n = [d/p | p <- ps]
    where  ps  = map fromIntegral (takeWhile (<n) allprimes)
           d   = fromIntegral n
\end{code}
\end{minipage}

and its average as

\begin{minipage}{\textwidth}
\begin{code}
  pquozavg :: Integer -> Double
  pquozavg n = (sum ds) / (fromIntegral $ length ds)
    where  qs  = pquoz n
           ns  = map (fromIntegral . ceiling) qs
           ds  = [n - q | (n,q) <- zip ns qs]
\end{code}
\end{minipage}

we can experiment with some numbers like

|pquozavg 10 = |$0.3\overline{095238}$\\
|pquozavg 100 = 0.548731|\\
|pquozavg 1000 = 0.5590468|\\
|pquozavg 10000 = 0.5666399|\\
|pquozavg 100000 = 0.5695143|\\
$\dots$

With greater and greater numbers, this value
approaches $\gamma$. Restricting $n$ to prime numbers
produces good approximations of $\gamma$ much earlier. 
From 7 on, |pquozavg| with primes results in numbers of the form
$0.5\dots$ |pquozavg 43 = 0.57416| 
is already very close to $\gamma$.
It may be mentioned that 43 is suspiciously close to 42.

With de la Vallée-Poussin's result in mind,
it is not too surprising that $\gamma$ is related
to divisors and Euler's totient number.
A result of Gauss' immediate successor in Göttingen, 
Peter Gustav Lejeune-Dirichlet (1805 -- 1859),
is related to the average number of divisors
of the numbers $1\dots n$.
We have already defined a function to generate
the divisors of a number $n$, namely |divs|.
Now we map this function on all numbers up to $n$:

\begin{minipage}{\textwidth}
\begin{code}
  divsupn :: Natural -> [[Natural]]
  divsupn n = map divs [1..n]
\end{code}
\end{minipage}

Applied to 10, this function yields:

|[[1],[1,2],[1,3],[1,2,4],[1,5],[1,2,3,6],[1,7],[1,2,4,8],[1,3,9],[1,2,5,10]]|

For modelling Lejeune-Dirichlet's result,
we further need to count the numbers of divisors
of each number:

\begin{minipage}{\textwidth}
\begin{code}
  ndivs :: Integer -> [Int]
  ndivs = map length . divsupn
\end{code}
\end{minipage}

Applied again to 10, |ndivs| produces:

|[1,2,2,3,2,4,2,4,3,4]|

Now we compute the average of this list using

\begin{minipage}{\textwidth}
\begin{code}
  dirichlet :: Integer -> Double
  dirichlet n   = s / l
    where   ds  = ndivs n
            l   = fromIntegral $ length ds
            s   = fromIntegral $ sum ds
\end{code}
\end{minipage}

For |dirichlet 10| we see 2.7.
This does not appear too spectacular.
Greater numbers show:

|dirichlet 100 = 4.759|\\
|dirichlet 250 = 5.684|\\
|dirichlet 500 = 6.38|\\
|dirichelt 1000 = 7.069|

As we can see, the number is slowly increasing
resembling a log function or, more specifically,
the natural log. When we compare the natural log,
we indeed see that the results are close:

$\ln 100  = 4.605$\\
$\ln 250  = 5.521$\\
$\ln 500  = 6.214$\\
$\ln 1000  = 6.907$

For greater and greater numbers,
the difference of 
the |dirichlet| function and 
the natural logarithm approaches

\begin{equation}
0.154435 \approx 2\gamma - 1.
\end{equation}

For the five examples above, the difference
is still significantly away from that number:

$\Delta 100 = 0.2148$\\
$\Delta 250 = 0.1625$\\
$\Delta 500 = 0.1635$\\
$\Delta 1000= 0.1612$,

but already $\Delta 2000 = 0.158$ comes close and
$\Delta 4000 = 0.1572$ 
approaches the value even further.

An important constant derived from $\gamma$ is
$e^{\gamma}$, which is a limit often seen in 
number theory. One instance is the lower bound
of the totient function. There is a clear upper bound,
namely $n-1$. Indeed, $\varphi(n)$ can never 
yield a value greater $n-1$ and this upper bound
is reached exclusively by prime numbers.
There is no such linear lower bound. 
That is, $\varphi(n)$ can assume values that are much smaller
than the value seen for $n-1$ or other numbers less than $n$.
But there is a lower bound that slowly grows with $n$.
This lower bound is often given as $\frac{n}{\ln\ln n}$.
This lower bound, however, is too big.
There are some values that are still below that border.
$\varphi(40)$, for instance, is 16.
$\frac{40}{\ln \ln 40}$, however, is around 30.
A better, even still not perfect approximation, is

\[
\frac{n}{e^{\gamma}\ln \ln n}.
\]

For $n=40$ again,
$\frac{40}{e^{\gamma}\ln \ln 40}$ is around 17 and, hence,
very close to the real value.

We see that $\gamma$ is really a quite mysterious number
that appears in different contexts, sometimes in quite
a subtle manner.
The greatest mystery, however, is that it is not so clear
that this number belongs here in the first place.
Indeed, it has not yet been shown that $\gamma$ is irrational.
In the approximations, we have studied in this section,
we actually have not seen 
the typical techniques to create irrational numbers 
like roots, continuous fractions and infinite series.
If $\gamma$ is indeed rational, then it must be the fraction
of two really large numbers. In 2003, it has been shown
that the denominator of such a fraction must be greater
than $10^{242080}$. 
A number big enough, for my taste, to speak of \speech{irrational}.
