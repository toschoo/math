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
  \frac{1}{2} + \dots
  \frac{1}{k} 
\end{equation}

This is easily implemented in Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  harmonic :: Natural -> Double
  harmonic n = go 1
    where go i  | i == fromIntegral n  = 1/i
                | otherwise            = 1/i + go (i+1)
\end{code}
\end{minipage}

Some harmonic numbers are |map harmonic [1..10]|:

$1, 1.5, 1.8\overline{3}, 2.08\overline{3}, 2.28\overline{3}, 
 2.45, 2.5928,2.7178, 2.8289, 2.9289.$

The harmonic series is an interesting object of study
in its own right. Here, however, we are interested in 
something else. Namely, the difference of the harmonic series
and the natural logarithm, which converges to

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
|map harmonatural [1..10]|,
we do not see special values: 

$1.0, 0.8068,0.7347,0.697,0.6738,0.6582,0.6469,0.6384,0.6317,0.6263,\dots$

Applied to greater numbers, the results approach a constant value:

|harmonatural    100 = 0.58220|\\
|harmonatural   1000 = 0.57771|\\
|harmonatural  10000 = 0.57726|\\
|harmonatural 100000 = 0.57722|

With even greater numbers, the difference converges to \num{0.57721}.
This number, $\gamma$, was first mentioned by -- of course -- 
Euler and some years later 
by Italian mathematician Lorenzo Mascheroni (1750 -- 1800) and
is therefore called the Euler-Mascheroni constant.

This mysterious number appears in different context and, apparently,
quite often as a difference or an average.
An ingenious investigation was carried out by the Belgian mathematician
Charles Jean de la Vallée-Poussin (1866 -- 1962) who is famous
for his proof of the Prime number theorem.
Vallée-Poussin investigate the quotients of a number $n$ 
and the primes up to that number. If $n$ is not prime itself,
then there are some prime numbers, 
namely those of the prime factorisation of $n$,
that result in an integeger, when $n$ is divided by them.
For others, the quotient is a rational number $\frac{n}{p}$.
Such a number falls short of the next natural number.
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
  pquoz n =  let  ps = map fromIntegral (takeWhile (<n) allprimes)
                  d  = fromIntegral n
             in   [d/p | p <- ps]
\end{code}
\end{minipage}

and its average as

\begin{minipage}{\textwidth}
\begin{code}
  pquozavg :: Integer -> Double
  pquozavg n =  let  qs = pquoz n
                     ns = map (fromIntegral . ceiling) qs
                     ds = [n - q | (n,q) <- zip ns qs]
                in   (sum ds) / (fromIntegral $ length ds)
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
approaches $\gamma$. There are some smaller numbers,
however, that already produce a good approximation.
For instance, the number 43 (which, as you may notice,
is one off 42):

|pquozavg 43 = 0.574161|.







\ignore{
- a mysterious constant
- number n divided by the sum of primes < n
- totient: number of divisors 
- it is not known if gamma is irrational...
}

