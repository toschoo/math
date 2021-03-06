\ignore{
\begin{code}
module E
where
  import Natural
  import Fact
  import Quoz
  import Zahl
\end{code}
}

The Bernoullis were a family of Huguenots from Antwerp
in the Spanish Netherlands from where they fled the repression
by the Catholic Spanish authorities, first to Frankfurt am Main,
later to Basel in Switzerland. Among the Bernoullis,
there is a remarkable number of famous mathematicians
who worked in calculus,
probability theory, number theory and many areas 
of applied mathematics. One of the Basel Bernoullis was
Johann Bernoulli (1667 -- 1748) who worked mainly
in calculus and tutored famous mathematicians like 
Guillaume L'Hôpital, but whose greatest contribution
to the history of math was perhaps to recognise 
the talent of another of his pupils, one Leonhard Euler. 

His brother Jacob Bernoulli (1655 -- 1705),
who worked, as his brother, in calculus, but
most prominently in probability theory, 
is much better known today, partly perhaps
because many of Johann's achievements 
were published under the name of L'Hôpital.
Unfortunately, early modern mathematics 
and science in general was
plagued with disputes over priorities in the
authorship of contributions, a calamity
that authors and authorities later tried to
solve by introducing the \term{droite d'auteur},
better known in the English speaking world as
\term{copyright}.

Among the many problems Jacob studied was
the calculation of interests. He started off
with a very simple problem. Suppose we have
a certain amount of money and a certain interest
credited after a given amount of time. To keep it
simple, let the amount equal 1 (of any currency
of your liking -- currencies in Jacob's lifetime
were extremely complicated, so we better ignore
that detail). After one year $100\%$ interest is paid.
After that year, we hence have $1+\frac{1\times 100}{100} = 2$ 
in our account. That is trivial.
But what, if the interest is paid in shorter periods
during the year?
For instance, if the interest is paid twice a year,
then the interest for that period would be $50\%$.
After six months we would have $1+\frac{1\times 50}{100} = 1.5$
in our account. After one year, the account would then be
$1.5 + \frac{1.5\times 50}{100} = 1.5 + \frac{75}{100} = 1.5 + 0.75 = 2.25$.

Another way to see this is that the initial value 
is multiplied by 1.5 (the initial value plus the interest) twice:
$1 \times 1.5 \times 1.5 = 1 \times 1.5^2 = 2.25$.
When we reduce the period even further, say, to three months,
then we had $1.25^4 \approx 2.4414$. On a monthly base,
we would get $\left(1+\frac{1}{12}\right)^{12} \approx 2.613$.
On a daily basis, we would have 
$\left(1+\frac{1}{365}\right)^{365} \approx 2.7145$.
With hourly interests and the assumption
that one year has $24 \times 365 = 8760$ hours, 
we would get $\left(1+\frac{1}{8760}\right)^{8760} \approx 2.71812$.
With interest paid per minute we would get
$\left(1+\frac{1}{525600}\right)^{525600} \approx 2.71827$ and
on interest paid per second, we would get
$\left(1+\frac{1}{3156000}\right)^{3156000} \approx 2.71828$.
In general, for interest on period $n$, we get:

\[
\left(1+\frac{1}{n}\right)^n.
\] 

You may have noticed in the examples above
that this formula converges with greater and greater $n$s.
For $n$ approaching $\infty$, it converges
to $2.71828$, a number that is so beautiful that we should
look at more than just the first 5 digits:

\begin{center}
2.7 1828 1828 4590 4523$\dots$
\end{center}

This is $e$.
It is called Euler's number or, 
for the first written appearance 
of concepts related to it in 1618,
Napier's number.
It is a pity that its first mentioning
was not in the year 1828.
But who knows -- perhaps in some rare
Maya calendar the year 1618 
actually is the year 1828.

An alternative way to approach $e$
that converges much faster than the closed form above
is the following:

\[
1+\frac{1}{2}+\frac{1}{6}+\frac{1}{24}+\frac{1}{120}+\dots
\]

or, in other words:

\begin{equation}
e = \sum_{n=1}^{\infty}{\frac{1}{n!}}.
\end{equation}

We can implement this equation in Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  e_ :: Integer -> Double
  e_ p = 1 + sum [1/(dfac n) | n <- [1..p]]
    where dfac = fromInteger . fac
\end{code}
\end{minipage}

After some experiments with this function,
we see that
it converges already after 17 recursions
to a value that does not change with greater
arguments at |Double| precision, such that 
|e_ 17 == e_ 18 == e_ 19 ==| $\dots$
We could then implement |e| as

\begin{minipage}{\textwidth}
\begin{code}
  e :: Double
  e = e_ 17
\end{code}
\end{minipage}

The fact that $e$ is related to the factorial
may lead to the suspicion that it also appears
directly in a formula dealing with factorials. 
There, indeed, is a formula derived by James Stirling
who we already know for the Stirling numbers.
This formula approximates the value
of $n!$ without the need to go through all
the steps of its recursive definition.
Stirling's formula is as follows:

\begin{equation}
n! \approx \sqrt{2\pi n}\left(\frac{n}{e}\right)^n.
\end{equation}

This equation is nice already because of the fact
that $e$ and $\pi$ appear together to compute
the result of an important function.
But how precise is the approximation?
To answer this question, we first implement
Stirling's formula:

\begin{minipage}{\textwidth}
\begin{code}
  stirfac :: Integer -> Integer
  stirfac i = ceiling $ (sqrt (2*pi*n)) * (n/e)^i
    where n = fromIntegral i
\end{code}
\end{minipage}

Note that we \emph{ceil} the value, instead of
rounding it just to the next integer value.

Then we define a function to compute the difference
|difac n = fac n - stirfac n|.
The result for the first 15 numbers is

\[
0,0,0,0,1,9,59,417,3343,30104,301174,3314113,39781324,517289459,7243645800.
\]

For the first numbers, the difference is 0. Indeed:

\begin{align*}
1! && = && stirfac(1) && = && 1\\
2! && = && stirfac(2) && = && 2\\
3! && = && stirfac(3) && = && 6\\
4! && = && stirfac(4) && = && 24
\end{align*}

Then, the functions start to disagree,
for instance $5! = 120 \neq stirfac(5) = 119$.
The difference grows rapidly and reaches more
than 3 million with $12!$. But what is the deviation
in relation to the real value?
We define the function 
|100 * (fromIntegral $ difac n) / (fromIntegral $ fac n)|
to obtain the difference in terms of a percentage
of the real value. We see starting from 5
(where the first difference occurs):

\[
0.8333,
1.25,
1.1706,
1.0342,
0.9212,
0.8295,
0.7545,
0.6918,
0.6388,
0.5933,
0.5539,\dots
\]

For 5, the value jumps up from 0 to $0.8333\%$,
climbs even higher to $1.25\%$ and then starts
to descrease slowly.
At 42 the deviation falls below $0.2\%$.
At 84, it falls below $0.1\%$ and keeps falling.
Even though the difference appears big
in terms of absolute numbers, the percentage
quickly shrinks and, for some problems, may
even be neglible.

A completely different way to approximate $e$
is by \term{continued fractions}.
Continued fractions are infinite fractions,
where each denominator is again a fraction.
For instance:

\begin{equation}
  e =  1 + \frac{1}{
         1 + \frac{1}{
           2 + \frac{1}{
             1 + \frac{1}{
               1 + \frac{1}{
                 4 + \frac{1}{\dots}}}}}}
\end{equation}

A more readable representation of continued fractions
is by sequences of the denominator like:

\begin{equation}
e = [2;1,2,1,1,4,1,1,6,1,1,8,1,1,\dots]
\end{equation}

where the first number is separated by a semicolon
to highlight the fact that it is not a denominator,
but an integral number added to the fraction that follows.
We can capture this very nicely in Haskell,
using just a list of integers.
However, in some cases we might have a fraction
with numerators other than 1.
An elegant way to represent this case is by
using fractions instead of integers.
We would then represent $\frac{2}{a + \dots}$
as $\frac{1}{\frac{1}{2}a + \dots}$.
Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  contfrac :: [Quoz] -> Double
  contfrac []  = 1
  contfrac [i] = fromQuoz i 
  contfrac (i:is) = n + 1 / (contfrac is)
    where n = fromQuoz i

  fromQuoz :: Quoz -> Double
  fromQuoz i = case i of
                (Pos (Q nu d)) -> fromIntegral nu / fromIntegral d
                (Neg (Q nu d)) -> negate (fromIntegral nu / fromIntegral d)
\end{code}
\end{minipage}

For |contfrac [2,1,2,1,1,4,1,1,6]| we get 2.7183,
which is not bad, but not yet too close to $e$. 
With |[2,1,2,1,1,4,1,1,6,1,1,8,1,1,10]| we get \num{2.718281828},
which is pretty close.

Examining the sequence a bit further, we see
that is has a regular structure.
We can generate it by means of the \term{Engel expansion}
named for Friedrich Engel (1861 -- 1941), 
a German mathematician who worked close with the great
Norwegian algebraist Sophus Lie (1842 -- 1899).
The Engel expansion can be implemented as follows:

\begin{minipage}{\textwidth}
\begin{code}
  engelexp :: [Integer]
  engelexp = 2:1:go 1
    where go n = (2*n):1:1:go(n+1)
\end{code}
\end{minipage}

The following fraction, however, conoverges
must faster than the Engel expansion:
$[1,\frac{1}{2},12,5,28,9,44,13]$.
Note that we take advantage of the datatype |Quoz|
to represent a numerator that is not 1.
This sequence can be generated by means of 

\begin{minipage}{\textwidth}
\begin{code}
  fastexp :: [Quoz]
  fastexp = 1:(Pos (1%2)):go 1
    where go n = (16*n-4):(4*n+1):go (n+1)
\end{code}
\end{minipage}

This fraction converges already after 7 steps
to the value \num{2.718281828}: 

|contfrac (take 7 fastexp)|. 

The area of mathematics where $e$ is really at home
is analysis and its vast areas of application, 
which we will study in the third part of this series.
The reason for the prominence of $e$ in analysis
stems from the \term{natural logarithm},
which we already introduced in the first chapter.
The natural logarithm of a number $n$, 
usually denoted $\ln(n)$,
is the exponent $x$, such that $e^x = n$.

The natural logarithm can be graphed as follows:

\begin{center}
\begin{tikzpicture}[trim axis left]
\begin{axis}[
  step=1,
  domain=0:10,
  samples=100,
  % enlarge x limits=true,
  ymin=-2,ymax=5,
  xmin=0.01,xmax=10,xstep=1,
  grid=both,
  %axis equal,
  no markers]
\addplot +[thick] {ln(x)};
\addplot [black] {0};
\end{axis}
\end{tikzpicture}
\end{center}

The curious fact that earned the natural logarithm its name
is that, at $x=1$, the curve has the slope 1.
This might sound strange for the moment.
We will investigate that later.


