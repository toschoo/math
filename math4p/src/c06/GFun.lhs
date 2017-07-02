\ignore{
\begin{code}
module GFun
where
  import Natural
  import Quoz
  import Real
  import Gamma
\end{code}
}

As we have already done in the domain of integers,
we will now try to generalise some of the combinatorial
sequences to the domain of real numbers.
We did this with integers
for the binomial coefficients.
Before we can introduce real binomials,
however, we need to look at factorials.
The factorial of $n$, $n!$, is the number of possible
permutations of a sequence of $n$ elements.
This is a very concrete and easy to grasp concept.
A function, however, that results in a 
fraction or even an irrational number
does not count anything similar to that.
It may measure a \emph{continuous} quantity 
(a weight or distance, for example), but certainly not
a \emph{discrete} value like a counting result.

This leads to a didactical dilemma 
that often arises in modern mathematics. 
Mathematics usually aims to generalise concepts
and often independent of concrete applications
of this generalised concept.
The applications often follow, sometimes years
or even centuries later.
We all know geometry in the two-dimensional plane.
Mathematicians have generalised the concepts of
two-dimensional geometry to $n$ dimensions.
From a na\"ive perspective looking 
only at immediate applicability,
there is not much sense
in such geometries beyond three or,
with Einstein in mind, four dimensions.
However, the areas of mathematics that study
\term{space} with more than four or even
infinitely many dimensions (such as linear algebra
and complex analysis), actually have many applications in
statistics, engineering, physics and other
rather practical domains.
Furthermore, applications is an important, but not
the only motivation for mathematical investigation.
Mathematics studies its fundamental concepts
(like numbers, sets or the space in which we
exercise geometry) to arrive at general theorems.
So, even when there is no immediate application yet,
for the sake of better understanding of the concepts involved,
mathematicians do not hesitate to ask questions
that appear to be absurd or meaningless to ``ordinary'' people
(whoever those are).

An obvious way to look at real factorials is to represent them
in the Cartesian plane, \ie\ in the coordinate system.
We can sketch the factorials of natural numbers as:

\begin{center}
\begin{tikzpicture}
   \draw [->] (0,0) -- (6,0);
   \draw [->] (0,0) -- (0,6.5);
   \node [teal,font=\small,anchor=north east] (fac) at (0,6.5) {$n!$};
   \node [teal,font=\small,anchor=north east] (fac) at (6,0) {$n$};
   \draw [red,fill=red] (0,0.05) circle (1pt);
   \draw [red,fill=red] (1,0.05) circle (1pt);
   \draw [red,fill=red] (2,0.1) circle (1pt);
   \draw [red,fill=red] (3,0.3) circle (1pt);
   \draw [red,fill=red] (4,1.2) circle (1pt);
   \draw [red,fill=red] (5,6) circle (1pt);
\end{tikzpicture}
\end{center}

where the factorials of $n$ are shown 
on the vertical axis (the $y$-axis)
with a scale of $1:10$ in relation to the values for $n$ 
on the horizontal axis (the $x$-axis).
The diagram shows the factorials 
of the numbers $n\in\lbrace 0, 1, 2, 3, 4, 5\rbrace$,
which are $1, 1, 2, 6, 24, 120$.
The factorials are drawn as dots with the coordinates
$(n,n!)$. The space between these dots is empty.
A continuous interpretation of the factorials would ask for the
values within this white space and aim to find a curve
that connects the discrete dots in the picture.

One obvious requirement for such a function $f$ is
that for any $n\in\mathbb{N}, f(n) = n!$.
A function that (almost) fulfils this requirement
is the \term{Gamma function}, $\Gamma$.
For any integer $n>1$, 
$\Gamma$ is

\begin{equation}
\Gamma(n) = (n-1)\Gamma(n-1)
\end{equation}

with $\Gamma(1)=1$.
We can model this in Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  gamman :: Natural -> Natural
  gamman 0 = undefined
  gamman 1 = 1
  gamman n = (n-1)*gamman (n-1)
\end{code}
\end{minipage}

When we apply this to the numbers $1\dots 10$,
|map gamman [1..10]|,
we see: 

|[1,1,2,6,24,120,720,5040,40320,362880]|

There is a snag. Here are the factorials,
created with |map fac [1..10]|:

|[1,2,6,24,120,720,5040,40320,362880,3628800]|

The $\Gamma$ function, hence, creates the factorials,
but shifted down by one. Indeed, we have

\begin{equation}
\Gamma(n+1) = n!
\end{equation}

With |[gamman (n+1) || n <- [1..10]]|, we finally see
the factorials in their correct places in the sequence:

|[1,2,6,24,120,720,5040,40320,362880,3628800]|

That the $\Gamma$ function is defined like this,
is for historical reasons and does not need to bother
us here. We only have to keep in mind that, whenever
we want to make the connection from $\Gamma$ to factorial,
we need to increment $n$ by 1.

Well, the |gamman| function above
shows us just another way to express
factorials for natural numbers. But we wanted to find
a function for real numbers.
There are indeed many ways to define the $\Gamma$ function
in that domain. The canonical way is 
the \term{Euler integral of the second kind},
but, since we have not yet introduced
integrals, we choose another way
that we already know, namely infinite products.

We first look at the following product, which
was found already by Euler:

\begin{equation}
\Gamma(x) = \frac{1}{x}\prod_{n=1}^{\infty}{
            \frac{(1+\frac{1}{n})^x}{1+\frac{x}{n}}}
\end{equation}

We can reformulate this equation in Haskell as:

\begin{minipage}{\textwidth}
\begin{code}
  gammal :: Natural -> RealN -> RealN
  gammal i x = (1/x)*product [(1+1/n)**x / (1+x/n) | n <- [1..m]]
    where m = fromIntegral i
\end{code}
\end{minipage}

The function receives a |Natural| and a |RealN|.
The |Natural|, $i$, is just the number of iterations
we want to perform, since we do not have the time
to go through all iterations of the infinite product.
The |go| function implements the product itself.
Finally we multiply $\frac{1}{x}$ and we are done.

Let us look at how precise this function can mimic $n!$
for a given number of iterations. 
When we apply |gammal| on $x=1$,
then we obviously get 1, since we compute

\[
\frac{1}{1}\prod_{n=1}^{\infty}{
\frac{(1+\frac{1}{n})^1}{1+\frac{1}{n}}} =
\prod_{n=1}^{\infty}{
\frac{1+\frac{1}{n}}{1+\frac{1}{n}}} =
1\times 1\times 1\times\dots = 1.
\]

Indeed, |gammal 1 1| immediately yields 1.
Next, we try |gammal 1 2| and get 
\[
0.\overline{6}
\]
That is far off the expected value 1.
We increase the number of iterations and try
|gammal 10 2|:

\[
0.91666666\dots
\]

Already better, but still not 1.
We try |gammal 100 2| and see:

\[
0.99019607\dots
\]

and |gammal 1000 2|:

\[
0.99900199\dots
\]

We see, the function converges slowly.
When we try the factorials for $3\dots 6$ 
with \num{1000} iterations, we see:

\[
1.99401993\dots,5.96418512\dots,23.76178831\dots,118.21843985\dots
\]

The first two values are fairly close to the expected results.
$\Gamma(5)$ and $\Gamma(6)$, which should be
24 and 120, however, are clearly off.
We try |gammal 10000 5| and see:

\[
23.97601798\dots
\]

Not good, but much better.
What about |gammal 10000 6|?
Here it is:

\[
119.82018583\dots
\]

Still more than 0.1 off the expected result 120.
We try again with \num{100000} iterations:

\[
119.98200185\dots
\]

That was still not enough! Let us try with
one million iterations:

\[
119.99820001\dots
\]

We see that the infinite product slowly approaches
the expected results of the $\Gamma$ function, but
the stress here is on ``slowly''. Already for $x=6$,
we need a lot of iterations to achieve a deviation
of less than 0.01.

Let us look at another infinite product.
It is not faster than the one we looked at --
on the contrary, it is even slower --
but it is a nice formula:

\begin{equation}
\Gamma(x) = \frac{e^{-\gamma x}}{x}\prod_{n=1}^{\infty}{
\left(1+\frac{x}{n}\right)^{-1}e^{\frac{x}{n}}},
\end{equation}

where $\gamma$ is the Euler-Mascheroni constant
and $e$, the Euler-Napier constant.
This formula was found by German mathematician
Karl Weierstrass (1815 -- 1897) who was instrumental
in the foundations of modern analysis. 
In Haskell, his definition of the $\Gamma$ function 
may look like:

\begin{minipage}{\textwidth}
\begin{code}
  gammae :: Natural -> RealN -> RealN
  gammae i x = f * product [e**(x/n) * (1+(x/n))**(-1) | n <- [1..m]]
    where  f = e**((-1)*gamma*x)/x
           m = fromIntegral (i-1)
\end{code}
\end{minipage}

An important difference to the first formula is
that, from this one, it is not obvious that it
should result in 1 for $x=1$. Let us give it a try.
|gammae 1 1|:

\[
0.56145836\dots
\]

That is an ugly result! Already for 1, it
diverges from the expected result by almost $\frac{1}{2}$!
Let us see how many iterations we need to approach 1:

|gammae 10   1   = 0.95043595...|\\
|gammae 100  1   = 0.99500219...|\\
|gammae 1000 1   = 0.99949804...|

and so on. What about 2?

|gammae 1    2   = 0.15761774...|\\
|gammae 10   2   = 0.82120772...|\\
|gammae 100  2   = 0.98022710...|\\
|gammae 1000 2   = 0.99799833...|

It definitely converges slower than Euler's formula.
For the remainder of this section, we will therefore stick
to Euler's solution.

Let us look at some other numbers,
not positive
integers, for instance:

\begin{minipage}{\textwidth}
\[
\Gamma(0) = \infty,
\]
\[
\Gamma(-1) = -\infty,
\]
\end{minipage}

So, $\Gamma(0)$ and $\Gamma(-1)$ yield $\pm\infty$.
What about $\frac{1}{2}$?
Using |gammal|, we see the following results:

\begin{minipage}{\textwidth}
|gammal    1 0.5 = 1.8856...|\\
|gammal   10 0.5 = 1.7927...|\\
|gammal  100 0.5 = 1.7746...|\\
|gammal 1000 0.5 = 1.7726...|\\
\end{minipage}

It will finally approach: 

\[
\Gamma(0.5) = 1.77267520\dots
\]

Is it possible that $\Gamma(0.5)$ yields such a boring number?
Well, is it such boring? Look what happens (with one million iterations):

|gammal 1000000 0.5 * gammal 1000000 0.5 = 3.14159...|

That is $\pi$! So $\Gamma(0.5) = \sqrt{\pi}$. Not bad!
The occurrence of both, $e$ and $\gamma$, in Weierstrass' formula
already looked somewhat suspicious. It was only a matter of time,
when we would meet $\pi$ in applying the function to some values.
In fact, there are many values for which $\Gamma$ produces
a product of $\sqrt{\pi}$ with some fraction. For instance:

\[
\Gamma\left(\frac{3}{2}\right) = \frac{1}{2}\sqrt{\pi},
\]
\[
\Gamma\left(\frac{5}{2}\right) = \frac{3}{4}\sqrt{\pi},
\]
\[
\Gamma\left(\frac{7}{2}\right) = \frac{15}{8}\sqrt{\pi},
\]
\[
\Gamma\left(\frac{9}{2}\right) = \frac{105}{16}\sqrt{\pi},
\]
\[
\dots
\]

These results suggest a pattern for odd numbers $n$.
Apparently, $\Gamma(\frac{n}{2})$ yields a product of the form
$\frac{k}{2^{(n-1)/2}}\sqrt{\pi}$,
where $k = (n-2)(k_{n-2})$.

Can we say more about the factor $k$?
For the odd numbers $1,3,\dots 11$, $k$ is
1, 3, 15, 105, 945, 10395.
These are the \term{double factorials}, $n!!$, 
for the odd numbers, \ie\
the products of all odd numbers $1,3,..,n$.
We, hence, have for odd numbers $n$:

\begin{equation}
\Gamma\left(\frac{n}{2}\right) = \frac{(n-2)!!}{2^{\frac{n-1}{2}}}\sqrt{\pi}
\end{equation}

which can be implemented in Haskell as

\begin{minipage}{\textwidth}
\begin{code}
  gammaho :: Natural -> RealN
  gammaho 1 = sqrt pi
  gammaho n  | even n     = error "not an odd number!"
             | otherwise  = rff (n-2) / 2**i 
                          * sqrt pi
    where  rff  = fromIntegral . facfac
           i    = fromIntegral ((n-1) `div` 2)
\end{code}
\end{minipage}

The $\Gamma$ function shows many of such suprising properties.
It has been and is still being extensively studied 
and a lot of relations to other functions,
such as the Riemann zeta function, have been found.

But let us now go on to the definition of real binomial coefficients
using the $\Gamma$ function.
To this end, we define a new |choose| function, namely:

\begin{minipage}{\textwidth}
\begin{code}
  chooser :: Natural -> RealN -> RealN -> RealN
  chooser i n k =  gammal i (n+1) / 
                   (gammal i (k+1) * gammal i (n-k+1))
\end{code}
\end{minipage}

Again, we try this function on integers.
For instance:

\begin{minipage}{\textwidth}
|choose 2 1 = 2|\\
|choose 3 1 = 3|\\
|choose 3 2 = 3|\\
|choose 5 2 = 10|\\
|choose 5 3 = 10|\\
|choose 7 2 = 21|\\
|choose 7 3 = 35|
\end{minipage}

We start with $\binom{2}{1}=2$, using |chooser|

|chooser 1 2 1 = 1.5|

Far off. So, again, we increase the number of iterations:

\begin{minipage}{\textwidth}
|chooser     1 2 1 = 1.5|\\
|chooser    10 2 1 = 1.8461...|\\
|chooser   100 2 1 = 1.9805...|\\
|chooser  1000 2 1 = 1.9980...|\\
|chooser 10000 2 1 = 1.9998...|
\end{minipage}

After \num{10000} iterations, we come pretty close.
Let us try the other examples with \num{10000} iterations:

\begin{minipage}{\textwidth}
|chooser 10000 3 1 = 2.9994...|\\
|chooser 10000 3 2 = 2.9994...|\\
|chooser 10000 5 2 = 9.9940...|\\
|chooser 10000 5 3 = 9.9940...|\\
|chooser 10000 7 2 = 10.9790...|\\
|chooser 10000 7 3 = 34.9580...|
\end{minipage}

which is fairly close for all these numbers.

The resulting function has been little studied.
It is known that many of the binomial identities
fail for real numbers. The behaviour for different
values of $n$ and $k$ not integers is very complex.
We will not go into details here. But we will certainly
come back to the $\Gamma$-function and its applications later on.


\ignore {
- History of the Gamma function
https://en.wikipedia.org/wiki/Binomial_coefficient#Two_real_or_complex_valued_arguments
Check numerical results against RealN!!!
}
