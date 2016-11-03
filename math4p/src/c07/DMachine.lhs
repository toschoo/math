\ignore{
\begin{code}
module PolyArith
where
  import Natural
  import Zahl
  import Quoz
  import Real
  import NumSystem
  import PolyArith
\end{code}
}

Polynomial arithmetic, as we have seen,
is very similar to number arithmetic.
What is the correspondent of interpreting
a number in a given numeral system
in the domain of polynomials?
Well, that is the \term{application} of the polynomial
to a given number. We would substitute $x$
for a number in the Field in which we are working
and just compute the formula.
For instance, the polynomial

\[
x^2 + x + 1
\]

can be applied to a rational number, say, 2.
Then we get the formula

\[
2^2 + 2 + 1,
\]

which is $4 + 2 + 1 = 7$.

For other values of $x$, it would of course
generate other values. For $x=0$, for instance,
it would give $0^2 + 0 + 1 = 1$; for $x=1$,
it give $1^2 + 1 + 1 = 3$; for $x=3$, it yields
$3^2 + 3 + 1 = 13$.

How do we need to do to apply a polynomial represented
by our Haskell type? Wee need to go through the list
of coefficients, raise $x$ to the power of the weight
of each particular coefficient, multiply it by the coefficient
and, finally, add all the values together.
Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  apply :: Num a    => Poly a -> a -> a
  apply (P []) _    = 0
  apply (P as) x    = go x $ zip [0..] as
    where  go _ []  = 0
           go z ((i,c):cs) = c*z^i + go z cs
\end{code}
\end{minipage}

First, we weigh the coefficients by zipping them
with the list of integers starting from 0 and then
we apply |go|. 
We define the base case of |go|,
as the one where the coefficients are exhausted. 
Otherwise, we raise $z$ (that is the number 
to which we apply the polynomial) to the current
weight and multiply the coefficient.
We continue by adding this result to the result
of apply |go| to the remainder of the list.
Let us try with a very simple polynomial, $x + 1$:

\begin{minipage}{\textwidth}
|apply (P [1,1]) 0| gives 1.\\
|apply (P [1,1]) 1| gives 2.\\
|apply (P [1,1]) 2| gives 3.\\
|apply (P [1,1]) 3| gives 4.
\end{minipage}

This polynomial, apparently, just counts the integers
adding one to the value to which we apply it.
It implements {\texttt i++}.

On the first sight, this result appears to be boring.
However, after a quick thought, there is a lesson to learn:
we learn something about the polynomial, when we look
at the \term{sequence} it produces when applied
to a sequence of numbers. So, let us implement
a function that maps |apply| to lists of numbers:

\begin{minipage}{\textwidth}
\begin{code}
  mapply :: Num a    => Poly a -> [a] -> [a]
  mapply p = map (apply p) 
\end{code}
\end{minipage}

For simple polynomials, the sequences are predictable.
$x^2$, obviously, just produces the squares;
$x^3$ produces the cubes and so on.
Sequences created by powers of the simple polynomial $x+1$,
however, are not so predictable anymore (or are they?), \eg\:

\begin{minipage}{\textwidth}
|mapply (P [1,2,1]) [0..10]|: 1,4,9,16,25,36,49,64,81,100,121\\
|mapply (P [1,3,3,1]) [0..10]|: 1,8,27,64,125,216,343,512,729,1000,1331\\
|mapply (P [1,4,6,4,1]) [0..10]|: 1,16,81,256,625,1296,2401,4096,6561,10000,14641\\
|mapply (P [1,5,10,10,5,1]) [0..10]|:\\
1,32,243,1024,3125,7776,16807,32768,59049,100000,161051\\
\end{minipage}

The first, easy to recognise, are the squares, but pushed one up,
\ie\ the application to 0 yields the value for $1^2$, 
the application to 1 yields the value for $2^2$ and so on.
The second, still easy to recognise,
are the cubes -- again pushed up by one.
The third are the powers of four and the fourth are the powers of five.

That is not too surprising at the end, since
|P [1,2,1]| is the result of squaring |P [1,1]|, 
which generates the integers pushed one up;
|P [1,3,3,1]| is the result of raising |P [1,1]| to the third power
and so on.

Things become more interesting, when we deviate
from the binomial coefficients. The sequence
produced by |mappy (P [1,2,3,4]) [1..10]|, for instance,
does not resemble such a simple sequence:
1, 10, 49, 142, 313, 586, 985, 1534, 2257, 3178, 4321.
Even the Online Encyclopedia has nothing interesting
to say about it.
The same is true for |mappy (P [5,6,7,8]) [1..10]|,
which is 5, 26, 109, 302, 653, 1210, 2021, 3134, 4597, 6458, 8765.

This raises another interesting question:
given a sequence, is there a method by which 
we can we recognise the polynomial that created it?
Yes, there is. In fact, there are.
There was even a machine that helped in guessing
polynomials from sequences. It was built in the early
$19^{th}$ century by Charles Babbage (1791 -- 1871),
an English polymath, mathematician, philosopher,
economist, and inventor.

Babbage stands in the tradition of designers and constructors
of early computing machinery; predecessors of his
in this tradition were, for instance, 
Blaise Pascal (1623 -- 1662) and
Gottfried Wilhelm Leibniz (1646 -- 1716).
Babbage designed two series of machines,
first, the difference engines and, later, 
the analytical engine.

The analytical engine, unfortunately, was not built in his lifetime.
Only more than a century after the final collapse of the project
in 1878 due to lack of finance, the machine was built 
by science historians and,
this way, it was proved that it was actually possible
to build such a machine with the technology available
in Babbage's time.
The analytical engine would have been 
a universal (Turing-complete) computer
very similar to our computers today,
but not working on electricity, but on steam.
It would have been programmed by punch cards that,
in Babbage's time, were used for controlling looms.
Programs would have resembled modern assembly languages
allowing control structures like selection and iteration.
In the context of a description of the analytical engine,
Ada Lovelace, a friend of Babbage and daughter of Lord Byron,
described how to compute Bernoulli numbers with the machine.
She is, therefore, considered the first computer programmer
in history.

The difference engine, at which we will look here,
is much simpler. It was designed to analyse polynomials
and what it did was, according to Babbage, ``computing differences''.



\ignore{
- Charles Babbage
- compute differences
- compute degree
- predict next
- derivative
- guess the polynomial
}

