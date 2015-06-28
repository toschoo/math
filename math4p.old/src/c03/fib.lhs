\ignore{
\begin{code}
  import Prelude hiding (gcd)
  import Natural hiding (gcd)
\end{code}
}

We have already discussed 
and analysed the run time behaviour of $GCD$.
Let us look at an intriguing example,
the $GCD$ of, say, $89$ and $55$.
As a reminder here the definition 
of $GCD$ once again:

\begin{code}
  gcd :: Natural -> Natural -> Natural
  gcd a 0  =  a
  gcd a b  =  gcd  b (a `rem` b)
\end{code}

We start with \haskell{gcd 89 55}, which results in 
\haskell{gcd 55 (89 `rem` 55)} after one step.
What is the remainder of 89 and 55?
89 divided by 55 is 1 leaving the remainder 
$89 - 55 = 34$.
The next round, hence, is \haskell{gcd 55 34}.
The remainder of 55 and 34 is $55 - 34 = 21$.
We recurse once again, this time with \haskell{gcd 34 21}.
The remainder of 34 and 21 is $34 - 21 = 13$.
The next step, hence, is \haskell{gcd 21 13},
which leads to the remainder $21 - 13 = 8$.
As you see, this gets quite boring, 
but we are not done yet,
since the next round \haskell{gcd 13 8}
forces us to call the function again with 8 and $13 - 8 = 5$,
which then leads to \haskell{gcd 5 3}, subsequently to
\haskell{gcd 3 2} and then to \haskell{gcd 2 1}.
The division of 2 by 1 is 2 leaving no remainder
and, finally, we call \haskell{gcd 1 0},
which reduces immediately to 1.

Apparently, we got in some kind of trap.
The first pair of numbers, 89 and 55,
leads to a sequence of numbers, where 
every number is the sum of its two predecessors:
$1 + 2 = 3, 2 + 3 = 5, 3 + 5 = 8, 5 + 8 = 13,
 8 + 13 = 21, 13 + 21 = 34, 21 + 34 = 55, 34 + 55 = 89$.
We entered with 89 and 55,
computed the remainder and, since the remainder
of two numbers with a distance among them that is less
than the lower number is just the difference 
of the two numbers,
we got to the next pair, 55 and 34,
and continued step for step 
until we finally reached $(2,1)$.

This sequence is well known.
It was used by the Italian mathematician
Leonardo Pisano, better known as Fibonacci
(\term{Filius}, that is, son of Bonaccio),
as an arithmetic exercise in his \term{Abacus}
(``calculating") book, which was published in 1202.
The sequence is the solution to an exercise
with the following wording:
``How many pairs of rabbits can be produced
  from a single pair in a year's time
  if every fertile pair produces a new pair
  of offsping per month and every pair
  becomes fertile in the age of one month?"
% check for a canonical solution
We start with 1 pair, which produces an offspring
after one month, yielding 2 pairs; in the second month,
the first pair produces one more offsping,
hence we have 3 pairs. In the third month,
we have 2 fertile pairs producing each 1 more pair,
we, hence have 5 pairs. 
This, quickly, becomes confusing.
Here a table that gives an overview of what happens
during the first year:

\begin{center}
\begin{tabular}{l||r||r||r||r||r||r||r||r||r||r||r||r}
month     & 1 & 2 & 3 & 4 &  5 &  6 &   7 &   8 &  9 &  10 &  11 &   12 \\\hline
new pairs & 1 & 1 & 2 & 3 &  5 &  8 &  13 &  21 & 34 &  55 &  89 &  144 \\\hline
total     & 1 & 2 & 4 & 7 & 12 & 20 &  33 &  54 & 88 & 143 & 232 &  376 
\end{tabular}
\end{center}

This means that,
in month 1, there is 1 new pair;
in month 2, there another new pair;
in month 3, there are 2 new pairs;
in month 4, there are 3 new pairs;
in month 5, there are 5 new pairs;
$\dots$;
in month 12, there are 144 new pairs.
This is the Fibonacci sequence,
whose first 12 values are given in the second row. 
The answer to Fibonacci's question
consists in summing the sequence up.
The results are given in the third row.
We should remove one from the final result 376,
since this number includes the first pair
that was already there.

If we define the Fibonacci function as

\[
F_n = \begin{cases}
        0 & \textrm{if n = 0}\\
        1 & \textrm{if n = 1}\\
        F_{n-1} + F_{n-2} & \textrm{otherwise}
      \end{cases}
\]

we can define a series
$\sum_{k=2}^{12}{F_k}$,
which, for the rabbit example, gives 375.

The Fibonacci sequence
is explicitly defined for 0 and 1
(since, of course, 0 and 1 do not have
two predecessor from which they could be derived)
and for all other cases recursively as
the sum of its two predecessors.
In Haskell this looks like:

\begin{code}
  fib :: Natural -> Natural
  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib (n-2)
\end{code}

Many people have studied the Fibonacci sequence,
following Fibonacci,
but also independently 
and even before it was mentioned in the \term{Abacus Book}.
The sequence has the astonishing habit
of popping up in very different contexts
in the study of mathematics, nature, arts and music.
Many mathematical objects, however, have
this surprising -- or, for some at least, annoying --
habit.

The first known practical application of the Fibonacci sequence
in Europe appeared in an article of the French mathematician
Gabriel Lamé in 1844 that identified the worst case
of the Euclidian $GCD$ algorithm as any two subsequent numbers
of the Fibonacci sequence.
This remarkable paper is also considered as the earliest
study in computational complexity theory,
a discipline that would be established only 120 years later.\footnote{
There are of course always predecessors.
The relation between $GCD$ and the Fibonacci sequence
was, according to Knuth, already discussed in a paper 
by a French mathematician called Léger in 1837,
and an analysis of the run time behaviour 
of the Euclidan algorithm
was already presented in a paper by another French mathematician
called Reynaud in 1811.}
Lamé gave the worst case of the algorithm as $n$
for $GCD(F_{n+2}, F_{n+1})$.
Let us check if this is true for our example above.
We started with 89 and 55, which correspond
to $F_{11}$ and $F_{10}$.
According to Lamé, we would then need 9 steps
to terminate the algorithm.
The pairs of numbers we applied are:
$(89,55),(55,34),(34,21),(21,13),(13,8),$
$(8,5),(5,3),(3,2),(2,1),(1,0)$,
which are 10 pairs of numbers and indeed 9 steps.
But why is this so and is it always true
or just for the sequence, we happen to look at?

We can answer the first question by observing
the recursive nature of the Euclidian algorithm.
When there is a pair of subsequent Fibonacci numbers
that needs $n$ steps, then the pair of the next
subsequent Fibonacci numbers 
will reduce to the first pair after one round of $GCD$
and, thus, needs $n + 1$ steps:
all the steps of the first pair plus the one for itself. 
This is just the structure of mathematical induction,
which leads us to the proof.
We choose $(2,1)$ as the base case,
which is $(F_3,F_2)$ and,
as we have seen,
needs one step to result 
in the trivial case $(1,0)$.
If the proposition that $GCD(F_{n+2}, F_{n+1})$ 
needs $n$ steps is true, then,
for the case $n = 1$,
$F_{n+2}$ is $F_3$, which is 2,
and $F_{n+1}$ is $F_2$, which is 1.
Therefore, the base case $(2,1)$ fulfils the rule.

Now assume that we have a case for which it is true
that the number of steps of $GCD ( F_{n+2}, F_{n+1})$ is $n$.
Then we have to show that 
the number of steps of 
$GCD ( F_{n+3}, \\ F_{n+2})$ is $n + 1$.
According to its definition, 
$GCD$ for a pair of numbers $(a,b)$ is $(b, a \bmod  b)$.
For subsequent Fibonacci numbers 
(as we have already shown informally above),
$a \bmod b$ is identical to $a - b$ 
(except for the case where $b = 1$).
After one step 
with $a=F_{n+3}$ and $b=F_{n+2}$, 
we therefore have:

\[
GCD(F_{n+2}, F_{n+3} - F_{n+2}).
\]

We can substitute $F_{n+3}$ in this formula
according to the definition of the Fibonacci sequence,
$F_{n} = F_{n-2} + F_{n-1}$,
by $F_{n+1} + F_{n+2}$:

\[
GCD(F_{n+2}, F_{n+1} + F_{n+2} - F_{n+2}),
\]

which, of course, simplifies to

\[
GCD(F_{n+2}, F_{n+1}).
\]

This shows that we can reduce
$GCD(F_{n+3}, F_{n+2})$ 
to 
$GCD(F_{n+2}, F_{n+1})$
in one step
and this concludes the proof.

There is much more to say about 
this delightful sequence,
and we are even far away from the conclusions
of Lamé's paper.
Unfortunately, we have not yet
acquired the tools to talk
about these upcoming issues
in a meaningful way.
But, very soon, we will have.
In the meanwhile,
you might try to discover the Fibonacci sequence
in other objects we will meet on our way,
for example, a certain very strange triangle.

\ignore{
proof: fib (gcd a b) = gcd (fib a) (fib b)
example: 
gcd 10 5 = 5
fib 5 = 5

gcd (fib 10 = 55) (fib 5 = 5) = 5

see http://www.cut-the-knot.org/arithmetic/algebra/FibonacciGCD.shtml
}

\ignore{
The German mathematician, astronomer and astrologer
Johannes Kepler
has realised that the quotient of two consecutive
Fibonacci numbers aproximates the \emph{golden ratio}
for sufficiently big numbers.
The golden ratio is very prominent
in aesthetic theory and practice
at least since the Renaissance.
Renaissance and humanistic scholars
date the use of the golden ratio
back to the antiquity
and, indeed, Euclide gives a description
of this ratio in the \term{Elements}.
Whether antique architecture and art
actually used the golden ratio is
contested among modern scholars.
However, the golden ratio describes a relation 
of two values $a$ and $b$ such that

\begin{equation}
\frac{a}{b} = \frac{a + b}{a}.
\end{equation}

In other words the greater of the two values,
$a$, has the same ratio to the smaller value, $b$,
as the sum of the two to $a$. 
The relation corresponds to

\[
\frac{1 + \sqrt{5}}{2} \approx 1.618.
\]

The golden ratio, indeed, looks very similar to the quotient
formed from two consecutive Fibonacci numbers:

\begin{equation}
\frac{F_{n}}{F_{n-1}} = \frac{F_{n-1} + F_{n-2}}{F_{n-1}}.
\end{equation}

From Kepler's observation, 
a closed form for the Fibonacci sequence can be deviced,
which is desirable because
Fibonacci numbers are actually helpful in the analysis
of algorithms (as we have just seen for $GCD$)
and the recursive computation of Fibonacci numbers
is quite expensive, as you may have experienced
if you have tried to to generate Fibonacci numbers
beyond $20$ with the \haskell{fib} implementation
given above.
I would very much like to present this closed form here,
but since its derivation requires some more math
than we have learnt so far,
I have to put you off for a future chapter.
This, of course,
is a somewhat anticlimactic finish to the current section.
On the other hand, with this postponement,
the very delightful Fibonacci sequence 
remains vivid in this textbook 
and we have something to look forward to. 
}

\ignore{
  - Kepler + golden ratio
  - closed form
  - yes, there is and it is based on Keplers observation.
    but there is more math in it than we can represent
    right now. we will come back to it later
}
