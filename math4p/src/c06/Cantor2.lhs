\ignore{
\begin{code}
module Cantor2
where
  import Data.List (nub,sort)
  import Data.Tree
  import Fact
  import Binom
  import Natural
  import Zahl
  import Quoz
\end{code}
}

So. How many real numbers are there?
As before, we will try to \term{count}
the real numbers by building a set of tuples
$(r,n)$ for all numbers $r\in \mathbb{R}$ and
$n\in\mathbb{N}$. In order to do this,
we need to first establish a sequence 
of real numbers. 
We start by investigating the real numbers
in the range $0\dots 1$.
We construct a sequence of 
real numbers in this interval.
We start by creating some sequence
like the following:

\begin{minipage}{\textwidth}
\[
0.10001\dots
\]
\[
0.01001\dots
\]
\[
0.00101\dots
\]
\[
0.00011\dots
\]
\[
0.00001\dots
\]
\[
\dots
\]
\end{minipage}

We now have an infinite sequence 
of infinite sequences of digits.
Can we enumerate this sequence and assign natural numbers 
to each single real number to count the whole sequence?
There is an issue. For any given sequence, we can easily
construct a new number, not yet in the sequence, but in
the same interval (the numbers $0\dots 1$).
The method is called 
Cantor's (second) \term{diagonal argument}.
It goes like this:
For each number in the sequence,
from the first number take the first digit,
from the second number take the second digit,
from the third number take the third digit
and so on.
Since the numbers in the sequence are distinct,
the new number, constructed in this way,
is different from all other numbers in the sequence.
For instance:

\begin{minipage}{\textwidth}
\[
0.\mathbf{1}0001\dots\\
\]
\[
0.0\mathbf{1}001\dots\\
\]
\[
0.00\mathbf{1}01\dots\\
\]
\[
0.000\mathbf{1}1\dots\\
\]
\[
0.0000\mathbf{1}\dots\\
\]
\[
\dots
\]
\[
0.\mathbf{11111}\dots\\
\]
\end{minipage}

Note that the sequence above was not particularly
designed to hold for this method. As long as the numbers are
irrational, \ie\ each of them consists of an infinite 
non-repeating sequence of digits, we will, 
following the method, always construct a number
that is not yet in the list.

The point is that we can do this with any sequence
of irrational numbers one may come up with.
In consequence, when we construct an enumeration 
of the real numbers, as we did for rational
numbers using the Calkin-Wilf or the Stern-Brocot tree,
there is always a number that we can introduce
between any two numbers in the sequence.
Any possible sequence of the real numbers, hence,
is necessarily incomplete.
We, therefore, arrive at the strange conclusion
that $||\mathbb{R}||\neq||\mathbb{N}||$ or,
in other words, $\mathbb{R}$ is not \term{countable}.

Cantor says that the sets are both infinite,
but they are infinite in different ways:
$\mathbb{N}$ is countably infinite, while
$\mathbb{R}$ is \term{uncountably} infinite.
There are thus different infinite cardinalities.
That of $\mathbb{N}$ is $\aleph_0$;
that of other sets may be $\aleph_1, \aleph_2, \aleph_3,\dots$
leading to a whole new universe of numbers
that express different ways of being infinite.

Cantor conjectured that the cardinality of $\mathbb{R}$
is the cardinality of the \term{powerset} of $\mathbb{N}$,
which is, as you may remember, $2^n$ for a set with $n$
elements. The cardinality of $\mathbb{R}$, would then be
$2^{\aleph_0}$.

This makes a lot of sense, 
when you consider the diagonal method above.
Indeed, when we created the powerset of a given set,
we used binary numbers to encode the presence or absence
of a given element in one of the sets in the powerset.
For the set $S=\lbrace a,b,c\rbrace$, the number $100_2$
would encode the set $\lbrace a\rbrace \in P(S)$.
Now, when you consider that $S$ is infinite,
you have an infinite sequence of such numbers and,
as we did above illustrating the diagonal argument,
we can introduce for any given such sequence
a new number, \ie\ a new element of $P(S)$.

Cantor further conjectured that
$\aleph_1 = 2^{\aleph_0}$.
That would mean that there is no infinite cardinality
``between'' that of $\mathbb{N}$ and that of $\mathbb{R}$.
Cantor's conjecture is very famous under the name
\term{Continuum Hypothesis} (\acronym{ch}).
In the early $20^{th}$ century it was important enough
for Hilbert to include it into his equally famous 23 problems
that he assumed to be the most important math problems
to be solved. It was, in fact, the first of these 23 problems.

The hypothesis as such is still unsolved today.
In 1940, however, Kurt Gödel showed
that \acronym{ch} cannot be disproven based on the standard
axiomatic system, the Zermelo-Fraenkel set theory (\acronym{zf}).
In 1963, again, Paul Cohen showed
that it cannot be proven in \acronym{zf} either.
Mathematicians today say that \acronym{ch} 
is \term{independent} from \acronym{zf}.
This may perhaps be translated into sloppy common speech as
\acronym{ch} is irrelevant, at least in the context
of the standard axiomatic system.

Without out too much phantasy, 
we can go beyond \acronym{ch} and suspect 
that there is a general rule that
for any $n \in \mathbb{N}: \aleph_n=2^{\aleph_{n-1}}$.
$\aleph_1=2^{\aleph_0}$ would then be no exception.
It would just be the way to count in infinity.
This hypothesis is called the
\term{Generalised Continuum Hypothesis} (\acronym{gch}).
The idea is extremely beautiful and, again,
it makes a lot of sense.
We cannot, of course, extend infinity
by just adding an element; that would still be infinity.
Between a set and its powerset, however, there is
a \emph{structural} difference that still holds,
as we have seen, with infinite sets.
There is no one-to-one mapping 
from the set to its powerset or, more formally worded,
``there is
no \term{surjection} from a set to its powerset''.
This is known as \term{Cantor's theorem} and
Cantor's diagonal argument demonstrates that 
the relation between the sets
$\mathbb{N}$ and $\mathbb{R}$
is an instance of this theorem.

The \acronym{gch}, just as the weaker \acronym{ch},
is also independent from \acronym{zf}.
There are, however, some stronger implications
when assuming the truth of \acronym{gch}, as shown, again,
by Kurt Gödel and by Polish mathematician
Wac\l{}aw Sierpiński (1882 -- 1969).
That, however, would lead us deeply 
into mathematical logic, which is not our topic here.
More interesting appears to be the question
what actually causes the difference between
the cardinalities of $\mathbb{N}$ and $\mathbb{R}$.

This question is discussed in Cantor's first article
on set theory, a tremendously important document
in the history of math.
In this short article -- it has less than five pages --
Cantor first proves that the set of real
\term{algebraic} numbers is countable and then
that the set of real numbers is not countable
providing this way a new proof for the existence
of the \term{transcendental} numbers
and demonstrating that it is the transcendental numbers
that make the set of real number uncountable.

To enumerate the algebraic numbers,
Cantor uses a sophisticated trick, that
involves mathematical machinery that is not yet
at our disposal. He uses \term{polynomials} --
those beasts will be a major topic of the next part --
that are \term{irreducible} over $\mathbb{Q}$.
He orders these polynomials and interprets
algebraic numbers as the \term{roots},
\ie\ the solutions, of the polynomials.
The first ten polynomials and their roots are:

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||c||c||}
\hline
Polynomial & Root \\\hline\hline
$x$       & 0  \\\hline
$x+1$     & -1 \\\hline
$x-1$     & 1   \\\hline
$x+2$     & -2  \\\hline
$2x+1$    & $-\frac{1}{2}$ \\\hline
$2x-1$    & $\frac{1}{2}$  \\\hline
$x-2$     &  2             \\\hline
$x+3$     & -3             \\\hline
$x^2+x-1$ & $\frac{-1-\sqrt{5}}{2}$ \\\hline
$x^2-2$   & $-\sqrt{2}$ \\\hline
$\dots$   & $\dots$\\\hline
\end{tabular}
\endgroup
\end{center}

The column ``Root'' contains the components of the enumeration
of the algebraic numbers, which, hence, goes like

\[
0,-1,1,-2,-\frac{1}{2},\frac{1}{2},2,-3,\frac{-1-\sqrt{5}}{2},-\sqrt{2},\dots
\]

The enumeration technique is better than the one
he used to enumerate the rationals, since it contains
each algebraic number only once. Perhaps you remember
that, using Cantor's original technique 
for enumerating the rationals, we had to
filter out duplicates.

We see further that the sequence enumerates an extension
of $\mathbb{Q}$ containing roots of rational numbers and 
more complex formulas including such roots.
Since Cantor used the property of a real number
being algebraic, \ie\ appearing as a solution
of a polynomial with rational coefficients,
he guarantees that the resulting enumeration
contains exactly all algebraic numbers.
He, thus, proves that the cardinality of the set
of algebraic numbers equals $||\mathbb{N}|| = \aleph_0$.
Since, as we have already seen,
the set of real numbers has not the cardinality $\aleph_0$,
this means that it is indeed the transcendental numbers
that make $\mathbb{R}$ uncountable.

\ignore{
first version:
- enumerating the algebraic numbers
https://en.wikipedia.org/wiki/Georg_Cantor%27s_first_set_theory_article
}
