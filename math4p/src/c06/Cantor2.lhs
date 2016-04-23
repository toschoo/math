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
between 0:

\[
0.00000\dots
\]

and 1:

\[
1.00000\dots
\]

Now, we construct a random sequence of 
real numbers in this interval:

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

That is, we now have an infinite sequence 
of infinite sequences of digits.
Can we enumerate this sequence and assign natural numbers 
to each single real number to count the whole sequence?
There is an issue. For any given sequence, we can easily
construct a new number, not yet in the sequence, but in
the same interval (the numbers $0\dots 1$ in our example).
The method to do this is called 
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

\ignore{
first version:
- enumerating the algebraic numbers
- this establishes an existing proof for transcendental numbers
- Continuum Hypothesis
}
