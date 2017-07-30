\ignore{
\begin{code}
module Arigeo
where
  import Natural
\end{code}
}

In the previous section, we found the \emph{Little Gauss},
named according to an anecdote about young Gauss where he
solved the stupid task of adding all number from
$1\dots 100$ with which the teacher intended to get
rid of his class for an hour or so in a couple of
minutes. Our proof technique, induction, was quite good.
But how did we find the hypotheses to be proved
in the first place? Well, we just looked at the numbers
until something sprang into our mind.
And if nothing ever springs into our mind by just looking
at the numbers? Well, then there are other techniques...

The series, we wanted to solve, was $1+2+3+\dots+n$.
We could start by finding alternative representations
of this series, for instance:

\begin{equation}
S_n = 1 + (1+1) + (1+2) + \dots + (1 + n-2) + (1+n-1),
\end{equation}

which we can state a bit different to eliminate the concrete
numbers in the first half of the formula, here form right to left:

\begin{equation}
2S_n = (1 + (n-1)) + (1 + (n-2)) + \dots + 1 + (n-(n-1)) + 1.
\end{equation}

When we plug in a number for $n$, say, 5, we get:

\[
1+(5-1) + 1 + (5-2) + 1 + (5-3) + 1 + (5-4) + 1,
\]

which is $1+4+1+3+1+2+1+1+1$ and, hence, equivalent to
$5+4+3+2+1$.

But this is not the only way. We could turn it around
and write:

\begin{equation}
S_n = (n - (n-1)) + (n-(n-2)) + (n-(n-3)) + \dots + (n-1) + n.
\end{equation}

When we plug in 5 again, we get
$5 - 4 + 5 - 3 + 5 - 2 + 5 - 1 + 5$, which is
$1+2+3+4+5$.

The next point to realise is that a series is a
\emph{mathematical object} and that means that we can
manipulate it. We can, for instance, add $S_n$ to itself:

\[
S_n + S_n = 2S_n.
\]

To compute the formula for $2S_n$ we can use
any valid representation of $S_n$, for instance
we can use the first formula to represent the first $S_n$
and the second to represent the second $S_n$.
The following equation shows this addition with
terms aligned that contain the sub-term $n-k$ for the same $k$:

\begin{equation}
\begin{array}{lclclclccclcl}
&2S_n &=&   & (1 + (n-1)) & + & (1 + (n-2)) & + & \dots & + & 1 + (n-(n-1)) & + & 1 \\
&     & & + & (n - (n-1)) & + & (n-(n-2))   & + & \dots & + & (n-1) & + & n.
\end{array}
\end{equation}

We see that in each tuple, the sub-term $n-k$ is added in the first line
and subtracted in the second. In consequence,
all these sub-terms fall away. We are left with

\begin{equation}
\begin{array}{lclclclccclcl}
&2S_n &=&   & 1 & + & 1 & + & \dots & + & 1 + n & + & 1 \\
&     & & + & n & + & n & + & \dots & + & 0     & + & n.
\end{array}
\end{equation}

This in its turn simplifies to

\begin{equation}
2S_n = 1 + n + 1 + n + \dots + 1 + n =
n(1 + n).
\end{equation}

When we divide by 2, we get the Little Gauss.

This is a nice technique! Indeed, treating formulas (and even sequences)
as objects that can be manipulated is a very powerful idea that we will
encounter over and over again. But can we do more with $S_n$?

An interesting question we could ask is what the sum of
any subsequence of the natural numbers is, \eg\ what is the sum of
$5\dots 10$?

The concrete formula would be
$5+6+7+8+9+10$.
This can be expressed as
$5 + (5+1) + (5+2) + (5+3) + (5+4) + (5+5)$, where $n$
is the length of the sequence, \ie\ $n=10-5+1=6$.
In terms of $S_n$, we could write for $a_1=5$:

\begin{equation}
S_n = a_1 + (n-1) + a_1 + (n-2) + \dots + a_1 + n+(n-1) + a_1.
\end{equation}

This corresponds to $5 + 5 + 5 + 4 + 5 + 3 + 5 + 2 + 5 + 1 + 5$ or
$10 + 9 + 8 + 7 + 6 + 5$.

The other variant of the formula would be

\begin{equation}
S_n = a_n - (n - 1) + a_n - (n-2) + \dots (a_n -1) + a_n.
\end{equation}

This second equation would express the sum as
$10 - 5 + 10 - 4 + 10 - 3 + 10 - 2 + 10 -1 + 10$, which is
just $5 + 6 + 7 + 8 + 9 + 10$.

If we use these two variants to calculate $2S_n$, we see
that all the sub-terms $(n-k)$ fall away, we are left with
a sequence of the form $a_1 + a_n + a_1 + a_n\dots$.
Since there are $n$ pairs of $a_1 + a_n$, we can simplify
to

\[
n(a_1 + a_n).
\]

When we divide by 2, we get a more general \emph{Little Gauss}:

\begin{equation}
S_n = \frac{n(a_1 + a_n)}{2}.
\end{equation}

Let us test this formula on $5+6+7+8+9+10 = 45$:

\[
\frac{6\times (5+10)}{2} = 3\times 15 = 45.
\]

Can we generalise this even further to a formula
that is not restricted to the sequences that progress
by a single unity per step, \ie\ for sequences
where the difference between the elements is a constant
term greater than 1, \eg\:

\[
2, 4, 6, 8, 10, \dots
\]

or

\[
1, 4, 7, 10, 13, \dots
\]

In this cases, we cannot express the series in terms of
$a1 + 1 + a1 + 2\dots$, but we have to account for the
differences. We, hence, get a series of the form

\begin{equation}
S_n = a_1 + (a_1 + d) + (a_1 + 2d) + (a_1 + 3d) + \dots + (a_1 + (n-1)d)
\end{equation}

and, for the alternative wording of this series:

\begin{equation}
S_n = a_n - (n-d) + a_n - (n-2d) + a_n - (n-3d) + \dots + a_n - d + a_n.
\end{equation}

Again, we get tuples with equal sub-terms, but this time, the sub-terms
are not $n-k$, but $n-kd$. Anyway, all these sub-terms fall away and,
again, we are left with 

\begin{equation}
2S_n = n(a_1 + a_n).
\end{equation}

In other words, the generalised series for 
any \term{arithmetic progression}
is exactly the same as the one we saw before for the 
special case $d=1$.

Let us test this formula: $2+4+6+8+10=30$ is

\[
\frac{5(2+10)}{2} = \frac{60}{2} = 30.
\]

For the second case: $1+4+7+10+13=35$ and

\[
\frac{5(1+13)}{2} = 5\times 7 = 35.
\]

\ignore{
- turn this into a function that returns a function
}


\ignore{
Arithmetic
==========

1 .. n
Sn = 1 + (1 + 1) + (1 + 2) + (1 + 3) + ... + (1 + n-1)
Sn = (n - (n-1)) + (n - (n-2)) + ... + (n -1) + n

1 + n-1 
n - (n-1) + n - (n-2)
Sn+Sn = n(1+n)
2Sn = n(1+n)
Sn = n(1 + n)/2

a .. a + n
Sn = a1 + (a1 + 1) + (a1 + 2) + (a1 + 3) + ... + (an + n-1)
Sn = (an - (n-1)) + (an - (n-2)) + ... + (an - 1) + an

Sn + Sn = n(a1 + an)
2Sn = n(a1 + an)
Sn = n(a1 + an)/2

a .. a + dn
Sn = a1 + (a1 + d) + (a1 + 2d) + (a1 + 3d) + ... + (an + (n-1)d)
Sn = (an - (n-d)) + (an - (n-2d)) + ... + (an - d) + an

Sn + Sn = n(a1+an)

Arithmetic progression: n(a1+an)/2

Geometric
=========
\sum_{k=0}^{n-1}{ar^k} // <-- connection to GF: \sum_{n=0}^{\infy}{a_nx^n}

Sn = a + ar + ar^2 + ar^3 + ... + ar^(n-1)
rSn = ar + ar^2 + ar^3 + ... + ar^n
s-rs = s(1-r) = a - ar^n
s = (a - ar^n) / (1-r)
for r < 1 and n growing:
a/(1-r)

for a = 1
1/(1-r)

}
