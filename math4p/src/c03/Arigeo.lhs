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

which we can state a bit differently to eliminate the concrete
numbers in the first half of the formula, here from right to left:

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

When we divide by 2, we get the \term{Little Gauss}.

This is a nice technique! Indeed, treating formulas (and even sequences)
as objects that can be manipulated is a very powerful idea that we will
encounter over and over again. But can we do more with $S_n$?

An interesting question we could ask is what the sum of
any subsequence of the natural numbers is, \eg\ what is the sum of
$5\dots 10$?

The concrete formula would be
$5+6+7+8+9+10$.
This can be expressed as
$5 + (5+1) + (5+2) + (5+3) + (5+4) + (5+5)$. 
When we make $n$ the length of the sequence, \ie\ $n=10-5+1=6$,
and $a_1$ the first element of the sequence, \ie\ $a_1=5$,
we can rewrite this sequence in terms of $S_n$:

\begin{equation}
S_n = a_1 + (n-1) + a_1 + (n-2) + \dots + a_1 + n+(n-1) + a_1.
\end{equation}

This corresponds to $5 + 5 + 5 + 4 + 5 + 3 + 5 + 2 + 5 + 1 + 5$ or
$10 + 9 + 8 + 7 + 6 + 5$.

The other variant of the formula would be (for $a_n=10$)

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
by a single unit per step, \ie\ for sequences
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
difference, $d$. We, hence, get a series of the form

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

For the second case, $1+4+7+10+13=35$:

\[
\frac{5(1+13)}{2} = \frac{5\times 14}{2} = 5\times 7 = 35.
\]

We can turn this into a Haskell function that
computes the sum for any sequence within an arithmetic progression
(\ie\ a sequence of numbers where each number is $d$ greater
than its predecessor where $d$ is a constant number).
The following Haskell code is an example of such a function:

\begin{minipage}{\textwidth}
\begin{code}
arith :: [Natural] -> Natural
arith []  = 0
arith ns  = n*(h+l) `div` 2
  where  n  = fronIntegral (length ns)
         h  = head ns
         l  = last ns
\end{code}
\end{minipage}

Note, by the way that this function always returns an integer.
To prove this we distinguish two cases:

\begin{enumerate}
\item $n$ is even
\item $n$ is odd.
\end{enumerate}

If $n$ is even, the numerator is even too and the fraction is,
hence, an integer.
If $n$ is odd, then $h$ and $l$ are both either even or odd,
but not one odd and the other even.
The sum of two even or two odd numbers, however, is always even.
In consequence, the numerator is even and the whole fraction
is an integer.\qed

An interesting variant of this function
is a function that computes a function
which, in its turn, computes a value for $n$ based on a given
arithmetically progressing sequence, \ie:

\begin{minipage}{\textwidth}
\begin{code}
arif :: [Natural] -> (Natural -> Natural)
arif []  =  \ _ -> 0
arif ns  =  \n -> n*(h+l) `div` 2
  where  h  =  head ns
         l  =  last ns
\end{code}
\end{minipage}

This function creates a function
from the sequence passed in.
The function computes the function already
implemented in |arith|, but with the value
for $n$ not being fixed to the length 
of the input sequence, but defined as a
parameter of the resulting function.

We can create an instance of such a function 
by calling |let f = [1,4,7,10,13]|.
Now we apply f on a set of numbers: |map f [1..10]|
and get:

|[7,14,21,28,35,42,49,56,63,70]|.

This looks suspiciously like multiples of 7!
Why is that again?
Have a look at the formula: we compute $(h+l)/2$,
which is the \term{averag} of the number $h$ and $l$.
The average of the sequence $1\dots 13$ is 7, \ie\
$(1+13)/2 = 14/2$. The function, we effectively return
in |arif| for this sequence, hence, is $f(n) = n\times 7$.

Can we apply the techniques used here for arithmetic progression
also for other kinds of sequences? 
One such type of sequence is \term{geometric progressions} 
where each number is greater than its predecessor by a \term{factor}.
In the following sequence, for instance, each number
is two times its predecessor:

\[
1, 2, 4, 8, 16, 32, \dots
\]

What is the sum of $n$ of such numbers?
Instead of wildly guessing around 
like we did in the previous section,
let us look at the sequence as a single object.
We can model the sequence as

\begin{equation}
S_n=a + ar + ar^2 + ar^3 + \dots + ar^{n-1}
\end{equation}

or, in \emph{short hand}:

\begin{equation}
S_n = \sum_{k=0}^{n-1}{ar^k}.
\end{equation}

Again, we can treat $S_n$ as a mathematical object
and manipulate it. We can of course try to add and subtract
it to and from itself, but since we are now dealing with
geometric progression, it quickly turns out that that is
not attractive. It is more interesting to multiply $S_n$.
One thing we can do: we can multiply $S_n$ by $r$:

\begin{equation}
rS_n=ar + ar^2 + ar^3 + \dots + ar^n
\end{equation}

Now we subtract $S_n - rS_n$ (and arrange equal terms):

\begin{equation}
\begin{array}{lcclclclcccl}
 S_n-rS_n  &=&   &      & + & ar^{n-1} & + & ar^{n-2} & + & \dots & + & a \\
           & & - & ar^n & - & ar^{n-1} & - & ar^{n-2} & - & \dots &   & 
\end{array}
\end{equation}

We are left with $a-ar^n$.
But how do we get $S_n$ back?
Simply by factoring $S_n$ out of $S_n-rS_n$, which leads to

\[
S_n(1-r).
\]

Now, we divide by $1-r$ and get $S_n$:

\begin{equation}
S_n = \frac{a-ar^n}{1-r} = \frac{a(1-r^n)}{1-r}
\end{equation}

For $a=1$ (like in the example above),
the equation simplifies to

\begin{equation}
S_n = \frac{1-r^n}{1-r}
\end{equation}

and this equation defines the basic form of a \term{geometric series}.
When we plug in the sequence $1\dots 32$, we get

\begin{equation}
S_n = \frac{1-2^6}{1-2} = \frac{-63}{-1} = 63,
\end{equation}

so the sum of the powers of 2 from 1 to $2^5$ is 63,
\ie\ $2^6-1$, a formula you, as a programmer,
have certainly seen already.

Techniques of looking at a sequence of numbers
in terms of a series,
\ie\ as the sum of its elements, is the basis of
the very important concept of
\term{generating functions}.
The fundamental idea is to turn a bunch of things
into a single object that can be manipulated.
The Hungarian mathematician George Pólya (1887 -- 1985)
compared them to bags in which to put other things,
so you can carry many things around without difficulty.
Herbert Wilf (1931 -- 2012), whom we will meet later,
said ``a generating function is a clothesline
on which we hang up a sequence of numbers for display''.

Our shorthand of the geometric series formula was 

\[
\sum_{k=0}^{n-1}{ar^k}.
\]

The equation for an \term{ordinary generating function}, $G$,
would be:

\begin{equation}
G_n = \sum_n^{\infty}{ar^n}.
\end{equation}

The main difference, hence, is that $G$ is
an \term{infinite} series, while our series are finite.
Anyway, generating functions involve much more advanced
techniques, in particular we cannot work whith them
with natural numbers alone. Herbert Wilf even sees
generating functions as a link between 
discrete and continuous mathematics.
We, therefore, need to pause here 
and resume the topic later.
