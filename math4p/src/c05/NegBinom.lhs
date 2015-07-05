\ignore{
\begin{code}
module NegBinom
where
  import Zahl
\end{code}
}

The aim of this section is to generalise
binomial coefficients to integers using
the new data type |Zahl|.
There are many ways how to approach this goal,
which perhaps can be grouped into two main
approaches: first, we can look for an
application of such a generalisation in
the ``real world'' and search an appropriate
mathematical tool for this problem.
We started with binomial coefficients
by discussing combinatorial issues,
but, of course, combinatorial problems
like the ways to choose $k$ objects out of $n$
provide no meaningful interpretation for
negative numbers -- what should a negative
number of possibilities mean?
But there are other applications,
such as the multiplication of sums.

The second basic approach is not to look
for applications, but to investigate
the formalism asking ``what happens,
when we change it?''
This may appear much less interesting
at the first sight, since there is no
obvious use case for such an altered
formalism. However, such formal approaches
do not only help deepening the insight
into specific mathematical problems,
but they also lead to new tools, which
may find their applications in the future.
This has happened more than once in the
history of mathematics.
When David Hilbert, the champion of the
formalistic approach, redefined geometry
extending it from the two-dimensional plane
and the three-dimensional space to 
$n$-dimensional manifolds, there was no
concrete application in sight.
It took only a short while, however,
before John von Neumann and others started using
the concepts of Hilbert's geometry to model
quantum physics.

Anyhow, we start with the second approach.
Still, there are many ways to go.
We can start with the formula $\binom{n}{k}$
and ask ourselves what happens if
$n<0$ or $k<0$.
To begin with, let us assume
$n<0$ and $k\ge 0$.
If we just apply the formula
$\frac{n(n-1)\dots (n-k+1)}{k!}$,
we get for $\binom{-n}{k}$
a kind of falling factorial in the numerator
that looks like a rising factorial with minus signs
in front of the numbers, \eg\ $\binom{-6}{3}$ is
$\frac{-6 \times -7 \times -8}{6}$,
which is $-1 \times -7 \times -8 = -56$.
Obviously, the signedness of the result
depends on whether $k$ is even or odd.
Since there are $k$ negative factors 
in the numerator, the product will be positive
if $k$ is even and negative otherwise.

Here is a Haskell implementation
for this version of negative binomial
coefficients:

\ignore{
\begin{code}
  toTheKfalling :: (Num n,Integral n) => n -> n -> n
  toTheKfalling n k = go n k
    where  go m 0 = m
           go m i = m * go (m-1) (i-1)

  toTheKrising  :: (Num n,Integral n) => n -> n -> n
  toTheKrising n k = go n k
    where  go m 0 = m
           go m i = m * go (m+1) (i-1)

  infix >|
  (>|) :: (Num n,Integral n) => n -> n -> n
  (>|) = toTheKfalling

  infix |>
  (|>) :: (Num n,Integral n) => n -> n -> n
  (|>) = toTheKrising 
\end{code}
}

\begin{minipage}{\textwidth}
\begin{code}
  chooseNeg :: Zahl -> Natural -> Zahl
  chooseNeg n k  | n >= 0 && k >= 0  =  Pos (choose (z2n n) k)
                 | n == k            =  1
                 | k == 0            =  1
                 | k == 1            =  n
                 | otherwise         =  (n >| (Pos k)) `div` (fac (Pos k))
\end{code}
\end{minipage}

This function accepts two arguments,
one of type |Zahl| and the other of type |Natural|.
For the moment, we want to avoid negative $k$s
and to rule negative values out right from the beginning,
we choose |Natural| as data type for $k$.

When both, $n$ and $k$, are positive,
we just use the old |choose| converting $n$ to |Natural|.
Then we handle the trivial cases.
Finally, we just implement one of the formulas for
binomial coefficients.
Here are some values:

|map (chooseNeg (-1)) [0..9]|\\
|[1,-1,1,-1,1,-1,1,-1,1,-1]|\\[12pt]
|map (chooseNeg (-2)) [0..9]|\\
|[1,-2,3,-4,5,-6,7,-8,9,-10]|\\[12pt]
|map (chooseNeg (-3)) [0..9]|\\
|[1,-3,6,-10,15,-21,28,-36,45,-55]|\\[12pt]
|map (chooseNeg (-4)) [0..9]|\\
|[1,-4,10,-20,35,-56,84,-120,165,-220]|\\[12pt]
|map (chooseNeg (-5)) [0..9]|\\
|[1,-5,15,-35,70,-126,210,-330,495,-715]|\\[12pt]
|map (chooseNeg (-6)) [0..9]|\\
|[1,-6,21,-56,126,-252,462,-792,1287,-2002]|

We see that the coefficients for each $n$
alternate between positive and negative values
depending on $k$ being even or odd.
We also see that there is no limit anymore
from which on the coefficients are all zero.
In the original definition,
we had $\binom{n}{k}=0$ for $k>n$.
But now we have to give up that rule,
because for $n<0$ and $k\ge 0$,
$k>n$ trivially holds for all cases.
In consequence, we lose the nice symmetry
we had in the original triangle.
Of course, we can restore many of the old
characteristics by changing the definition
of |chooseNeg| for negative $n$s to
|(n ||> (Pos k)) `div` (fac (Pos k))|.
In this variant, let us call it |chooseNeg2|,
we use the rising factorial,
such that the absolute values of the numbers 
in the numerator equal the numbers in the numerator
for $n\ge 0$.
For instance:

|map (chooseNeg2 (-2)) [0..9]|\\
|[1,-2,1,0,0,0,0,0,0,0]|\\[12pt]
|map (chooseNeg2 (-3)) [0..9]|\\
|[1,-3,3,-1,0,0,0,0,0,0]|\\[12pt]
|map (chooseNeg2 (-4)) [0..9]|\\
|[1,-4,6,-4,1,0,0,0,0,0]|\\[12pt]
|map (chooseNeg2 (-5)) [0..9]|\\
|[1,-5,10,-10,5,-1,0,0,0,0]|\\[12pt]
|map (chooseNeg2 (-6)) [0..9]|\\
|[1,-6,15,-20,15,-6,1,0,0,0]|

The first solution, |chooseNeg|, however,
is more faithful to the original definition
of the binomial coefficients,
even if its results do not resemble the original results.
One of the characteristics that is preserved
is Pascal's rule:

\begin{equation}
  \binom{n}{k} = \binom{n-1}{k-1} + \binom{n-1}{k}.
\end{equation}

Indeed, we could use Pascal's rule
to find negative coefficients in the first place.
If we are interested in the coefficient
$\binom{n-1}{k}$, we just subtract
$\binom{n-1}{k-1}$ from both sides and get

\begin{equation}\label{eq:PascalNegativeN}
  \binom{n-1}{k} = \binom{n}{k} - \binom{n-1}{k-1}.
\end{equation}

We can use this equation to search the unknown
territory of negative coefficients starting
from the well-known territory of positive coefficients
using each result as a base camp for further
expeditions.
We start with $\binom{0}{0}$ and want to know
the value for $\binom{-1}{0}$.
Since coefficients for $k=0$ are defined as 1,
this case turns out to be trivial:
$\binom{-1}{0} = 1$.
The next is $\binom{-1}{1}$, which is
$\binom{0}{1} - \binom{-1}{0}$, hence
$0 - 1 = -1$. The next is 
$\binom{-1}{2} = \binom{0}{2} - \binom{-1}{1}$, which is
$0 - (-1) = 0 + 1 = 1$.
Next: $\binom{-1}{3} = \binom{0}{3} - \binom{-1}{2}$,
which is $0 - 1 = -1$ and so on.
Indeed, the coefficients of $-1$, as we have seen before,
are just alternating
positive and negative 1s:

|map (chooseNeg (-1)) [0..9]|\\
|[1,-1,1,-1,1,-1,1,-1,1,-1]|

On the basis of this result,
we can investigate the coefficients of $-2$.
We know that $\binom{-2}{0}$ is 1 and continue with
$\binom{-2}{1}$, which is $\binom{-1}{1} - \binom{-2}{0}$,
which is $-1 - 1 = -2$.
The next is $\binom{-2}{2} = \binom{-1}{2} - \binom{-2}{1}$
or $1 - (-2) = 1 + 2 = 3$.
We continue with $\binom{-2}{3} = \binom{-1}{3} - \binom{-2}{2}$,
which is $-1 - 3 = -4$.
It turns out that the coefficients for $n=-2$ are

|map (chooseNeg (-2)) [0..9]|\\
|[1,-2,3,-4,5,-6,7,-8,9,-10]|,

which is a beautiful result.
If we go on this way, we will reproduce the values observed above
using |chooseNeg|.

Now, what about negative $k$s?
There is no direct way to implement something like |chooseNeg|
for negative $k$s, because we do not know what
a negative factorial would mean.
Of course, we can apply a trick 
very similar to that we used for |chooseNeg2|,
\ie\ we compute the factorial not as
|fac n = n * fac (n-1)|, but as
|fac n = n * fac (n+1)| going up towards zero.
In this case, we also have to take care of
the rising or falling factorial of the numerator.
For instance, we can use 
the falling factorial with the inverse of $k$,
such that the value of the numerator remains
the same independent of $k$ being positive or
negative. The result would resemble that
of |chooseNeg2|, \ie\ we would have
alternating positive and negative coefficients
for $n > 0$ and positive coefficients for $n < 0$.

More interesting is using Pascal's rule
to create coefficients for negative $k$s.
But we have to be careful.
In equation \ref{eq:PascalNegativeN},
we used a coefficient for $k-1$
to find the coefficient for $k$.
This will not work. When we look for
$\binom{n}{-k}$, we do not yet know 
the value for $\binom{n}{-(k+1)}$,
since we are entering the territory
of negative numbers from above.
Therefore, we need a variant of 
equation \ref{eq:PascalNegativeN}.
Instead of subtracting $\binom{n-1}{k-1}$
from Pascal's rule, we subtract the other
term $\binom{n-1}{k}$ and get

\begin{equation}\label{eq:PascalNegativeK}
\binom{n-1}{k-1} = \binom{n}{k} - \binom{n-1}{k}.
\end{equation}

It is obvious that any coefficient
resulting from a positive $n$
and a negative $k$ in this way equals 0,
since any coefficient with $k=0$ is 1.
So, $\binom{n}{-1} = 1 - 1 = 0$.
However, we also have $\binom{0}{k} = 0$
for any $k$ and $\binom{n}{n} = 1$.
For $\binom{-1}{-2}$, we therefore have
$\binom{0}{-1} - \binom{-1}{-1} = 0 - 1 = -1$.
For $\binom{-1}{-3}$, we get
$0 - \binom{-1}{-2} = 0 - (-1) = 0 + 1 = 1$.
The next coefficient $\binom{-1}{-4}$,
froreseeably, is $0 - \binom{-1}{-3}$,
which is $0 - 1 = -1$.
Again, for $n=-1$, we get a sequence
of alternating negative and positive ones.

For $\binom{-2}{k}$, we get 
$\binom{-2}{-2} = 1$, $\binom{-2}{-3}$
is $\binom{-1}{-2} - \binom{-2}{-2} = -1 - 1 = -2$.
$\binom{-2}{-4}$ then is 
$\binom{-1}{-3} - \binom{-2}{-3} = 1 - (-2) = 3$.
The next coefficient is 
$\binom{-2}{-5} = \binom{-1}{-4} - \binom{-2}{-3}$,
which is $-1 - 3 = -4$ and so on.
The binomial coefficients for -2 with negative $k$s,
thus, is just the sequence $1,-2,3,-4,5,-6,7,-8,9,-10,\dots$,
which we have already seen for positive $k$s above.
The symmetry of the triangle, hence, is reinstalled.
For coefficients with $n=-2$
and $k=-9,-8,\dots,-1,0,1,\dots,7$, 
we get the sequence

$-8,7,-6,5,-4,3,-2,1,0,1,-2,3,-4,5,-6,7,-8$.

To confirm this result, we implement the backward
rule as

\begin{minipage}{\textwidth}
\begin{code}
  pascalBack :: Zahl -> Zahl -> Zahl 
  pascalBack n 0 = 1
  pascalBack n k  | k == n     = 1
                  | k == 0     = 1
                  | n == 0     = 0
                  | n >  0 && k < 0   = 0
                  | n >  0 && k >= 0  = Pos (choose (z2n n) (z2n k)
                  | k <  0     = pascalBack (n+1) (k+1)  - pascalBack n (k+1)
                  | otherwise  = pascalBack (n+1) k      - pascalBack n (k-1)
\end{code}
\end{minipage}

We first handle the trivial cases
$n=k$, $k=0$ and $n=0$.
When $n > 0$ and $k < 0$, the coefficient is 0.
For $n$ and $k$ both greater 0,
we use |choose|.
For $k<0$, we use the rule in equation \ref{eq:PascalNegativeK}
and for $n<0$, with $k>0$, we use the rule in
equation \ref{eq:PascalNegativeN}.
Now we map |pascalBack| for specific $n$s to a range of $k$s:

|map (pascalBack (-1)) [-9,-8..9]|\\
|[1,-1,1,-1,1,-1,1,-1,1,1,-1,1,-1,1,-1,1,-1,1,-1]|\\[12pt]
|map (pascalBack (-2)) [-9,-8..9]|\\
|[-8,7,-6,5,-4,3,-2,1,0,1,-2,3,-4,5,-6,7,-8,9,-10]|\\[12pt]
|map (pascalBack (-3)) [-9,-8..9]|\\
|[28,-21,15,-10,6,-3,1,0,0,1,-3,6,-10,15,-21,28,-36,45,-55]|\\[12pt]
|map (pascalBack (-4)) [-9,-8..9]|\\
|[-56,35,-20,10,-4,1,0,0,0,1,-4,10,-20,35,-56,84,-120,165,-220]|\\[12pt]
|map (pascalBack (-5)) [-9,-8..9]|\\
|[70,-35,15,-5,1,0,0,0,0,1,-5,15,-35,70,-126,210,-330,495,-715]|

The symmetry of positive and negative $k$s is not perfect.
The coefficients for $n<0$ and $n < k < 0$ are all 0.
The sequence, in consequence, is mirrowed
with a delay of $||n||-1$ $k$s, such that
the coefficient that corresponds to the coefficient $\binom{n}{k}$
is not $\binom{n}{-k}$, but rather $\binom{n}{n-k}$.
For instance: 
$\binom{-2}{4} = 5 = \binom{-2}{-6}$,
$\binom{-3}{5} = -21 = \binom{-3}{-8}$ and
$\binom{-5}{2} = 15 = \binom{-5}{-7}$.

Looking at the numbers of negative $n$s,
I have the strange feeling that I already
saw those sequences somewhere.
But where? These are definitely not the rows
of Pascal's triangle, but perhaps something else?
Let us look at the triangle once again:

\begin{tabular}{l c c c c c c c c c c c c c c c c c c c c}
0 &   &   &   &   &    &    &    &    &     &  1 &     &    &    &    &    &   &   &   &   &  \\
1 &   &   &   &   &    &    &    &    &   1 &    &   1 &    &    &    &    &   &   &   &   &  \\
2 &   &   &   &   &    &    &    &  1 &     &  2 &     &  1 &    &    &    &   &   &   &   &  \\
3 &   &   &   &   &    &    &  1 &    &   3 &    &   3 &    &  1 &    &    &   &   &   &   &  \\
4 &   &   &   &   &    &  1 &    &  4 &     &  6 &     &  4 &    &  1 &    &   &   &   &   &  \\
5 &   &   &   &   &  1 &    &  5 &    &  10 &    &  10 &    &  5 &    &  1 &   &   &   &   &  \\   
6 &   &   &   & 1 &    &  6 &    & 15 &     & 20 &     & 15 &    &  6 &    & 1 &   &   &   &  \\
7 &   &   & 1 &   &  7 &    & 21 &    &  35 &    &  35 &    & 21 &    &  7 &   & 1 &   &   &  \\
8 &   & 1 &   & 8 &    & 28 &    & 56 &     & 70 &     & 56 &    & 28 &    & 8 &   & 1 &   & \\
9 & 1 &   & 9 &   & 36 &    & 84 &    & 126 &    & 126 &    & 84 &    & 36 &   & 9 &   & 1
\end{tabular}

Now follow the diagonals from the upper vertex
left and right downwards.
On both sides you see first the 1s,
then the counting numbers $1,2,3,\dots$,
then the sequence we saw with $\binom{-3}{k}$,
then the sequence we saw with $\binom{-4}{k}$
and so on.
In other words, if we turn the triangle
by $90^{\circ}$ counterclockwise, 
we obtain the sequences for negative $n$;
the same also works with a clockwise turn,
but then we read the sequence from right to left.

This boils down to the equations

\begin{equation}\label{eq:NegPos_1}
\left\lvert\binom{-n}{1}\right\rvert 
= \binom{n}{1}
= \binom{n}{n-1}
\end{equation}

and

\begin{equation}\label{eq:NegPos_k}
\left\lvert\binom{-n}{k+1}\right\rvert 
= \binom{n+k-1}{k+1}
= \binom{n+k-1}{n-1}.
\end{equation}

For instance:

\begin{align*}
\left\lvert\binom{-3}{1}\right\rvert &&=&&\binom{3}{1} &&=&&\binom{3}{2} &&=&& 3\\
\left\lvert\binom{-3}{2}\right\rvert &&=&&\binom{4}{2} &&=&&\binom{4}{2} &&=&& 6\\
\left\lvert\binom{-3}{3}\right\rvert &&=&&\binom{5}{3} &&=&&\binom{5}{2} &&=&& 10\\
\left\lvert\binom{-3}{4}\right\rvert &&=&&\binom{6}{4} &&=&&\binom{6}{2} &&=&& 15\\
\left\lvert\binom{-3}{5}\right\rvert &&=&&\binom{7}{5} &&=&&\binom{7}{2} &&=&& 21\\
\left\lvert\binom{-3}{6}\right\rvert &&=&&\binom{8}{6} &&=&&\binom{8}{2} &&=&& 28
\end{align*}

This result may look a bit surprising at the first sight.
But when we look at the formula that actually generates
the value, it is obvious:

\begin{equation}
\binom{n}{k} = \frac{n^{\underline{k}}}{k!}.
\end{equation}

When we have a negative $n$, the falling factorial
in the numerator is in fact a rising factorial with
negative numbers:

\[
-n^{\underline{k}} = -n \times -(n+1) \times \dots \times -(n+k-1).
\]

Each number in the product is one less than its predecessor,
but, since the numbers are negative, the absolute value
is greater than its predecessor.
If we eliminate the minus signs, we obtain the rising factorial
for $n$:

\[
n^{\overline{k}} = n \times (n+1) \times \dots \times (n+k-1),
\]

which is just the falling factorial for $n+k-1$:

\[
(n+k-1)^{\underline{k}} = (n+k-1) \times (n+k-2) \times \dots \times n.
\]

We can therefore conclude that

\begin{equation}
\left\lvert\binom{-n}{k}\right\rvert = 
\frac{(n+k-1)^{\underline{k}}}{k!} = \binom{n+k-1}{k}.
\end{equation}

That $\binom{n+k-1}{k}$ also equals
$\binom{n+k-1}{n-1}$ results from the fact
that $n-1$ and $k$ maintain the same distance
from one of the sides of the triangle,
\ie\ from either $\binom{n+k-1}{0}$ or $\binom{n+k-1}{n+k-1}$.
$k$ is trivially $k$ coefficients away from any $\binom{n}{0}$,
whereas $n-1$ is $(n+k-1) - (n-1)$ away from $\binom{n+k-1}{n+k-1}$,
which is $n+k-1-n+1=k$. This is just an implication
of the triangle's symmetry.

Somewhat more difficult is to see the relation to Pascal's rule.
In fact, we have never proven that Pascal's rule follows
from the fraction $\frac{n^{\underline{k}}}{k!}$.
If we can establish this relation, the backward rule
will follow immediately.
So, we definitely should try to prove Pascal's rule.

We want to establish that

\begin{equation}
\binom{n+1}{k} = \binom{n}{k} + \binom{n}{k-1}
\end{equation}

and do this directly using the definitions

\begin{equation}
\binom{n}{k} = \frac{n^{\underline{k}}}{k!}
\end{equation}

\begin{equation}
\binom{n}{k-1} = \frac{n^{\underline{k-1}}}{(k-1)!}
\end{equation}

Our claim is that

\begin{equation}\label{eq:Pascal1}
\binom{n+1}{k} = 
\frac{n^{\underline{k}}}{k!} +
\frac{n^{\underline{k-1}}}{(k-1)!}
\end{equation}

In other words, when we can deduce
the left-hand side of this equation
from the right-hand side, we are done.
So let us just add the two fractions on 
the right-hand side.
We first convert them to a common denominator.
We can do this simply by multiplying
the second fraction by $k$:

\[
\frac{kn^{\underline{k-1}}}{k(k-1)! = k!}
\]

We can join the two fractions with the common
denominator and obtain a sum in the numerator:

\[
\frac{n^{\underline{k}} + kn^{\underline{k-1}}}{k!}
\]

If we extend the falling factorials 
in the numerator we get

\[
n(n-1)\dots(n-k+1) + n(n-1)\dots(n-(k-1)+1)k. 
\]

The terms have the same number of factors,
where the last factor in the first term is $n-k+1$
and the one in the second term is $k$.
The factor before the last in the second term
is $n-(k-1)+1$, which is $n-k+1+1 = n-k+2$,
and this term is also the last but second factor
in the first term.
In other words, the factors of the terms are equal
with the exception of the last factor.
We can factor the equal factors out.
If we do this stepwise, this looks like
(using brackets to indicate what remains within
the sum):

\begin{minipage}{\textwidth}
\[
n[(n-1)(n-2)\dots (n-k+1) + (n-1)(n-2)\dots (n-k+2)k]
\]
\[
n(n-1)[(n-2)\dots (n-k+1) + (n-2)\dots (n-k+2)k]
\]
\[
\dots
\]
\[
n(n-1)\dots(n-k+2)[n-k+1+k].
\]
\end{minipage}

The remaining sum can now be simplified:
$n-k+1+k = n+1$ and with this 
we recognise in the whole expression
the falling factorial of $n+1$.
The whole fraction is now 

\[
\frac{(n+1)^{\underline{k}}}{k!},
\]

which is the definition of the binomial coefficient
$\binom{n+1}{k}$ and that concludes the proof.\qed

The proof establishes the relation between
the definition of binomial coefficients and
Pascal's rule. This spares us from going
through the laborious task of establishing
the relation expressed in equation \ref{eq:NegPos_k}
using only Pascal's rule.

We now switch to the first approach mentioned 
at the beginning of this section,
\ie\ trying to find a mathematical
formalism for a practical problem.
The practical problem is multiplication.
We want to know what happens
with negative $a$s or $b$s in products of the form

\[
(a + b)^n.
\]

Negative $n$s are not too interesting here,
since $(a+b)^{-n}$ is just the inverse of
$(a+b)^n$, which is $\frac{1}{(a+b)^n}$,
without any effect on the coefficients themselves.

We could, of course go on by trying out
this formula with concrete numbers $a$ and $b$.
It appears much more promising, however, to choose a 
symbolic approach manipulating strings of the form
|"a"| and |"b"|.
The idea is to use string operations to simulate
multiplication and addition.
We do so in two steps: first we combine strings,
then we simplify them in a way mimicking the 
rules of addition and multiplication.
Since we want to see the differences between
positive and negative coefficients, 
we need a means to negate strings simulating
negative numbers.
To this end, we define the simple data type

\begin{minipage}{\textwidth}
\begin{code}
  data Sym = P String | N String
    deriving (Eq,Show)
\end{code}
\end{minipage}

where, as you may have guessed,
the |P|-constructor represents positive strings
and the |N|-constructor represents negative strings.
We now combine two symbols to simulate
multiplication:

\begin{minipage}{\textwidth}
\begin{code}
  comb :: Sym -> Sym -> Sym
  comb  (P a)  (P b)  =  P  (a++b)
  comb  (P a)  (N b)  =  N  (a++b)
  comb  (N a)  (P b)  =  N  (a++b)
  comb  (N a)  (N b)  =  P  (a++b)
\end{code}
\end{minipage}

You probably realise the pattern
we already used to define the number type |Zahl|:
two numbers of equal signedness result in a positive number and
two numbers of different signedness result in a negative number.
Multiplication itself is just the concatenation of
the two strings.
|comb (P "a") (P "b")|, hence, is |P "ab"|;
|comb (N "a") (P "b")| is |N "ab"|.

We represent addition as lists of strings.
We then can formulate the distributive law as

\begin{minipage}{\textwidth}
\begin{code}
  combN :: Sym -> [Sym] -> [Sym]
  combN x = map (comb x)
\end{code}
\end{minipage}

where we multiply a number, $x$, with a sum
by multiplying that number with each term of the sum,
mapping the basic combinator operation |comb|
on the list representing the sum.
Based on |combN|, we can now define the multiplication
of a sums by itself:

\begin{minipage}{\textwidth}
\begin{code}
  combine :: [Sym] -> [Sym]
  combine xs = concat [combN x xs | x <- xs]
\end{code}
\end{minipage}

Let us look at an example to get a grip on |combine|:

|let a = P "a"|\\
|let b = P "b"|\\
|combine [a,b] = concat [combN x [a,b] || x <- [a,b]]|.

The list comprehension will first apply |combN a [a,b]|,
resulting in |[aa,ab]|, and then it will apply
|combN b [a,b]| resulting in |[ba,bb]|.
These two lists are then merged using |concat|
resulting in |[aa,ab,ba,bb]|.
We will later have to simplify this list,
since, as we know, |ab| and |ba| are equal
and can be written $2ab$.
We come back to this immediately,
but first we want to implement one more function,
namely a combinator that applies |combine| $n$ times:

\begin{minipage}{\textwidth}
\begin{code}
  combinator :: [Sym] -> Natural -> [Sym]
  combinator  _  0 = []
  combinator  xs n = go xs (n-1)
    where  go ys 0  =  ys
           go ys n  =  go (concat [combN x ys | x <- xs]) (n-1)
\end{code}
\end{minipage}

Note that this function does not reuse |combine|,
but implements it anew.
The reason is that we do not want to combine
the original input with itself,
but with the result of the previous recursion.
For instance, if $n=3$,
then we start with |[combN x [a,b] || x <- [a,b]]|
resulting in |[aa,ab,ba,bb]|.
In the next round, we multiply this result
by |[a,b]|: |[combN x [aa,ab,ba,bb] || x <- [a,b]]|
resulting in |[aaa,aab,aba,abb,baa,bab,bba,bbb]|
and corresponding to the expression $(a+b)^3$.

Another interesting aspect of |combinator|
is the base case for $n=0$,
which is just defined as being the empty list.
In this context, the empty list would, hence,
represent the number 1, since 1, as we know,
is the coefficient $\binom{0}{0}$.

Now we want to simplify results, such that
|[aa,ab,ba,ba] = [aa,2*ab,ba]|.
First, we need to express that |"ab"| and |"ba"|
are the same thing.
Therefore, we sort the string:

\begin{minipage}{\textwidth}
\begin{code}
  sortStr :: Sym -> Sym
  sortStr (P a)  = P  (sort a)
  sortStr (N a)  = N  (sort a)
\end{code}
\end{minipage}

leading to a canonical form for strings.
The call |sortStr (P "ba")| would result in
|P "ab"|.

To simplify a complete list, we compare all
symbols in the lists and consolidate
symbols with equal strings resulting
in lists of the type |(Natural,Sym)|,
where the natural number counts the ocurrences
of that symbol in the list:

\begin{minipage}{\textwidth}
\begin{code}
  simplify :: [Sym] -> [(Natural, Sym)]
  simplify xs = go (sort $ map sortStr xs) 1
    where  go []  _ = []
           go [x] n = [(n,x)]
           go  ((P x):(P y):zs)  n  | x == y     = go ((P y):zs) (n+1)
                                    | otherwise  = (n,P x) : go ((P y) : zs) 1
           go  ((N x):(N y):zs)  n  | x == y     = go ((N y):zs) (n+1)
                                    | otherwise  = (n,N x) : go ((N y) : zs) 1
           go  ((N x):(P y):zs)  n  | x == y     = go zs 1
                                    | otherwise  = (n,N x) : go ((P y) : zs) 1
           go  ((P x):(N y):zs)  n  | x == y     = go zs 1
                                    | otherwise  = (n,P x) : go ((N y) : zs) 1
\end{code}
\end{minipage}
\ignore{$}

We start by first transforming all symbols
into the canonical form sorting their strings.
Then we sort the list of symbols itself.
The idea is that all strings of the same kind
are listed in a row, such that we can easily
count them.
But, of course, we do not want to have the
negative and positive symbols separated,
we want all symbols with the same string in a row,
independently of these symbols being positive
or negative.
We have to implement this notion of comparison
ignoring the sign. To this end, we make |Sym|
an instance of |Ord|:

\begin{minipage}{\textwidth}
\begin{code}
  instance Ord Sym where
    compare (P a)  (P b)  = compare a b
    compare (P a)  (N b)  = compare a b
    compare (N a)  (P b)  = compare a b
    compare (N a)  (N b)  = compare a b
\end{code}
\end{minipage}

We now |go| through the list of symbols sorted 
in this sense of comparison
and check on the first and the second of the remaining list
on each step.
If the signedness of first and second are equal
and their strings are equal, we
increment $n$
and store |(n,x)|, |x| the head of the list,
whenever they differ
starting the rest of the list with $n=1$.
Otherwise, when the signs are not equal,
but the strings are, then we drop both symbols
and continue with the rest of the list 
after the second.

Applied step for step on the list

|[aaa,aab,aba,abb,baa,bab,bba,bbb]|,

this would advance as follows.
We first sort all strings resulting

|[aaa,aab,aab,abb,aab,abb,abb,bbb]|.

We next sort the symbols:

|[aaa,aab,aab,aab,abb,abb,abb,bbb]|.

We then weight according to the number of their appearance:

|[(1,aaa),(3,aab),(3,abb),(1,bbb)]|.

In this example, we ignore signedness,
since all terms are positive anyway.
The interesting thing thus is still
to commence: the application to sums
with negative terms.
We do this with the simple function

\begin{minipage}{\textwidth}
\begin{code}
  powsum :: [Sym] -> Natural -> [(Natural,Sym)]
  powsum xs = simplify . combinator xs
\end{code}
\end{minipage}

We now define two variables,
a positive string $a$ and a negative one $b$:
|let a = P "a"| and |let b = N "b"|
and just apply |powsum| with increasing $n$s:

|powsum [a,b] 1|\\
|[(1,P "a"),(1,N "b")]|\\[12pt]
|powsum [a,b] 2|\\
|[(1,P "aa"),(2,N "ab"),(1,P "bb")]|\\[12pt]
|powsum [a,b] 3|\\
|[(1,P "aaa"),(3,N "aab"),(3,P "abb"),(1,N "bbb")]|

It appears that the absolute values of the coefficients 
do not change. We have, for instance,
$\binom{3}{0} = 1$,
$\binom{3}{1} = -3$,
$\binom{3}{2} = 3$,
$\binom{3}{3} = -1$.
What, if we swap the negative sign:
|let a = N "a"| and |let b = P "b"|?

|powsum [a,b] 1|\\
|[(1,N "a"),(1,P "b")]|\\[12pt]
|powsum [a,b] 2|\\
|[(1,P "aa"),(2,N "ab"),(1,P "bb")]|\\[12pt]
|powsum [a,b] 3|\\
|[(1,N "aaa"),(3,P "aab"),(3,N "abb"),(1,P "bbb")]|

The result is just the same for even exponents.
For odd exponents, the signs are just exchanged:
$\binom{3}{0} = -1$,
$\binom{3}{1} = 3$,
$\binom{3}{2} = -3$,
$\binom{3}{3} = 1$.

What if both terms are negative:
|let a = N "a"| and |let b = N "b"|?

|powsum [a,b] 1|\\
|[(1,N "a"),(1,N "b")]|\\[12pt]
|powsum [a,b] 2|\\
|[(1,P "aa"),(2,P "ab"),(1,P "bb")]|\\[12pt]
|powsum [a,b] 3|\\
|[(1,N "aaa"),(3,N "aab"),(3,N "abb"),(1,N "bbb")]|

Now, wonder of wonders,
even exponents lead to positive coefficients,
while odd exponents lead to negative coefficients:
$\binom{3}{0} = -1$,
$\binom{3}{1} = -3$,
$\binom{3}{2} = -3$,
$\binom{3}{3} = -1$.

This result appears quite logical,
since, with even exponents,
we multiply an even number of negative factors,
while, with odd exponents,
we multiply an odd number of negative factors.

To confirm these results with more 
data, we define one more function
to extract the coefficients and, this way,
making the output more readable:

\begin{minipage}{\textwidth}
\begin{code}
  coeffs :: [Sym] -> Natural -> [Natural]
  coeffs xs = map getCoeff . powsum xs
\end{code}
\end{minipage}

where |getCoeff| is

\begin{minipage}{\textwidth}
\begin{code}
  getCoeff :: (Natural,Sym) -> Natural
  getCoeff (n,P _) = n
  getCoeff (n,N _) = -n
\end{code}
\end{minipage}

We now map |coeffs| on the numbers |[0..9]|
and, with $a$ and $b$ still both negative,
see Pascal's triangle with signs alternating
per row:

\begin{minipage}{\textwidth}
|[]|\\
|[-1,-1]|\\
|[1,2,1]|\\
|[-1,-3,-3,-1]|\\
|[1,4,6,4,1]|\\
|[-1,-5,-10,-10,-5,-1]|\\
|[1,6,15,20,15,6,1]|\\
|[-1,-7,-21,-35,-35,-21,-7,-1]|\\
|[1,8,28,56,70,56,28,8,1]|\\
|[-1,-9,-36,-84,-126,-126,-84,-36,-9,-1]|
\end{minipage}

For one of the value $a$ and $b$ positive and
the other negative, we see the coefficients in one row
alternating in signedness.
For $a$ positive and $b$ negative, we see:

\begin{minipage}{\textwidth}
|[]|\\
|[1,-1]|\\
|[1,-2,1]|\\
|[1,-3,3,-1]|\\
|[1,-4,6,-4,1]|\\
|[1,-5,10,-10,5,-1]|\\
|[1,-6,15,-20,15,-6,1]|\\
|[1,-7,21,-35,35,-21,7,-1]|\\
|[1,-8,28,-56,70,-56,28,-8,1]|\\
|[1,-9,36,-84,126,-126,84,-36,9,-1]|
\end{minipage}

The other way round, $a$ negative and $b$ positive,
we see just the same triangle where, for odd exponents,
the minus signs are swapped.
The triangle, hence, is the same as the one before
with each row reversed:

\begin{minipage}{\textwidth}
|[]|\\
|[-1,1]|\\
|[1,-2,1]|\\
|[-1,3,-3,1]|\\
|[1,-4,6,-4,1]|\\
|[-1,5,-10,10,-5,1]|\\
|[1,-6,15,-20,15,-6,1]|\\
|[-1,7,-21,35,-35,21,-7,1]|\\
|[1,-8,28,-56,70,-56,28,-8,1]|\\
|[-1,9,-36,84,-126,126,-84,36,-9,1]|
\end{minipage}


