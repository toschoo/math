\ignore{
\begin{code}
module SternBrocot
where
  import Natural
  import Quoz
  import Real
\end{code}
}

Achille Brocot (1817 -- 1878) was part of a clockmaker dynasty
in Paris started by his father and continuing after his death.
The Brocots had a strong emphasis on engineering
and innovated clockmaking with the aim to reduce production cost
without equivalent degradation in quality.
The constant enginieering work manifested in a considerable
number of patents hold by family members. The most productive,
in terms of engineering, however, was Achille who improved
many of his father's inventions and developed his own ones.
He also introduced a novelty to mathematics, which, surprisingly,
has not only practical, but also theorectical value.

In clockmaking, as in machine construction in general,
determining the ratio of components to each other,
for instance, gear ratios, is a very frequent task.
As often in practice, those ratios are not nice and clean,
but very odd numbers with many decimal digits.
Brocot developed a way to easily approximate such numbers
with arbitrary precision. In the process, he developed a method
to approximate irrational numbers with arbitrary precision
and -- yet another way to list all rational numbers.

Brocot's method can be described in terms 
of finite continued fractions. Recall that we can
use lists of the form

\[
[n;a,b,c,\dots]
\]

to encode continued fractions like

\[
n + \frac{1}{a+\frac{1}{b+\frac{1}{c+\dots}}}.
\]

In contrast to continued fractions we have seen so far,
we now look at finite continued fractions that actually
result in rational numbers. The process to compute
such a continued fraction can be captured in Haskell as:

\begin{minipage}{\textwidth}
\begin{code}
  contfracr :: [Ratio] -> Ratio
  contfracr  []      = 0
  contfracr  [i]     = i 
  contfracr  (i:is)  = i + (invert $ contfracr is)
\end{code}
\end{minipage}

Here, |invert| is a function to create the multiplicative
invert of a fraction, \ie\ $invert(\frac{n}{d}) = \frac{d}{n}$
or in Haskell:

\begin{minipage}{\textwidth}
\begin{code}
  invert :: Ratio -> Ratio
  invert (Q n d) = Q d n
\end{code}
\end{minipage}

As you will see immediately, 
the expression |(invert $ contfracr is)|,
corresponds to 
\ignore{$}

\[
\frac{1}{\text{\textit{contfracr is}}}.
\]

The definition above, hence, creates a continued
fraction that terminates with the last
element in the list.

Now we introduce a simple rule to create
from any continued fraction given in list notation
two new continued fractions:

\begin{minipage}{\textwidth}
\begin{code}
  brocotkids :: [Ratio] -> ([Ratio],[Ratio])
  brocotkids r  =  let  h   = init r
                        l   = last r
                        s   = length r
                        k1  = h++[l+1]
                        k2  = h++[l-1,2]
                   in if even s then (k1,k2) else (k2,k1)
\end{code}
\end{minipage}

This function yields two lists, $k_1$ and $k_2$.
The order in which these numbers are returned depends
on the parity of the length of the list.

$k_1$ and $k_2$ are computed as 
the initial part of the input list,
to which, in the case of $k_1$, one number is added,
namely the last element of the input list plus 1,
or, in the case of $k_2$, two numbers are added, namely
the last element minus 1 and 2. 
For the input list |[0,1]|, 
which is just 1,
for instance,
$k_1$ is |[0,2]|, which is $\frac{1}{2}$, and
$k_2$ is |[0,0,2]|, which is $\frac{1}{\frac{1}{2}} = 2$.

When we look at the paritiy of 
the length of the input list,
we see that $k_1$ has the same parity as the input list 
and $k_2$ has the opposite parity.
In particular, if the input list is even,
then $k_1$ is even and $k_2$ is odd; if it is odd,
then $k_1$ is odd and $k_2$ is even.
$k_1$ simply substitutes
the last element of the input list by this element
incremented by 1, \ie\ a greater element.

Now, we see for an even list like |[a,b]| that there is an 
integer, $a$, to which the inverse of the second number,
$b$ is added. If $b$ grows, then the overall result shrinks.
The structure of an odd list is like |[a,b,c]|.
Again, the integer $a$ is added to the inverse of
what follows in the list.
But this time, if $c$ grows, the inverse of $c$,
$\frac{1}{c}$, shrinks and, as such, the value of
$\frac{1}{b+1/c}$ grows.
Therefore, if the number of elements is even,
the value of $k_1$ is less than the value of the input list
and, if it is odd, then the value is greater.
You can easily convince yourself that, for $k_2$,
this is exactly the other way round.
In consequence, the left list returned by |brocotkids|
is always smaller than the input and the right one is greater.

Using this function, we can now approximate any real number.
The idea is that, if the current continued fraction
results in a number greater than the number in question, we
continue with the left kid; if it is smaller,
we continue with the right kid. Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  approx :: Natural -> RealN -> [Ratio]
  approx i d = go i [0,1] 
    where  go 0 _  = []
           go j r  =  let  (R n d) = contfracr r 
                           d' = (fromIntegral n) / (fromIntegral d)
                           (k1,k2) = brocotkids r
                      in if d' == d  then [R n d]
                                     else if d' < d  then r':go (j-1) k2
                                                     else r':go (j-1) k1
\end{code}
\end{minipage}
 
This function takes two arguments.
The first, a natural number, defines the number of steps we want to do.
The second is the real number we want to approximate.
We start the internal |go| with $i$ and the list |[0,1]|.
In |go|, as long as $j > 0$, 
we compute the rational number that corresponds to the input list;
then we compute the corresponding real number.
If the number we computed this way
equals the input (\ie\ the input is rational),
we are done. Otherwise, if it less then the input, we continue with $k2$;
if it is greater, we continue with $k1$.

The function yields the whole trajectory. 
The last number in the result set is the best approximation.
The result of |approx 10 pi|, for instance is:

\[
1,2,3,4,\frac{7}{2},
\frac{10}{3},
\frac{13}{4},
\frac{16}{5},
\frac{19}{6},
\frac{22}{7}.
\]

The last fraction $\frac{22}{7}$ is approximately
3.142857, which still is a bit away from 3.141592.
We reach 3.1415 with |approx 25 pi|, for which 
the last fraction is $\frac{333}{106}$, which is
3.141509. This way, we can approximate $\pi$ as
close as we wish.

Since, with the |brocotkids| function, we always
create two follow-ups for the input list,
we can easily define a binary tree where, for each node,
$k_1$ is the left subtree and $k_2$ is the right subtree.
This tree, in fact, is well-known and is called 
\term{Stern-Brocot tree} in honour of Achille Brocot and
Moritz Stern, the German number theorist we already know
from the discussion of the Calkin-Wilf tree.

The Stern-Brocot tree can be defined as

\begin{minipage}{\textwidth}
\begin{code}
  type SterBroc = Tree [Ratio]

  sterbroc  :: Zahl -> [Ratio] -> SterBroc
  sterbroc  i r  | i == 0     =  Node r []
                 | otherwise  =  let (k1,k2) = brocotkids r
                                 in Node r [  sterbroc (i-1) k1,
                                              sterbroc (i-1) k2]
\end{code}
\end{minipage}

Ther function |sterbroc| takes an integer argument
to define the number of generations we want to create and
an initial list of |Ratio|. If we have exhausted the number
of generations, we create the final |Node| without kids.
If we start with a negative number, we will generate 
infinitely many generations.
Otherwise, we create the |brocotkids| and continue with
|sterbroc| on $k_1$ and $k_2$.
We can now convert the continued fractions in the nodes
to fractions by |fmap|ing |contfracr| on them. Here is a function
that creates the Stern-Brocot Tree from root node |[0,1]|
labled with fractions:

\begin{minipage}{\textwidth}
\begin{code}
  sterbrocTree :: Int -> Tree Rational
  sterbrocTree i = fmap contfracr (sterbroc i [0,1])
\end{code}
\end{minipage}

For the first five generations, this tree is

\newcommand{\connect}[2]{
  \draw [-,color=black] (#1) to (#2)
}

\begin{center}
\begin{tikzpicture}
% root
\node (A1) at ( 7,  0) {$\frac{1}{1}$};

% first level
\node (A2) at ( 5,-1 ) {$\frac{1}{2}$};
\node (A3) at ( 9,-1 ) {$\frac{2}{1}$};

% kids of A2
\node (A4) at ( 4,-2 ) {$\frac{1}{3}$};
\node (A5) at ( 6,-2 ) {$\frac{2}{3}$};

% kids of A3
\node (A6) at ( 8,-2 ) {$\frac{3}{2}$};
\node (A7) at (10,-2 ) {$\frac{3}{1}$};

% kids of A4
\node (A8) at ( 3  ,-3 ) {$\frac{1}{4}$};
\node (A9) at ( 4.2,-3 ) {$\frac{2}{5}$};

% kids of A5
\node (A10) at ( 5.2,-3 ) {$\frac{3}{5}$};
\node (A11) at ( 6.5,-3 ) {$\frac{3}{4}$};

% kids of A6
\node (A12) at ( 7.5,-3 ) {$\frac{4}{3}$};
\node (A13) at ( 8.5,-3 ) {$\frac{5}{3}$};

% kids of A7
\node (A14) at ( 9.5,-3 ) {$\frac{5}{2}$};
\node (A15) at (11  ,-3 ) {$\frac{4}{1}$};

% kids of A8
\node (A16) at ( 2  ,-4 ) {$\frac{1}{5}$};
\node (A17) at ( 2.6,-4 ) {$\frac{2}{7}$};

% kids of A9
\node (A18) at ( 3.3,-4 ) {$\frac{3}{8}$};
\node (A19) at ( 3.9,-4 ) {$\frac{3}{7}$};

% kids of A10
\node (A20) at ( 4.7,-4 ) {$\frac{4}{7}$};
\node (A21) at ( 5.3,-4 ) {$\frac{5}{8}$};

% kids of A11
\node (A22) at ( 6.1,-4 ) {$\frac{5}{7}$};
\node (A23) at ( 6.7,-4 ) {$\frac{4}{5}$};

% kids of A12
\node (A24) at ( 7.3,-4 ) {$\frac{5}{4}$};
\node (A25) at ( 7.9,-4 ) {$\frac{7}{5}$};

% kids of A13
\node (A26) at ( 8.7,-4 ) {$\frac{8}{5}$};
\node (A27) at ( 9.3,-4 ) {$\frac{7}{4}$};

% kids of A14
\node (A28) at (10.1,-4 ) {$\frac{7}{3}$};
\node (A29) at (10.6,-4 ) {$\frac{8}{3}$};

% kids of A15
\node (A30) at (11.4,-4 ) {$\frac{7}{2}$};
\node (A31) at (12  ,-4 ) {$\frac{5}{1}$};

% connect root
\connect {A1} {A2};
\connect {A1} {A3};

% connect A2
\connect {A2} {A4};
\connect {A2} {A5};

% connect A3
\connect {A3} {A6};
\connect {A3} {A7};

% connect A4
\connect {A4} {A8};
\connect {A4} {A9};

% connect A5
\connect {A5} {A10};
\connect {A5} {A11};

% connect A6
\connect {A6} {A12};
\connect {A6} {A13};

% connect A7
\connect {A7} {A14};
\connect {A7} {A15};

% connect A8
\connect {A8} {A16};
\connect {A8} {A17};

% connect A9
\connect {A9} {A18};
\connect {A9} {A19};

% connect A10
\connect {A10} {A20};
\connect {A10} {A21};

% connect A11
\connect {A11} {A22};
\connect {A11} {A23};

% connect A12
\connect {A12} {A24};
\connect {A12} {A25};

% connect A13
\connect {A13} {A26};
\connect {A13} {A27};

% connect A14
\connect {A14} {A28};
\connect {A14} {A29};

% connect A15
\connect {A15} {A30};
\connect {A15} {A31};

\end{tikzpicture}
\end{center}

%\begin{center}
%\includegraphics[width=\textwidth]{src/c06/sterb7}
%\end{center}

As you can see at once, this tree has many properties
in common with the Calkin-Wilf tree.
First and trivially, the left kid of a node $k$ is less
than $k$ and the right kid of the same node is greater than $k$.
The left-most branch of the tree contains all fractions
with 1 in the numerator like
$\frac{1}{1},
 \frac{1}{2},
 \frac{1}{3},
 \frac{1}{4}$ and so on.
The right-most branch contains the integers 
$\frac{1}{1},
 \frac{2}{1},
 \frac{3}{1},
 \frac{4}{1}$ and so on.

Furthermore, the product of each generation is 1.
For instance,
$\frac{1}{1} = 1$,
$\frac{1}{2} \times \frac{2}{1} = 1$,
$\frac{1}{3} \times \frac{2}{3} \times
 \frac{3}{2} \times \frac{3}{1} = 1$ and so on.
In fact, we see in each generation the same fractions
we would also see in the Calkin-Wilf tree.
The order of the fraction, however, is different.
More precisely, the order of the inner fractions
differs, since, as we have seen, 
the left-most and right-most numbers are the same.

We could hence ask the obvious question:
how can we permute the generations of the Stern-Brocot tree
to obtain the generations of the Calkin-Wilf tree and vice versa?
Let us look at an example.
The $4^{th}$ generation of the Calkin-Wilf tree is\\
|getKids 4 (calWiTree 4 (Q 1 1))|: 

\[
\frac{1}{4},
\frac{4}{3},
\frac{3}{5},
\frac{5}{2},
\frac{2}{5},
\frac{5}{3},
\frac{3}{4},
\frac{4}{1}.
\]

The $4^{th}$ generation of the Stern-Brocot tree is\\
|getKids 4 (sterbroctree 4)|: 

\[
\frac{1}{4},
\frac{2}{5},
\frac{3}{5},
\frac{3}{4},
\frac{4}{3},
\frac{5}{3},
\frac{5}{2},
\frac{4}{1}.
\]

We see that only some fractions changed their place
and the changes are all direct swaps, such that
the second position in the Calkin-Wilf tree changed with
the fifth position and
the fourth position changed with the seventh position.
The other position, the first, third, sixth and eighth,
remain in their place. We could describe this in 
cyclic notation, using indexes from 0 -- 7 
for the eight positions:

\[
(1,4)(3,6).
\]

In other words,
we represent the generations as arrays with indexes 0 -- 7:

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||c||c||c||c||c||c||c||c||}\hline
0             & 1             & 2             & 3             & 4             & 5             & 6             & 7 \\\hline\hline
$\frac{1}{4}$ & $\frac{4}{3}$ & $\frac{3}{5}$ & $\frac{5}{2}$ & $\frac{2}{5}$ & $\frac{5}{3}$ & $\frac{3}{4}$ & $\frac{4}{1}$\\\hline
$\frac{1}{4}$ & $\frac{2}{5}$ & $\frac{3}{5}$ & $\frac{3}{4}$ & $\frac{4}{3}$ & $\frac{5}{3}$ & $\frac{5}{2}$ & $\frac{4}{1}$\\\hline
\end{tabular}
\endgroup
\end{center}

So, what is so special about the indexes 1,3,4 and 6
that distinguishes them from the indexes 0,2,5 and 7?
When we represent these numbers in binary format with
leading zeros, so that all binary numbers have the same length, we have

\begin{center}
\begin{tabular}{||c||c||c||c||c||c||c||c||}\hline
0    & 1   & 2    & 3   & 4   & 5   & 6   & 7 \\\hline\hline
000  & 001 & 010  & 011 & 100 & 101 & 110 & 111\\\hline
\end{tabular}
\end{center}

When we look at the indexes whose fractions do not change,
we see one property that they all have in common:
they are all symmetric. That is, when we reverse the bit strings
we, still, have the same number.
$0 = 000$ reversed is still $000 = 0$;
$2 = 010$ reversed is still $010 = 2$;
$5 = 101$ reversed is still $101 = 5$ and
$7 = 111$ reversed is still $111 = 7$.
$1 = 001$ reversed, however, is $100 = 4$ and vice versa and
$3 = 011$ reversed is $110 = 6$.
This corresponds exactly 
to the permutation $(1,4)(3,6)$
and is an instance of a bit-reversal permutation.



\ignore{
properties:
- bit-reversal permutation
- Stern-like sequence
- Mediants?
}

