\ignore{
\begin{code}
module SternBrocot
where
  import Data.List (sort,nub)
  import Data.Tree
  import Cantor1
  import Natural
  import Zahl
  import Quoz
  import Realrep
  import Real
\end{code}
}

Achille Brocot (1817 -- 1878) was part of a clockmaker dynasty
in Paris started by his father and continuing after his death.
The Brocots had a strong emphasis on engineering
and, under the pressure of cheap low-quality imports
mainly from the USA, 
innovated clockmaking with the aim to reduce production cost
without equivalent degradation in quality.
The constant engineering work manifested in a considerable
number of patents hold by family members. The most productive,
in terms of engineering, however, was Achille who improved
many of his father's inventions and developed new ones.
He also introduced a novelty to mathematics, which, surprisingly,
has not only practical, but also theoretical value.

In clockmaking, as in machine construction in general,
determining the ratio of components to each other,
for instance, gear ratios, is a very frequent task.
As often in practice, those ratios are not nice and clean,
but very odd numbers with many decimal digits.
Brocot developed a way to easily approximate such numbers
with arbitrary precision and, in consequence, to approximate
any real number with arbitrary precision.
In the process, he developed yet another way 
to list all rational numbers.

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
They are computed as 
the initial part of the input list,
to which, in the case of $k_1$, one number is appended,
namely the last element of the input list plus 1,
or, in the case of $k_2$, two numbers are appended, namely
the last element minus 1 and 2. 
For the input list |[0,1]|, 
which is just 1,
for instance,
$k_1$ is |[0,2]|, which is $\frac{1}{2}$, and
$k_2$ is |[0,0,2]|, which is $\frac{1}{\frac{1}{2}} = 2$.

When we compare the paritiy of 
the length of the lists,
we see that $k_1$ has the same parity as the input list 
and $k_2$ has the opposite parity.
In particular, if the input list is even,
then $k_1$ is even and $k_2$ is odd; if it is odd,
then $k_1$ is odd and $k_2$ is even.

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
You can easily convince yourself that for $k_2$
this is exactly the other way round.
In consequence, the numerical value of 
the left list returned by |brocotkids|
is always smaller than that of the input list
and that of the right one is greater.

Using this function, we can now approximate any real number.
The idea is that, if the current continued fraction
results in a number greater than the number in question, we
continue with the left kid; if it is smaller,
we continue with the right kid. Here is an implementation:

\begin{minipage}{\textwidth}
\begin{code}
  approx :: Natural -> RealN -> [Ratio]
  approx i d = go i [0,1] 
    where  go :: Natural -> [Ratio] -> [Ratio]
           go 0 _  = []
           go j r  =  let  k@(Q a b) = contfracr r 
                           d' = (fromIntegral a) / (fromIntegral b)
                           (k1,k2) = brocotkids r
                      in if d' == d  then [Q a b]
                                     else if d' < d  then  k:go (j-1) k2
                                                     else  k:go (j-1) k1
\end{code}
\end{minipage}
 
This function takes two arguments.
The first, a natural number, 
defines the number of iterations we want to do.
The second is the real number we want to approximate.
We start the internal |go| with $i$ and the list |[0,1]|.
In |go|, as long as $j > 0$, 
we compute the rational number that corresponds to the input list;
then we compute the corresponding real number.
If the number we computed this way
equals the input (\ie\ the input is rational),
we are done. Otherwise, if it is less than the input, 
we continue with $k_2$;
if it is greater, we continue with $k_1$.

The function yields the whole trajectory whose
last number is the best approximation with $n$ iterations.
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
the last fraction is $\frac{333}{106} = 3.141509$. 
This way, we can come as close to $\pi$ as we wish.

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

The function |sterbroc| takes an integer argument
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
  sterbrocTree :: Zahl -> Tree Ratio
  sterbrocTree i = fmap contfracr (sterbroc i [0,1])
\end{code}
\end{minipage}

For the first five generations of this tree are

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

We see that only some fractions changed their places
and the changes are all direct swaps, such that
the second position in the Calkin-Wilf tree changed with
the fifth position and
the fourth position changed with the seventh position.
The other positions, the first, third, sixth and eighth,
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
\begin{tabular}{||c||c||c||c||c||c||c||c||c||}\hline
             & 0             & 1             & 2             & 3             & 4             & 5             & 6             & 7 \\\hline\hline
Calkin-Wilf  & $\frac{1}{4}$ & $\frac{4}{3}$ & $\frac{3}{5}$ & $\frac{5}{2}$ & $\frac{2}{5}$ & $\frac{5}{3}$ & $\frac{3}{4}$ & $\frac{4}{1}$\\\hline
Stern-Brocot & $\frac{1}{4}$ & $\frac{2}{5}$ & $\frac{3}{5}$ & $\frac{3}{4}$ & $\frac{4}{3}$ & $\frac{5}{3}$ & $\frac{5}{2}$ & $\frac{4}{1}$\\\hline
\end{tabular}
\endgroup
\end{center}

So, what is so special about the indexes 1, 3, 4 and 6
that distinguishes them from the indexes 0, 2, 5 and 7?
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
they are all symmetric. That is, when we reverse the bit strings,
we still have the same number.
$0 = 000$ reversed is still $000 = 0$;
$2 = 010$ reversed is still $010 = 2$;
$5 = 101$ reversed is still $101 = 5$ and
$7 = 111$ reversed is still $111 = 7$.
$1 = 001$ reversed, however, is $100 = 4$ and vice versa and
$3 = 011$ reversed is $110 = 6$.
This corresponds exactly 
to the permutation $(1,4)(3,6)$
and is an instance of a bit-reversal permutation.

Let us try to implement the bit-reversal permutation.
First we implement the bit-reverse of the indexes.
To do so, we first need to convert the decimal index
into a binary number; then we add zeros in front of all
binary numbers that are shorter
than the greatest number; then we simply
reverse the lists of binary digits, remove the leading
zeros and convert back to decimal numbers.
This can be nicely expressed by the function

\begin{minipage}{\textwidth}
\begin{code}
  bitrev :: Int -> Int -> Int
  bitrev x =  fromIntegral . fromBinary       . 
              cleanz .  reverse . fillup x 0  . 
              toBinary . fromIntegral
\end{code}
\end{minipage}

where |fillup| is defined as

\begin{minipage}{\textwidth}
\begin{code}
  fillup :: Int -> Int -> [Int] -> [Int]
  fillup i z is  |  length is == i  = is
                 |  otherwise       = fillup i z (z:is)
\end{code}
\end{minipage}

and |cleanz| as 

\begin{minipage}{\textwidth}
\begin{code}
  cleanz :: [Int] -> [Int]
  cleanz []      = []
  cleanz [0]     = [0]
  cleanz (0:is)  = cleanz is
  cleanz is      = is
\end{code}
\end{minipage}

To apply this, we first have to calculate
the size of the greatest number in our set
in binary format. If we assume that we have
a list of consecutive numbers from $0\dots n-1$,
then the size of the greatest number is just
$\log_2 n$, the binary logarithm of $n$.
For $n=8$, for instance, this is 3.
With this out of the way, we can define
a bit reversal of the indexes of any set |[a]| as:

\begin{minipage}{\textwidth}
\begin{code}
  idxbitrev :: [a] -> [Int]
  idxbitrev xs =  let  l  = fromIntegral $ length xs
                       x  = round $ logBase 2 (fromIntegral l)
                  in  [bitrev x i | i <- [0..l-1]]
\end{code}
\end{minipage}

and use this function to permute the original input list:

\begin{minipage}{\textwidth}
\begin{code}
  bitreverse :: [a] -> [a]
  bitreverse xs = go xs (idxbitrev xs)
    where  go _ []       = []
           go zs (p:ps)  = zs!!p : go zs ps
\end{code}
\end{minipage}

Applied on the list |[0,1,2,3,4,5,6]|,
we see exactly the $(1,4)(3,6)$ permutation
we saw above, namely |[0,4,2,6,1,5,3]|.
Applied on a generation from the Calkin-Wilf tree,
we see the corresponding generation from the 
Stern-Brocot tree. 
Let |t = calWiTree (-1) (1%1)|,
we see for 

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||c||c||}\hline
|getKids 3 t| & 
$\frac{1}{3},
 \frac{3}{2},
 \frac{2}{3},
 \frac{3}{1}$ \\\hline
|bitreverse (getKids 3 t)| &
$\frac{1}{3},
 \frac{2}{3},
 \frac{3}{2},
 \frac{3}{1}$\\\hline
|getKids 4 t| & 
$\frac{1}{4},
 \frac{4}{3},
 \frac{3}{5},
 \frac{5}{2},
 \frac{2}{5},
 \frac{5}{3},
 \frac{3}{4}, 
 \frac{4}{1}$ \\\hline
|bitreverse (getKids 4 t)| &
$\frac{1}{4},
 \frac{2}{5},
 \frac{3}{5},
 \frac{3}{4},
 \frac{4}{3},
 \frac{5}{3},
 \frac{5}{2},
 \frac{4}{1}$\\\hline
\end{tabular}
\endgroup
\end{center}

Since the generations of the Stern-Brocot tree
are nothing but permutations of the Calkin-Wilf tree, we
can derive a sequence from the Stern-Brocot tree
that lists all rational numbers.
We create this sequence in exaclty the same way
we did for the CalkinWilf tree, namely

\begin{minipage}{\textwidth}
\begin{code}
  enumQsb :: [Ratio]
  enumQsb = go 1 $ sterbrocTree (-1) 
    where go i t = getKids i t ++ go (i+1) t
\end{code}
\end{minipage}\ignore{$}

The numerators of the sequence derived in this way
from the Calkin-Wilf tree equal the well-known Stern sequence.
Is there another well-known sequence that is equivalent
to the numerators of the Stern-Brocot tree sequence?
Let us ask the On-line Encyclopedia with the first segment
of that sequence generated by |map numerator (take 20 enumQsb)|:

\[
1,1,2,1,2,3,3,1,2,3,3,4,5,5,4,1,2,3,3,4,5,5,4,5,7.
\]

The Encyclopedia tells us that this is the numerators of
the \term{Farey sequence}.
This sequence, named for British geologist 
John Farey (1766 -- 1826), has a lot of remarkable properties.
The Farey sequence of $n$ lists all fractions 
in canoncial form between 0 and 1,
usually included, with a denominator less or equal than $n$.
For instance, the Farey sequence of 1, designated $F_1$ just contains
$0,1$; $F_2$ contains $0,\frac{1}{2},1$;
$F_3$ contains $0,\frac{1}{3},\frac{2}{3},1$ and so on.

A direct way to implement this could be to combine all numbers
from $0\dots n$ in the numerator with all numbers $1\dots n$
in the denominator that are smaller than 1 and to sort and |nub|
the resulting list, like this:

\begin{minipage}{\textwidth}
\begin{code}
  farey2 :: Natural -> [Ratio]
  farey2 n = sort (nub $  filter (<= 1) $ 
                          concatMap (\x -> map (x%) [1..n]) [0..n])
\end{code}
\end{minipage}

With this approach, we create a lot of fractions
that we do not need and that we filter out again afterwards.
A more interesting approach, also in the light of the topic
of this section, is the following:

\begin{minipage}{\textwidth}
\begin{code}
  farey :: Natural -> [Ratio]
  farey n = 0 : sort (go 1 $ sterbrocTree (-1))
    where  go k t  =  let  g = getKids k t
                           l = filter fltr g
                      in if null l then l else l++go (k+1) t
           fltr k  =  k <= 1 && n >= denominator k 
\end{code}
\end{minipage}\ignore{$}

Here, we iterate over the generations of the Stern-Brocot tree
removing the fractions that are greater than 1 or have a denominator
greater n. When we do not get results anymore, \ie\ all denominators
are greater than $n$, we are done.

Let us try this algorithm on some numbers:

\begin{equation}
F_4 = \left\lbrace 0, 
\frac{1}{4}, 
\frac{1}{3}, 
\frac{1}{2}, 
\frac{2}{3}, 
\frac{3}{4}, 
1\right\rbrace
\end{equation}
\begin{equation}
F_5 = \left\lbrace 0, 
\frac{1}{5}, 
\frac{1}{4}, 
\frac{1}{3}, 
\frac{2}{5}, 
\frac{1}{2}, 
\frac{3}{5}, 
\frac{2}{3}, 
\frac{3}{4}, 
\frac{4}{5}, 
1\right\rbrace
\end{equation}
\begin{equation}
F_6 = \left\lbrace 0, 
\frac{1}{6}, 
\frac{1}{5}, 
\frac{1}{4}, 
\frac{1}{3}, 
\frac{2}{5}, 
\frac{1}{2}, 
\frac{3}{5}, 
\frac{2}{3}, 
\frac{3}{4}, 
\frac{4}{5}, 
\frac{5}{6}, 
1\right\rbrace
\end{equation}

We see some interesting properties.
First and this should be obvious,
we see $n$ as a denominator in sequence $F_n$ exaclty
$\varphi(n)$ times.
For $F_6$, for instance, we could create the fractions
$\frac{1}{6}$, 
$\frac{2}{6}$, 
$\frac{3}{6}$, 
$\frac{4}{6}$ and
$\frac{5}{6}$.
The fractions $\frac{2}{6}\dots\frac{4}{6}$, however,
are not in canonical form, since the numerators $2\dots 4$ 
all share divisors with 6. 
Since there are $\varphi(n)$ numerators
that do not share divisors with $n$,
there are only $\varphi(n)$ fractions less than 1
whith $n$ in the denominator. 

Another property is that, for two consecutive fractions
in the Farey sequence,
$\frac{a}{b}$ and $\frac{c}{d}$,
the cross products
$ad$ and $cb$ are consecutive integers.
In again $F_6$, 
for the fractions $\frac{1}{6}$ and $\frac{1}{5}$,
the cross products, trivially, are 5 and 6.
More interesting are 
the fractions $\frac{3}{5}$ and $\frac{2}{3}$ whose
cross products are $3\times 3 = 9$ and $5 \times 2 = 10$.

Even further, for any three consecutive fractions
in a Farey sequence, the middle one, called the mediant fraction,
can be calculated from the outer ones as
$\frac{a}{b}, \frac{a+c}{b+d}, \frac{c}{d}$.
For instance in $F_6$: 

\begin{equation}
 \frac{1+1}{6+4} = \frac{2}{10} = \frac{1}{5},
\end{equation}
\begin{equation}
 \frac{2+3}{5+5} = \frac{5}{10} = \frac{1}{2}
\end{equation}
and
\begin{equation}
 \frac{3+5}{4+6} = \frac{8}{10} = \frac{4}{5}.
\end{equation}

This property can be used to compute $F_{n+1}$ from $F_n$.
We just have to insert those mediant fractions 
of two consecutive fractions in $F_n$, for which
the denominator is $n+1$.
In $F_6$ we would insert

\[
\frac{0+1}{1+6},
\frac{1+1}{4+3},
\frac{2+1}{5+2},
\frac{1+3}{2+5},
\frac{2+3}{3+4},
\frac{5+1}{6+1}
\]

resulting in

\begin{equation}
F_7 = \left\lbrace 0, 
\frac{1}{7}, 
\frac{1}{6}, 
\frac{1}{5}, 
\frac{1}{4}, 
\frac{2}{7}, 
\frac{1}{3}, 
\frac{2}{5}, 
\frac{3}{7}, 
\frac{1}{2}, 
\frac{4}{7}, 
\frac{3}{5}, 
\frac{2}{3}, 
\frac{5}{7}, 
\frac{3}{4}, 
\frac{4}{5}, 
\frac{5}{6}, 
\frac{6}{7}, 
1\right\rbrace
\end{equation}

We can implement this as

\begin{minipage}{\textwidth}
\begin{code}
  nxtFarey :: Natural -> [Ratio] -> [Ratio]
  nxtFarey n []   = []
  nxtFarey n [r]  = [r]
  nxtFarey n (a:b:rs)  |  denominator a + 
                          denominator b == n  = nxtFarey n (a:x:b:rs)
                       |  otherwise           = a:nxtFarey n (b:rs)
    where x =  let  n1 = numerator a
                    n2 = numerator b
                    d1 = denominator a
                    d2 = denominator b
               in  (n1+n2) % (d1+d2)

\end{code}
\end{minipage}
 
In fact, we can construct the Stern-Brocot tree
by means of mediant fractions. The outer fractions,
in this algorithm are the predecessors of the current node,
namely the direct predecessor and either the predecessor
of the predecessor or the sibling of that node.
For instance, the second node in the third generation
is $\frac{2}{3}$. Its kids are 
$\frac{3}{5}$ and $\frac{3}{4}$.
$\frac{3}{5}$ is $\frac{2+1}{3+2}$ and, thus,
the sum of $\frac{2}{3}$ and its predecessor;
$\frac{4}{3}$, however, is $\frac{2+1}{3+1}$
and, hence, the sum of the $\frac{2}{3}$ 
and the predecessor of its predecessor.

The question now is how to bootstrap this algorithm,
since the root node does not have predecessors.
For this case, we imagine two predecessors, namely
the fractions $\frac{0}{1}$ and $\frac{1}{0}$,
the latter of which, of course, is not a proper fraction.
The assumption of such nodes, however, helps us derive
the outer branches, where, on the left side, the numerator
does not change, hence is constructed by addition with 0, and,
on the right side, the denominator does not change and is
likewise constructed by addition with 0.

We implement this as

\begin{minipage}{\textwidth}
\begin{code}
  mSterbroctree :: Zahl ->  Natural -> Natural -> 
                            Natural -> Natural -> Ratio -> Tree Ratio
  mSterbroctree 0 _ _ _ _ r  =  Node r []
  mSterbroctree n a b c d r  =  let  rn  = numerator r
                                     rd  = denominator r
                                     k1  = (a+rn) % (b+rd)
                                     k2  = (c+rn) % (d+rd)
                                in   if k1 < k2
                                     then  Node r [  mSterbroctree (n-1) a b rn rd k1,
                                                     mSterbroctree (n-1) c d rn rd k2]
                                     else  Node r [  mSterbroctree (n-1) c d rn rd k2,
                                                     mSterbroctree (n-1) a b rn rd k1]
\end{code}
\end{minipage}

Note that we have to use two pairs of natural numbers
instead of two fractions to encode the predecessors.
This is because we have to represent the imagined
predecessor $\frac{1}{0}$, which is not a proper fraction.
Finally, we check for the smaller of the resulting numbers
$k_1$ and $k_2$ to make sure that the smaller one 
always goes to the left and the greater to the right.
This implementation now gives exactly the same tree
as the implementation using continued fractions
introduced at the beginning of the section.

