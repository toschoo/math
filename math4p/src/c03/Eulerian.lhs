\ignore{
\begin{code}
module Eulerian
where
  import Natural
  import Fact
\end{code}
}

\ignore{$\eulerianOne{n}{k}$}
\ignore{$\eulerianTwo{n}{k}$}
\ignore{$}

When we discussed Haskell, we studied a beautiful
sorting algorithm, Hoare's \term{quicksort}.
We already mentioned that |quicksort|
is not the best algorithm in practice 
and it is this question
to which we are coming back now.

The reason why |quicksort| is not optimal is
that it looks at the world in black and white:
a sequence is either sorted or it is not.
But reality is not like this.
Complete order, that is to say,
a sequence where every element is at its
place according to the order of the data type,
starting with the least element in the sequence
and each element being greater than its predecessor,
is very rare, of course, and usually
an effort is necessary to create complete
order in this sense.
Complete disorder, however, that is
a sequence where no element is at its
place, is quite rare too.
In fact, it is as rare as complete order:
for any sequence of unique elements,
there is exactly one permutation showing
complete order and one permutation
showing complete disorder.
Order and disorder balance each other.

For instance, the sequence $1,2,3,4,5$,
is completely ordered;
the permutation showing complete disorder
would be $5,4,3,2,1$ and that is the original
sequence reversed obeying as such another
kind of order, \viz\ $\le$ instead of $\ge$.
All other permutations are in between.
That is they show some order with a leaning
either to the original sequence or to 
its reverse. For example: $5,2,4,3,1$
is close to the opposite order;
reversed, it would be $1,3,4,2,5$
and close to order.

A sorting algorithm that exploits this
pre-existing order in any input is
\term{mergesort}. The underlying idea
of |mergesort| is to merge two ordered lists.
This can be implemented in Haskell simply as

\begin{minipage}{\textwidth}\begin{code}
  merge :: (Ord a) => [a] -> [a] -> [a]
  merge [] xs  =  xs
  merge xs []  =  xs
  merge (x:xs) (y:ys)  | x <= y     =  x  :  merge xs (y:ys)
                       | otherwise  =  y  :  merge (x:xs) ys

\end{code}\end{minipage}

We first treat the base cases
where one of the lists is empty,
just yielding the respective other list.
Then we compare the heads of the lists.
The smaller one goes to the head of the merged list
and we recurse with what remains.

We now want to use |merge| on an unordered,
that is to say, incompletely ordered list.
Therefore, we split the input into a list
of ordered sublists. 
Ordered sublists are often referred to as |run|s.
The positions where one ordered list ends
and another one begins are called |stepdown|s,
reflecting the idea that the sequence of 
increasing elements is interrupted by stepping
down to a smaller element.
Here is a Haskell function that splits a list
into runs:

\begin{minipage}{\textwidth}\begin{code}
  runs :: (Ord a) => [a] -> [[a]]
  runs []  =  []
  runs xs  =  let (r,zs) = run xs in r : runs zs
    where  run []   =  ([], [])
           run [x]  =  ([x],[])
           run (x:y:zs)  |  x > y      = ([x],y:zs)
                         |  otherwise  = let (r,ys) = run (y:zs) in (x:r,ys)
\end{code}\end{minipage}

The function applied to the empty list
just yields the empty list. 
Applied to a non-empty list is calls the helper function |run|
that returns a tuple consisting of two lists of |a|s. 
The first element is then the head of the list
resulting from |runs| applied to the second list.

The helper function |run| applied on the empty list
yields a tuple of twice the empty list.
Applied to a list containing only one element,
it yield a tuple containing this list and the empty list.
Otherwise, the first two elements of the list, $x$ and $y$, 
are compared.
If $x > y$, $x$ goes to the first list of the resulting tuple,
$y$ goes to the rest of the list.
This is a stepdown: the first element $x$ is greater than 
its successor $y$
and, hence, the natural order of the sequence is violated.
Otherwise, we continue with |y:zs| and insert $x$
as the head of the resulting $r$, the first of the tuple.
This is the case, where the run continues.

Let us look at an example: the permutation
we already used above $1,3,4,2,5$.
When we call |run| for the first time, we have:

|run (1:3:[4,2,5])|

and we enter the |otherwise| alternative with $x=1$:

|run (3:4:[2,5])|.

We again enter |otherwise|, this time $x=3$:

|run (4:2:[5])|.

But this time we have $4 > 2$ and enter
the first branch, that is we yield

|([4],2:[5])|.

Going backwards, we see:

|(3:[4],[2,5])|\\
|(1:[3,4],[2,5])|,

which finally appears as result
of the first call to |run| in |runs|,
which leads to |[1,3,4] : runs [2,5]|.
|[2,5]| is now handled in the same way
and we obtain the overall result |[[1,3,4],[2,5]]|.
This way, thee entire list is split into two runs.

When we look at the other example,
the permutation that was closer to disorder,
there are more runs: $5||24||3||1$,
which is the proper mathematical notation
for the list |[[5],[2,4],[3],[1]]|
consisting of four runs.
If we reverse the list, however,
the number of runs reduces and 
we get the list with only two runs above.

Can we exploit the fact that we can reduce
the number of runs by reverting the list?
Yes, of course, otherwise the question
would not have been asked.
It turns out that, 
for lists with unique elements,
if the number of runs $r$ is greater
than $\frac{n}{2} + 1$, then
$r'$, the number of runs of the reversed list
is less than $r$.
In other words, we could check the number of runs,
before we start to sort, and revert the list
if the number of runs is greater than 
the half of the length of the list plus 1.

Note that in the real world
we often see lists with repeated elements.
The repetitions, however, would not spoil the result,
since repetitions would just reduce
the possible number of runs. In consequence,
it may happen that reverting the list,
even though the number of runs is less
than the half of the list size plus 1, would 
improve performance. But without reverting,
the algorithm is still good, \viz\ comparable
to the performance of a cousin of the same size
without repetitions.

Let us look at an implementation of |mergesort|
that exploits order in the input in this sense.
First, we need a function that merges the runs,
that is a merge for a list of lists.
Let us call this function |multimerge|:

\begin{minipage}{\textwidth}\begin{code}
  multimerge :: (Ord a) => [[a]] -> [a]
  multimerge []        =  []
  multimerge [xs]      =  xs
  multimerge (x:y:zs)  =  merge (merge x y) (multimerge zs)
\end{code}\end{minipage}

The function reduces a list of lists of |a|
to a plain list of |a|.
For the empty list, it yields the empty list.
For a list that contains only one single list,
it yields just that list.
For a list with more elements,
it merges the first two lists 
and merges the resulting list with the list
that results from |multimerging| the rest.
With an example that will become clearer.
But let us not use the list with two runs,
because that would not let us see the recursion
on |multimerge|. Instead, we use the non-optimal
list with four runs.
We would start with:

|multimerge [[5]:[2,4]:[[3],[1]]|

and perform

|merge (merge [5] [2,4]) (multimerge [[3],[1]])|,

which is 

|merge [2,4,5] (multimerge [[3],[1]])|

and results in the call to |multimerge|:

|multimerge [[3],[1]]|.

This reduces to

|merge (merge [3] [1]) (multimerge [])|,

which is

|merge [1,3] []|,

which, in its turn, is |[1,3]|. 
Going backwards, we obtain

|merge [2,4,5] [1,3]|,

which is |[1,2,3,4,5]|.

We can now put everything together:

\begin{minipage}{\textwidth}\begin{code}
mergesort :: (Ord a) => [a] -> [a]
mergesort l =  let  rs  =  runs l 
                    n'  =  length l
                    n   =  if even n' then n' else n'+1
                in  if length rs > (n `div` 2) + 1 
                    then multimerge $ runs (reverse l) 
                    else multimerge rs
\end{code}\end{minipage}
\ignore{$}

We first create the runs for the input list |l|.
We then compute the length of |l| |n'|.
If |n'| is not even, we add one,
just to be sure, we later refer to at least the half of |n'|.
Then, if the length of the runs is more than half of |n| plus 1,
then we run |multimerge| on the runs of the reversed input list,
otherwise, we run |multimerge| on the runs of |l|.

There is a well-known mathematical concept 
that is closely related to the conept of runs:
the \term{Eulerian numbers}, which, as the Stirling numbers,
come in two flavours ingeniously called 
Eulerian numbers of the first kind and
Eulerian numbers of the second kind.

The Eulerian numbers of the first kind,
denoted by $\eulerianOne{n}{m}$, count
the number of permutations of a set of $n$ 
distinct elements where $m$ numbers are greater
than the previous element.
They, hence, do not count the number of runs
directly. 
They, first, use the number of elements 
that are actually member of a run without the first 
element of that run (which is a stepdown).
This, actually, is the value of $m$,
which counts, in other words, how many of the elements
are not stepdowns.
They, second, count the number of permutations
that have $m$ elements in such a configuration.
For instance, the set $\lbrace 1,2,3,4,5\rbrace$
has $5! = 120$ permutations.
There is exactly one permutation where 4 elements
are greater than their predecessor, namely 
the permutation $1,2,3,4,5$.
Hence: $\eulerianOne{5}{4} = 1$.
There is also only one permutation with no element
greater than its predecessor, namely $5,4,3,2,1$.
Hence: $\eulerianOne{5}{0} = 1$.
There are 26 permutations with only 3 elements
greater than their predecessor
and 66 with only 2 elements greater than their
predecessor.

You perhaps see immediately that Eulerian numbers
can be used to compute the average running time
of |mergesort| for input of a given size $n$.
The Eulerian numbers allow us to compute
the probability of that input having
1 run, 2 runs, $\dots$, $n$ runs.
For the example above, we have the probabiliy
$\frac{1}{5! = 120}$ that there is no stepdown at all
and that we, hence, do not have to do anything;
we have the probability $\frac{26}{120} = \frac{13}{60}$
that we have to do only one merge step and
the probability of $\frac{66}{120} = \frac{11}{20}$
that we have to do two merge steps and so on.
(Knuth provides an extensive analysis 
in the third volume of his masterpiece.)

Let us look at a smaller set, where we can actually
look at all permutations, say $\lbrace 1,2,3\rbrace$.
There is of course 1 permutation for 2 non-stepdowns
and also 1 for no non-stepdown at all: 
$\eulerianOne{3}{0} = \eulerianOne{3}{2} = 1$.
For 1 element greater than its predecessor,
there are $\eulerianOne{3}{1} = 4$ possibilities,
namley: $213, 231, 132, 312$.
The sum of all values is of course the number
of all permutations, \ie\ $3! = 6$:
$\eulerianOne{3}{0} + \eulerianOne{3}{1} + \eulerianOne{3}{2} 
= 1 + 4 + 1 = 6$.

Here are some values arranged in -- 
oh, you guessed it already:

\begin{tabular}{l c c c c c c c c c c c c c c c}
1 &   &   &    &    &    &    &     &  1 &     &    &    &    &    &   &   \\
2 &   &   &    &    &    &    &   1 &    &   1 &    &    &    &    &   &   \\
3 &   &   &    &    &    &  1 &     &  4 &     &  1 &    &    &    &   &   \\
4 &   &   &    &    &  1 &    &  11 &    &  11 &    &  1 &    &    &   &   \\
5 &   &   &    &  1 &    & 26 &     & 66 &     & 26 &    &  1 &    &   &   \\
6 &   &   &  1 &    & 57 &    & 302 &    & 302 &    & 57 &    &  1 &   &   \\   
7 &   & 1 &    &120 &    &1191&     &2416&     &1191&    &120 &    & 1 &   
\end{tabular}

Make sure yourself that, for each line $n$, the following identity holds:

\begin{equation}
n! = \sum_{m=0}^{n-1}{\eulerianOne{n}{m}}
\end{equation}

There is a recursive formula 
to compute the Eulerian numbers, which is

\begin{equation}
\eulerianOne{n}{m} = (n-m) \eulerianOne{n-1}{m-1}
                   + (m+1) \eulerianOne{n-1}{m},
\end{equation}

with $\eulerianOne{0}{m} = \eulerianOne{n}{0} = \eulerianOne{n}{n-1} = 1$.
In Haskell, this can be implemented as:

\begin{minipage}{\textwidth}\begin{code}
  eulerian1 :: Natural -> Natural -> Natural
  eulerian1 0 _  =  1
  eulerian1 _ 0  =  1
  eulerian1 n m  | m == n - 1  =  1
                 | otherwise   =   (n-m) * eulerian1 (n-1) (m-1)
                               +   (m+1) * eulerian1 (n-1)  m
\end{code}\end{minipage}

There is also a closed form to compute
the $n$th Eulerian number and this closed form
reveals a relation of the Eulerian numbers
with the binomial coefficients:

\begin{equation}
\eulerianOne{n}{m} = \sum_{k=0}^m{(-1)^k\binom{n+1}{k}(m+1-k)^n}.
\end{equation}

This expands into a series where the results of the products
$\binom{n+1}{k}(m+1-k)^n$ are alternately added or subtracted.
For instance for $\eulerianOne{5}{2}$:

\begin{equation}
\eulerianOne{5}{2} = (-1)^0 \times \binom{6}{0} \times (2+1-0)^5 +  
                     (-1)^1 \times \binom{6}{1} \times (2+1-1)^5 +
                     (-1)^2 \times \binom{6}{2} \times (2+1-2)^5,
\end{equation}

which is 

\begin{align*}
  &&  1  && \times && 3^5 &&   &&             &&=&&  243\\ 
- &&  6  && \times && 2^5 && = && 6 \times 32 &&=&&  192\\
+ && 15  && \times && 1^5 &&   &&             &&=&&   15,
\end{align*}

hence: $243 - 192 + 15 = 66$.

It perhaps helps to get the closed form right
to look at it in Haskell:

\begin{minipage}{\textwidth}\begin{code}
  eu1closed :: Natural -> Natural -> Natural
  eu1closed n m  = go 0
    where go k   =  let  a = (-1)^k
                         b = choose (n+1) k
                         c = (m + 1 - k)^n
                    in (a * b * c) + if k == m  then 0 
                                                else go (k+1)
\end{code}\end{minipage}

The Eulerian numbers of the second kind 
are as well quite interesting. They deal
with \term{multisets}, that is sets with
repeated elements and, as such, they
may pave the way to a theory for real world input.
In concrete, they count the number of permutations
of a multiset with $n$ different elements
with $m$ ascents, where an ascent occurs
whenever a number is greater than its predecessor.
The concept of an ascent, hence, is stronger
than that of a non-stepdown. A non-stepdown
would include a number that equals its predecessor,
but that is not an ascent.

To understand Eulerian numbers of the second kind,
we first have to understand how many permutations
there are for multisets. It is not the factorial,
but the \term{doublefactorial}, also called the
\term{semifactorial}, of $2n-1$, where $n$ is the number of unique
elements in the multiset.
For instance, the multiset $\lbrace 1,1,2,2,3,3\rbrace$
has $n=3$ unique elements, namely 1, 2 and 3.
The doublefactorial of $2n-1 = 5$, denoted by $n!!$, is 15.

The doublefactorial of $n$ is the product of all numbers
$1\dots n$ that have the same parity as $n$.
If $n$ is even, we multiply all even numbers $2\dots n$,
otherwise, if $n$ is odd, we multiply the odd numbers
$1\dots n$. In Haskell, this may look like this:

\begin{minipage}{\textwidth}\begin{code}
  doublefac :: Natural -> Natural
  doublefac 0  = 1
  doublefac 1  = 1
  doublefac n  = n * doublefac (n - 2)
\end{code}\end{minipage}

For instance, $5!! = 1 \times 3 \times 5 = 15$
and $6!! = 2 \times 4 \times 6 = 48$.

The permutations of the multiset $\lbrace 1,1,2,2,3,3\rbrace$ are

332211,\\
221133, 221331, 223311, 233211, 113322, 133221, 331122, 331221,\\
112233, 122133, 112332, 123321, 133122, 122331.

In the first line, there is one permutation
with no ascent,
in the second line, there are 8 permutations
with one ascent and
in the third line, there are 6 permutations
with two ascents.
The Eulerian numbers of the second kind,
denoted by $\eulerianTwo{n}{m}$, for a multiset
with $n=3$ different elements, thus, are:
$\eulerianTwo{3}{0} = 1$,
$\eulerianTwo{3}{1} = 8$ and
$\eulerianTwo{3}{2} = 6$.
Here are some values arranged in 
the last triangle you will see for some time:

\begin{tabular}{l c c c c c c c c c c c c c c}
1 &   &    &    &    &    &     &  1  &     &     &    &     &    &    &   \\
2 &   &    &    &    &    &   1 &     &   2 &     &    &     &    &    &   \\
3 &   &    &    &    &  1 &     &  8  &     &  6  &    &     &    &    &   \\
4 &   &    &    &  1 &    &  22 &     &  58 &     & 24 &     &    &    &   \\
5 &   &    &  1 &    & 52 &     &328  &     &444  &    & 120 &    &    &   \\
6 &   &  1 &    &114 &    &1452 &     &4400 &     &3708&     &720 &    &   \\   
7 & 1 &    &240 &    &5610&     &32120&     &58140&    &33984&    &5040&   
\end{tabular}

Again, this triangle shows some interesting
properties. The sum of each row $n$ is of course
the doublefactorial of $2n-1$, \eg\
$\eulerianTwo{3}{0} + \eulerianTwo{3}{1} + \eulerianTwo{3}{2} = 5!! = 15$ and
$\eulerianTwo{4}{0} + \eulerianTwo{4}{1} + \eulerianTwo{4}{2} + \eulerianTwo{4}{3} =
7!! = 105$.
Neatly, the oughter right-hand diagonal
shows the factorials. The last value of each row,
$\eulerianTwo{n}{n-1}$, hence, is $n!$.

Here comes the formula to compute the Eulerian numbers
of the second kind recursively:

\begin{equation}
\eulerianTwo{n}{m} = (2n-m-1) \eulerianTwo{n-1}{m-1} 
                   + (m+1)    \eulerianTwo{n-1}{m}, 
\end{equation}

with $\eulerianTwo{0}{0} = 1$ and $\eulerianTwo{0}{m} = 0$.
In Haskell this is:

\begin{minipage}{\textwidth}\begin{code}
  eulerian2 :: Natural -> Natural -> Natural
  eulerian2 0 0  = 1
  eulerian2 0 _  = 0
  eulerian2 n m  = (2*n-m-1)  *  eulerian2  (n-1) (m-1)
                   +   (m+1)  *  eulerian2  (n-1)  m
\end{code}\end{minipage}

The Eulerian numbers, as many other things in mathematics,
are named after Leonhard Euler (1707 -- 1783),
a Swiss mathematician and one of the most important
mathematicians of all time.
He is certainly the most important mathematician of his own time,
and, for sure, the most productive one ever.
He turned his family into a kind of math factory
that produced thousands of papers in 
number theory, analysis, mechanics, optics,
astronomy, ballistics and even music theory.
He is also regarded as the founder of graph theory
and topology.
Modern terminology and notation is strongly based
on Euler. He proved many theorems and 
made even more conjectures.
But he was also a great math teacher,
as his \term{Letters to a German Princess}
show in which he lectured on mathematical subjects
to non-mathematicians.
