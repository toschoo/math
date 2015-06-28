\ignore{
\begin{code}
module Stirling
where
  import Data.List (nub)
  import Natural
  import Fact
  import Types
\end{code}
}

We saw that the number of all permutations of a set 
equals the factorial of the number of elements in that set.
We also looked at the cycle notation where permutations
are encoded as results of subsequent
applications of this permutation; $(1~2~5)(3~4)$, for instance,
applied once to the sequence $(1~2~3~4~5)$, would yield $(2~5~4~3~1)$.
It is now quite natural to ask -- at least for a mathematician --
how many permutations there are for a given number of orbits
in cycle notation.

To answer this question,
we first have to know how many different combinations 
of a given number of orbits we actually may have.
The orbits in cycle notation, in fact, are \term{partitions}
of a set. Partitions are non-empty, distinct subsets.
The union of the partitions of a 
complete partitioning of a set is just the original set.
The orbits of the permutation given above, for instance,
$(1~2~5)(3~4)$ can be seen as subsets that,
obviously, are not empty and, 
since they have no element in common,
are distinct.
Their union $\lbrace 1,2,5\rbrace \cup \lbrace 3,4\rbrace$,
as you can easily verify, equals
the original set $\lbrace 1,2,3,4,5\rbrace$.

We could, hence, think in the lines of the powerset
to generate partitions. We just leave out the empty set
and, eventually, pick only groups of sets that, together,
add up to the whole set.
It is in fact somewhat more complicated.
To illustrate that let us look at an
algorithm that generates all possibilities to
partition a set into two partitions:

\begin{code}
  twoPartitions :: (Eq a) => [a] -> [([a],[a])]
  twoPartitions  =  fltr . p2
    where  p2 []      =     [([],[])]
           p2 (x:xs)  =     [(x:a,b) | (a,b) <- p2 xs] ++
                      fltr  [(a,x:b) | (a,b) <- p2 xs]
           fltr   =  filter (\p -> not (  null (fst p) ||
                                          null (snd p)))
\end{code}

This innocent looking lines of code are quite tricky.
The basic idea is implemented in |p2|:
For an empty set, |p2| returns a pair of empty sets.
For any other set, it applies |p2| twice on the |tail| of the list
and adds the |head| once to the first of the pair
and once to the second of the pair.
This sounds easy, but there is an issue:
The intermediate result sets will contain empty sets.
In the first result, the second set is empty and,
in the second result, the first one is empty.
To solve this problem, we explicitly filter empty sets out
(using |fltr|).
If we applied the filter once on the overall result,
we would get the following pairs
for the set $\lbrace 1,2,3\rbrace$: 

|([1,2],[3])|\\
|([1,3],[2])|\\
|([1],[2,3])|\\
|([2,3],[1])|\\
|([2],[1,3])|\\
|([3],[1,2])|.

All results are correct,
but there are too many of them.
More specifically, some of the results are repeated.
|([1,2],[3])| and |([3],[1,2])| are different tuples of course,
but they describe the same partitioning
consisting of the subsets 
$\lbrace 1,2\rbrace$ and $\lbrace 3\rbrace$.

In the code above, this issue is solved
by applying the filter once again, \viz\
on the second intermediate result.
Let us look at how the second list comprehension develops.
After the first application of |p2|,
we have a tuple of two empty sets:

|p2 (3:[])  =  [(a,3:b) || (a,b) <- [([],[])]]|\\
|p2 (3:[])  =  [([],[3])]|.

This result is filtered out, because the first set is empty.
In the next round we consequently have, as input, 
only the result of the first, unfiltered, list comprehension:

|p2 (2:[3])  =  [(a,2:b) || (a,b) <- [([3],[])]]|\\
|p2 (2:[3])  =  [([3],[2])]|,\\

which is preserved.
We then get to the final round
where the input is now the result of the first comprehension
plus the one created above:

|p2 (1:[2,3])  =  [(a,1:b) || (a,b) <- [([3],[2]), ([2,3],[])]|\\
|p2 (1:[2,3])  =  [([3],[1,2]), ([2,3],[1])]|.

The result of the first comprehension in the last round consists of the pairs:
|([1,3],[2])|, which results from the input |([3],[2])|, and
|([1,2,3],[])|, which is removed by the filter on the final result of |p2|.
This gives the correct result: 

|([1,3],[2])|\\
|([2,3],[1])|\\
|([3], [1,2])|.

As long as we create only two partitions,
we can use pairs and the 
nice list comprehension to make the code clear.
For the generation of $k$ partitions,
the code becomes somewhat more obscure.
It is in particulare not sufficient anymore
to call the filter twice.
Instead, we need an additional function
that removes the permutations of sets of partitions.
To partition the set $\lbrace 1,2,3,4\rbrace$
into three partitions, for example,
we need to remove all but one of

$\lbrace 1,2\rbrace, \lbrace 3\rbrace, \lbrace 4\rbrace$,\\ 
$\lbrace 1,2\rbrace, \lbrace 4\rbrace, \lbrace 3\rbrace$,\\
$\lbrace 4\rbrace, \lbrace 1,2\rbrace, \lbrace 3\rbrace$,\\
$\lbrace 3\rbrace, \lbrace 1,2\rbrace, \lbrace 4\rbrace$,\\
$\lbrace 4\rbrace, \lbrace 3\rbrace, \lbrace 1,2\rbrace$ and\\
$\lbrace 3\rbrace, \lbrace 4\rbrace, \lbrace 1,2\rbrace$.

We will not develop this algorithm here,
because it is highly inefficient.
We would create much more sets than necessary
and, then, we still have to generate permutations
to remove those sets that are superfluous.
Fortunately, there is an alternative
very similar to that we have used to create powersets.
For powersets, we used all binary numbers from 0 to $2^n-1$,
where $n$ is the number of elements in the set.
To generate partitions, we have to modify this idea
in two respects.

First, we do not have the simple decision
whether an element is in the current subset or not,
but in which of $k$ partitions it is.
To get this information, we need a number system
with the base $k$.
For two partitions, this is just the binary number system.
For three partitions, it would be a number system with base 3.
For four partitions, we would use a number system with base 4 
and so on.

Second, we have to restrict the numbers in a way
that the result set points only to distinct partitionings.
Imagine we want to know how to partition 
the set $\lbrace 1,2,3,4,5\rbrace$ into three subsets.
We would then need numbers of the form:
$01200$ or $01002$.
The first number, $01200$, would point to the partitions
$\lbrace 1,4,5\rbrace, \lbrace 2\rbrace, \lbrace 3\rbrace$ and
the second number, $01002$, would point to the partitions
$\lbrace 1,3,4\rbrace, \lbrace 2\rbrace, \lbrace 5\rbrace$.
In other words, the digits of the number indicate
the partition in which the element at this position
(in the original sequence) would be starting to count at
index 0 for the first partition.
Obviously, the number $01000$ would be no good,
because it does not describe a set of three partitions,
but only one of two partitions.
Also, with number $00012$ already in the result,
we do not want to generate the number $22210$,
because the corresponding sequences of partitions
$\lbrace 1,2,3\rbrace, \lbrace 4\rbrace, \lbrace 5\rbrace$ and
$\lbrace 5\rbrace, \lbrace 4\rbrace, \lbrace 1,2,3\rbrace$ 
are permutations of each other and, thus, describe 
the same set of partitions.

There is a simple trick to avoid such duplications
and this trick has a name: \term{Restricted Growth String}s,
\acronym{rgs} for short.
\acronym{rgs} are similar to numbers, but have leading zeros --
they are therefore strings rather than proper numbers.
The length of \acronym{rgs} depends on the purpose
for which they are used. In our case,
we want their length to equal the number of elements
in the original set.

When counted up, \acronym{rgs} grow in an ordered fashion,
such that lesser digits appear before greater ones.
For instance, we allow numbers like $0012$ and $0102$,
but do not allow such like $0201$ or $1002$.
This implies that each new digit is at most one greater
than the greatest digit already in the number, \ie\
$a_i \le 1 + \max{a_1,a_2,...a_{i-1}}$.
This restriction rules out numbers with a combination of digits
that has already appeared with smaller \acronym{rgs} before.
Of the strings
$0123$, $0132$, $0213$, $0231$, $0312$ and $0321$
only the first is a valid \acronym{rgs}.
With the others, either 3 or 2 appear
before 1, violating the restriction that no digit
must be greater than the greatest number appeared so far
plus 1. You can easily verify that
all those strings point to the same partitioning
of set $\lbrace 1,2,3,4\rbrace$, namely permutations
of the set
$\lbrace\lbrace 1\rbrace,
        \lbrace 2\rbrace,
        \lbrace 3\rbrace,
        \lbrace 4\rbrace\rbrace$.

The ordered growth also implies 
that the first digit in an \acronym{rgs} is always 0.
Otherwise, if 0 did not appear in the string at all,
the first partition would be empty and
the partitioning would, hence, be invalid;
if 0 did appear later in the string,
the string would not be ordered, \ie\
a greater number would appear 
before the smallest possible number |zero|.

To be sure that the \acronym{rgs}-technique effectively
avoids duplication of subset by suppressing permuations,
we should at least sketch a proof of the concept.
We should prove that restricted growth makes 
complementing groups of digits impossible, such
that all digits $k_1$ and $k_2$ swap their positions
from one string to the other.
The following diagram shows four positions
in a string where, at positions $i$ and $i+1$,
there is the digit $k_1$ and, at positions $j$ and $j+1$,
there is the digit $k_2$:

\begin{tabular}{c||c||c||c||c||c||c}
$\dots$ & $i$   & $i+1$ & $\dots$ & $j$   & $j+1$ & $\dots$\\\hline
$\dots$ & $k_1$ & $k_1$ & $\dots$ & $k_2$ & $k_2$ & $\dots$
\end{tabular}

We assume that $j > i+1$ and we assume that all occurences of $k_1$ and $k_2$
in the string are shown.
In other words, this partial string shows the partitions
$k_1 = \lbrace i,i+1\rbrace$ and
$k_2 = \lbrace j,j+1\rbrace$.

We prove by contradiction on restricted growth and assume
that this string is possible with both cases,
$k_1 < k_2$ or, alternatively, $k_2 < k_1$.
Consider the case $k_2 < k_1$.
In this case $k_2$ must appear in a position $p < i$,
otherwise, ordering would be violated. 
But this contradicts the assumption that $k_2 = \lbrace j,j+1\rbrace$.
So, either we violate ordering or $k_2$ is not shown completely
in the diagram above and, then, the subsets are not complementing. 
Ordering is violated because it implies that any digit $a_i$ in the string
is at most $1 + \max{a_1, a_2, \dots, a_{i-1}}$.
$i$ is either 0, then $k_1$, per definition, is 0 as well
and no (natural) number is less than 0, 
hence $k_2$ cannot be less than $k_1$;
or $i$ is not 0, then there must be a digit $k_0 = k_1 - 1$.
If we assume that $k_2 < k_1$, we must assume that 
$k_1 - 1 < k_2 < k_1$ and, hence, that $0 < k2 < 1$.
But that cannot be, 
since $k_2$ is still a natural number.$\qed$

To implement the \acronym{rgs} analogy,
we first need a function that converts decimal numbers
into numbers with base $b$.
We have already looked at such functions,
for $b = 2$ in the previous section,
which we called |binExp|, and, for $b=10$,
in the previous chapter in the context of
the conversion function |integer2Num|.
|binExp| was tailored for binary numbers,
since it yielded only the positions 
where the binary result would have a 1.
That information is obviously not sufficient
for number systems with $b > 2$, where the decision
which number to put at a given position is not binary.

Let us recall how we converted integer to 
our natural number type.
We divided the number by 10,
collecting the remainders and continuing on the quotient,
like in the following example:

|1000  `quotRem` 10 = (100,0)|\\
|100   `quotRem` 10 = (10,0)|\\
|10    `quotRem` 10 = (1,0)|\\
|1     `quotRem` 10 = (0,1)|.

Now, the remainders of the subsequent divisions
bottom-up would read $1,0,0,0$, which are just 
the components of the decimal representation 
of the number \num{1000}.
If we do this with $b=2$, we would see:

|1000 `quotRem` 2 = (500,0)|\\
|500  `quotRem` 2 = (250,0)|\\
|250  `quotRem` 2 = (125,0)|\\
|125  `quotRem` 2 = (62,1)|\\
|62   `quotRem` 2 = (31,0)|\\
|31   `quotRem` 2 = (15,1)|\\
|15   `quotRem` 2 = (7,1)|\\
|7    `quotRem` 2 = (3,1)|\\
|3    `quotRem` 2 = (1,1)|\\
|1    `quotRem` 2 = (0,1)|.

\num{1000}, in the binary system, hence, 
is $1111101000$, the same result we have already obtained
in the previous section.
We can implement this procedure in Haskell as:\footnote{
We use |Int| instead of |Natural| here, because,
in the following, we will need list functions
like |length| or |take| quite often;
with |Natural|, we would have to add a lot of conversions,
which is much harder to read.}

\begin{code}
  toBaseN :: Int -> Int -> [Int]
  toBaseN b = reverse . go 
    where go x =  case x `quotRem` b of
                  (0,r) -> [r]
                  (q,r) -> r : go q
\end{code}

The |go|-part of this function applied to base $b = 3$
and, say, \num{1024} would develop
as follows:

|go 1024  =  1024 `quotRem` 3|\\
|1 : go 341 = 341 `quotRem` 3|\\
|1 : 2 : go 113 = 113 `quotRem` 3|\\
|1 : 2 : 2 : go 37 = 37 `quotRem` 3|\\
|1 : 2 : 2 : 1 : go 12 = 12 `quotRem` 3|\\
|1 : 2 : 2 : 1 : 0 : go 4 = 4 `quotRem` 3|\\
|1 : 2 : 2 : 1 : 0 : 1 : go 1 = [1]|\\
|1 : 2 : 2 : 1 : 0 : 1 : 1|,

which, reversed, is |[1,1,0,1,2,2,1]|
and the correct representation of \num{1000}
in the ternary system.

Now we need some functions to convert 
the number given in the $b$-ary system
into an \acronym{rgs}. 
First we fill the number with leading zeros
until it has the desired size $n$:

\begin{code}
  rgs :: Int -> Int -> Int -> [Int]
  rgs b n i =  let  r  =  toBaseN b i
                    d  =  n - length r 
                    p  =  if d > 0 then take d (repeat 0) else []
               in if d < 0 then [] else p ++ r
\end{code}

Note that, if the length of the result of |toBaseN| exceeds $n$,
the function yields the empty list.
This, in fact, is an error case that could be handled explicitly.
On the other hand, returning the empty list
is a good enough indication for an error
and we could check for this error in code
using |rgs| later.

We now define a wrapper around this conversion function
to apply the restrictions:

\begin{code}
  toRgs :: ([Int] -> Bool) -> 
           Int -> Int -> Int -> (Int, [Int])
  toRgs rst b n i = go (rgs b n i)
    where go r  |  not (rst r)  =  toRgs rst b n (i + 1)
                |  otherwise    =  (i,r)
\end{code}

This function converts a decimal number
to an \acronym{rgs} and checks if the result
obeys the restrictions, which are passed in as a boolean function.
If it does not, then the input is incremented by one
and the function is called again.
Otherwise, the function yields a tuple consisting 
of the decimal number that we have eventually reached
and the \acronym{rgs}.

We define the growth restriction as follows:

\begin{code}
  rGrowth :: [Int] -> Bool
  rGrowth  []      =  True
  rGrowth  (x:xs)  =  go x xs
    where  go _  []      =  True
           go d  (z:zs)  =  if z - d > 1 then False
                            else  let d' = max d z in go d' zs
\end{code}

Since we want to see
only partitionings with $b$ subsets,
we, still, need another restriction.
We do not want to see  \acronym{rgs} of the form
$01000$, when we ask for three partitions, or
$01230$, when we ask for five.
For this end, we need the restriction
that there must be $b$ different digits
in the resulting \acronym{rgs}.
The restriction is easily implemented as:

\begin{code}
  hasN :: Int -> [Int] -> Bool
  hasN b r = length (nub r) == b
\end{code}

We apply this restrictions in yet another wrapper 
to call |toRgs|:

\begin{code}
  toRgsN :: Int -> Int -> Int -> (Int, [Int])
  toRgsN b = toRgs rst b
     where rst r = rGrowth r  && hasN  b r
\end{code}

Finally, we can implement a loop
that counts \acronym{rgs} up from 1 to the last number
with leading 0:

\begin{code}
  countRgs :: Int -> Int -> [[Int]]
  countRgs 1 n  =  [rgs 1 n 1]
  countRgs b n  |  b == n     =  [[1..n-1]]
                |  b >  n     =  []
                |  otherwise  =  go 1
    where go i  =  let (j,r) = toRgsN b n i 
                   in if head r /= 0 then [] else r : go (j+1)
\end{code} 

Note that we jump over numbers that do not obey
the restrictions: we continue always with |go| applied to $j$, 
the first return value of |toRgsN|.
$j$ does not necessarily equal $i$;
it depends on how many numbers have been ignored
by |toRgs| because they did not obey the restrictions.
When we call countRgs on 3, the numbers of partitions we want to have,
and 4, the number of elements in the original set,
we get the following \acronym{rgs}:

|[0,0,1,2]|\\
|[0,1,0,2]|\\
|[0,1,1,2]|\\
|[0,1,2,0]|\\
|[0,1,2,1]|\\
|[0,1,2,2]|,

which correspond to the partitionings of $\lbrace 1,2,3,4\rbrace$:

$\lbrace 1,2\rbrace, \lbrace 3  \rbrace, \lbrace 4  \rbrace$\\
$\lbrace 1,3\rbrace, \lbrace 2  \rbrace, \lbrace 4  \rbrace$\\
$\lbrace 1  \rbrace, \lbrace 2,3\rbrace, \lbrace 4  \rbrace$\\
$\lbrace 1,4\rbrace, \lbrace 2  \rbrace, \lbrace 4  \rbrace$\\
$\lbrace 1  \rbrace, \lbrace 2,4\rbrace, \lbrace 3  \rbrace$\\
$\lbrace 1  \rbrace, \lbrace 2  \rbrace, \lbrace 3,4\rbrace$.

This analogy between \acronym{rgs} and partitions is implemented as:

\begin{code}
  rgs2set :: (Eq a) => [Int] -> [a] -> [[a]] -> [[a]]
  rgs2set [] _  ps           =  ps
  rgs2set _  [] ps           =  ps
  rgs2set (r:rs) (x:xs) ps   =  rgs2set rs xs (ins r x ps)
    where  ins _ _ []        =  undefined
           ins 0 p (z:zs)    =  (p:z) : zs
           ins i p (z:zs)    =  z : ins (i-1) p zs 
\end{code}

The function receives three arguments:
The \acronym{rgs}, the original set we want to partition
and the result set, which initially 
should contain $k$ empty lists
with $k$ the number of partitions we want to obtain. 
(This pre-condition, actually, is not enforced in this code.)
Note that the logic of using $k$ empty lists is very similar 
to the trick we used in |twoParatitions| above.

When we have exhausted either the \acronym{rgs} or
the original set, the result is just $ps$,
the result set we passed in.
Otherwise, we recurse with the tails 
of the \acronym{rgs} and the set
(this way establishing the analogy)
inserting the element of the set that corresponds
to the current position of the \acronym{rgs}
to the partition that, in its turn, corresponds
to the digit of the \acronym{rgs} at this position.
If the digit is 0, we just insert the element into the first list,
otherwise we recurse (on |ins|) 
decrementing the digit by 1.
Note that |ins| is |undefined| for the case
that the result set is empty before we reach 0.
This, obviously, would hint to an erroneous \acronym{rgs}
and, hence, to a coding error.

We now can put everything together:

\begin{code}
  nPartitions :: (Eq a) => Int -> [a] -> [[[a]]]
  nPartitions _ []  =  []
  nPartitions 1 xs  =  [[xs]]
  nPartitions k xs   |  k >= length xs  =  [[[x] | x <- xs]]
                     |  otherwise       =  go (countRgs k (length xs))
    where  go []      =  []
           go (r:rs)  =  rgs2set r xs (take k (repeat [])) : go rs
\end{code}

The function |nPartitions| receives 
the number of partitions we would like to have, $k$,
and the original set, |xs|.
If the set is empty, the result is empty, too.
If we just want one partition, we return the set as its only partition.
If $k$ equals or exceeds the size of the set,
we just return each element in its own set.
(We could return an error for the case that $k$ exceeds the size
of the set, but, for sake of simplicity, we allow this case, 
returning an incomplete result.)
Otherwise, we call |countRgs|
and apply |rgs2set| to all elements of the result.
The set of empty lists we need to start with |rgs2set|
is created by |repeat []| which creates an infinite list
containing the empty list -- |[[],[],[],...]| --
from which we just take $k$, \ie\ the number of partitions.
The call |nPartitions 2 [1,2,3]| would
yield the expected result of all possibilities
to partition |[1,2,3]| in 2 subsets:

|[2,1],[3]|\\
|[3,1],[2]|\\
|[1],[3,2]|.

This result corresponds to the \acronym{rgs}:

$001$,\\
$010$ and \\
$011$.

The order of elements within partitions is explained by the fact
that |ins| (in |rgs2set|) adds new elements using ``|:|'' --
the element that was first inserted into the list
is therefore the last one in the resulting partition.

It should be noted that, as for the powerset,
we can optimise this code by using other data structures than lists.
Since, as for the powerset as well, 
the number of possible partitionings of huge sets
is incredibly large, it is not feasible
to computate all partitionings of great sets anyway.
We, therefore, leave it with a non-optimal implementation.

Now, that we have arrived here,
the mathematically natural question occurs
of how many partitions there are for a set of size $n$.
Well, we have a tool to try that out.
The following -- \speech{er} -- triangle 
shows results of calls of |nPartitions|.
Each line corresponds to the call 
|[length (nPartitions k [1..n]) || k <- [1..n]]|,
where $n$ starts with 1 at the top of the triangle
and is counted up until 7 at its bottom.

\begin{tabular}{l c c c c c c c c c c c c c c c c c c c c}
1 &   &   &   &   &    &    &    &     &     &   1 &     &     &    &    &    &   &   &   &   &  \\
2 &   &   &   &   &    &    &    &     &   1 &     &   1 &     &    &    &    &   &   &   &   &  \\
3 &   &   &   &   &    &    &    &   1 &     &   3 &     &   1 &    &    &    &   &   &   &   &  \\
4 &   &   &   &   &    &    &  1 &     &   7 &     &   6 &     &  1 &    &    &   &   &   &   &  \\
5 &   &   &   &   &    &  1 &    &  15 &     &  25 &     &  10 &    &  1 &    &   &   &   &   &  \\
6 &   &   &   &   &  1 &    & 31 &     &  90 &     &  65 &     & 15 &    &  1 &   &   &   &   &  \\   
7 &   &   &   & 1 &    & 63 &    & 301 &     & 350 &     & 140 &    & 21 &    & 1 &   &   &   &  
\end{tabular}

On the first sight, the values in this triangle
besides the ones in the outer diagonals appear less regular
than those in Pascal's nice and tidy triangle.
Nevertheless, already the second sight reveals some curious relations.
The second diagonal from top-right to left-bottom,
which reads 1,3,7,15,31,63, corresponds to the values $2^n-1$.
The second diagonal from top-left to right-bottom 
shows other numbers we already know, namely 1,3,6,10,15,21.
If you take the differences, you will observe that each
number is the sum of $n$ and its predecessor.
In other words, this diagonal contains the sum of all numbers
from 1 to $n$, where $n$ is the line number.

The triangle overall shows the \term{Stirling set numbers},
also known as the \term{Stirling numbers of the second kind},
which are denoted by

\begin{equation}
  S(n,k) = \stirlingTwo{n}{k}.
\end{equation}

The formula to compute Stirling numbers remarkably
resembles Pascal's rule:

\begin{equation}
\stirlingTwo{n}{k} = \begin{cases}
                       0 & \textrm{if $n = 0$}\\
                       1 & \textrm{if $n = 1$}\\
                       k \times \stirlingTwo{n-1}{k} + 
                                \stirlingTwo{n-1}{k-1} &
                         \textrm{otherwise}.
                   \end{cases}
\end{equation}

This translates into Haskell as:

\begin{code}
  stirling2 :: Natural -> Natural -> Natural
  stirling2 0 _  =  0
  stirling2 _ 0  =  0
  stirling2 1 1  =  1
  stirling2 n k  |  k > n      =  0
                 |  otherwise  =  k *  (stirling2 (n-1) k) +
                                       (stirling2 (n-1) (k-1))
\end{code}

The code is almost a one-to-one translation
of the mathematical formulation.
However, there is an extra line, namely
the base case |stirling2 _ 0 = 0|.
This base case must be introduced to avoid
that we go below zero for $k$ with the rule part
|stirling2 (n-1) (k-1)|.
The natural numbers are not defined for the range
less than zero and so we have to stop here explicitly.

The sum of all Stirling numbers of the same row,
\ie\ $\sum_{k=1}^{n}{\stirlingTwo{n}{k}}$,
is called the \term{Bell number} of $n$.
The first 7 Bell numbers are: 1, 2, 5, 15, 52, 203, 877.

Since Stirling numbers of the second kind count the ways
to partition a set into a given number of subsets,
Bell numbers indicate all ways to partition a set,
not restricting the number of subsets we want to obtain.
The following table shows this relation 
for the set $\lbrace 1,2,3,4\rbrace$:

\bgroup
\renewcommand{\arraystretch}{1.3}
\begin{center}
\begin{tabular}{||c||c||c||c||}\hline
$\stirlingTwo{4}{1} = 1$ & $\stirlingTwo{4}{2} = 7$ & 
$\stirlingTwo{4}{3} = 6$ &  $\stirlingTwo{4}{4} = 1$\\\hline\hline
% -- 1st row -------------------------------------------------------------------------------------
$\lbrace 1,2,3,4\rbrace$ & 
$\lbrace 1,2,3\rbrace, \lbrace 4\rbrace$ & 
$\lbrace 1,2\rbrace, \lbrace 3\rbrace, \lbrace 4\rbrace$ & 
$\lbrace 1\rbrace, \lbrace 2\rbrace, \lbrace 3\rbrace, \lbrace 4\rbrace$ \\\cline{2-3}
% -- 2nd row -------------------------------------------------------------------------------------
 & 
$\lbrace 1,2,4\rbrace, \lbrace 3\rbrace$ & 
$\lbrace 1,3\rbrace, \lbrace 2\rbrace, \lbrace 4\rbrace$ & \\\cline{2-3}
% -- 3rd row -------------------------------------------------------------------------------------
 & 
$\lbrace 1,3,4\rbrace, \lbrace 2\rbrace$ & 
$\lbrace 1,4\rbrace, \lbrace 2\rbrace, \lbrace 3\rbrace$ & \\\cline{2-3}
% -- 4th row -------------------------------------------------------------------------------------
 & 
$\lbrace 2,3,4\rbrace, \lbrace 1\rbrace$ & 
$\lbrace 1\rbrace, \lbrace 2,3\rbrace, \lbrace 4\rbrace$ & \\\cline{2-3}
% -- 5th row -------------------------------------------------------------------------------------
 & 
$\lbrace 1,2\rbrace, \lbrace 3,4\rbrace$ & 
$\lbrace 1\rbrace, \lbrace 2,4\rbrace, \lbrace 3\rbrace$ & \\\cline{2-3}
% -- 6th row -------------------------------------------------------------------------------------
 & 
$\lbrace 1,3\rbrace, \lbrace 2,4\rbrace$ & 
$\lbrace 1\rbrace, \lbrace 2\rbrace, \lbrace 3,4\rbrace$ &  \\\cline{2-3}
% -- 7th row -------------------------------------------------------------------------------------
 & 
$\lbrace 1,4\rbrace, \lbrace 2,3\rbrace$ &  & \\\hline
\end{tabular}
\end{center}
\egroup

If you count the partitionings in one column,
you get the Stirling number for that column;
the number of all partitions in all columns
equals the Bell number of 4, which is 15.

The inventors -- or discoverers, depending on 
your philosophical view -- of Bell numbers and Stirling numbers
are two very interesting characters.
Eric Temple Bell (1883 -- 1960) was professor of mathematics
in the United States for most of his life.
But he was also an early science fiction writer
and a math historian.
His science fiction reached a higher level of science
than most other publications in this genre at his time,
but was often critised as poorly written and, in particular,
for its weak characterisation of protagonists.
His contributions to math history were even more fiercely critised as
fictitious and romantic (as in the case of his biographical sketch
of Évarist Galois) or as stereotypical (as in the case
of his description of the life of Georg Cantor).

James Stirling (1692 -- 1770) was from Scotland.
He studied and taught in Oxford for some years, but had to
flee from England, when he was accused of conspiracy
based on his correspondence with Jacobites, 
that is supporters of the catholic kings, 
in particular James II who was deposed in 1688.
After ten years of exile in Venice,
he started to fear for his life again, because
he discovered a trade secret of the glassmakers of Venice
and returned to England with the help of his friend
Isaac Newton. Much of Stirling's work
is in fact tightly coupled with that of Newton.
Stirling very much promoted Newton's discoveries and methods,
for instance
in his book \term{Methodus differentialis}.
During the last years of his life,
he was manager of the Scots Mining Company.
During this period, he published mainly on topics
of applied mathematics.

But let us return to the intial question.
We were investigating the possible permutations
with a given number of orbits in the cycle notation
and have just learnt how to generate all possible $k$ orbits
of a set with $n$ elements.
We have found out how to partition 
a set of $n$ elements into $k$ subsets.
However, the distinct subsets are not yet sufficient
to generate all possible permutations,
since the permutation of $(1~2~3~4~5)$
$\sigma1 = (1~2~5) (3~4)$
is not the same as $\sigma2 = (1~5~2) (3~4)$:

\[
\sigma1 = 2~5~4~3~1
\]
\[
\sigma2 = 5~1~4~3~2
\]

So, do we need all permutations of the subsets?
(Was our survey of \acronym{rgs} in vain?)
Apparently not, since $\sigma1$ is just the same
permutation as $\sigma3 = (1~2~5) (4~3)$.
It is also the same as $(5~1~2) (3~4)$.
In fact, the cycle notation is indifferent
concerning the starting point -- for this reason, it is called cyclic.
Therefore, not all permutations are relevant,
but only those that change the order after the first element.
An orbit permutating function aware of this peculiarity is:

\begin{code}
  permOrbits :: (Eq a) => Perm a -> [Perm a]
  permOrbits []      =  [[]]
  permOrbits (o:oo)  =  concat [map (:x) (oPerms o) | x <- permOrbits oo] 
    where  oPerms []      =  []
           oPerms (x:xs)  =  [x:ps | ps <- perms xs] 
\end{code}

This function just passes through all orbits
of the input permutation
creating permutations of each one using |oPerms|.
It looks a bit weird that we do not just use |map|
to create that result, but a list comprehension
to which we even further apply |concat|.
However, |map| does not yield the desired result.
|map| would just create a list of permutated orbits --
but we want to obtain complete cycles each of which
may consist of more than one orbit.
For this reason, we create permutations of the head
and insert all permutations of the head to all results
of the recursion of |permOrbits|.

The function we use for creating permutations of orbits
is |oPerms|, which applies
|perms|, all permutations of a list,
to the tail of the input list.
The head of the list, hence, remains always the same.
For instance, |oPerms [3, 4]| is just |[3,4]|.
|oPerms [1,2,5]|, however, yields |[1,2,5]| and |[1,5,2]|.

Now, for one possible cycle, we can just apply
all permutations resulting from |permOrbits|:

\begin{code}
  permsOfCycle :: (Eq a) => Perm a -> [a] -> [[a]]
  permsOfCycle os xs = [permute o xs | o <- permOrbits os]
\end{code}

This function creates all permutations that are possible
given one partitioning of the input set.
We now map this function on all possible partitionings
with $k$ subsets:

\begin{code}
  permsWithCycles :: (Eq a) => Int -> [a] -> [[a]]
  permsWithCycles k xs = concat [
    permsOfCycle x xs | x <- nPartitions k xs]
\end{code}

Applied on sets with $n$ elements, for $n = 1 \dots  7$,
and $k = 1 \dots n$, |permsWithCycles| yields results of length:

\begin{tabular}{l c c c c c c c c c c c c c c c c c c c c}
1 &   &   &   &   &    &    &    &     &     &   1 &     &     &    &    &    &   &   &   &   &  \\
2 &   &   &   &   &    &    &    &     &   1 &     &   1 &     &    &    &    &   &   &   &   &  \\
3 &   &   &   &   &    &    &    &   2 &     &   3 &     &   1 &    &    &    &   &   &   &   &  \\
4 &   &   &   &   &    &    &  6 &     &  11 &     &   6 &     &  1 &    &    &   &   &   &   &  \\
5 &   &   &   &   &    & 24 &    &  50 &     &  35 &     &  10 &    &  1 &    &   &   &   &   &  \\
6 &   &   &   &   & 120&    &274 &     & 225 &     &  85 &     & 15 &    &  1 &   &   &   &   &  \\   
7 &   &   &   &720&    &1764&    &1624 &     & 735 &     & 175 &    & 21 &    & 1 &   &   &   &  
\end{tabular}

These, as you may have guessed already,
are the Stirling numbers \term{of the first kind},
also known as \term{Stirling cycle numbers},
since they count the number of possible permutations
with a given number of orbits in the cycle notation.
They are denoted by

\begin{equation}
  s(n,k) = \stirlingOne{n}{k}
\end{equation}

and can be calculated as

\begin{equation}
\stirlingOne{n}{k} = \begin{cases}
                       0 & \textrm{if $n = 0$}\\
                       1 & \textrm{if $n = 1$}\\
                       (n-1) \times \stirlingOne{n-1}{k} + 
                                    \stirlingOne{n-1}{k-1} &
                         \textrm{otherwise}.
                   \end{cases}
\end{equation}

In Haskell, this would be:

\begin{code}
  stirling1 :: Natural -> Natural -> Natural
  stirling1 0 _  =  0
  stirling1 _ 0  =  0
  stirling1 1 1  =  1
  stirling1 n k  |  k > n      = 0
                 |  otherwise  = (n-1) *  (stirling1 (n-1) k) + 
                                          (stirling1 (n-1) (k-1))
\end{code}

We have seen that the sum of all Stirling numbers of the second kind 
in one row, \ie\ $\sum_{k=1}^{n}{\stirlingTwo{n}{k}}$,
is the Bell number of $n$.
Can you guess what this sum is for Stirling numbers of the first kind?

Remember that each Stirling number of the form 
$\stirlingOne{n}{k}$ shows the number of permutations
with a given number of orbits in cycle notation.
If you add up all possible permutations of all numbers 
of orbits $1 \dots n$, what do you get?
Let us see:

For $n=1$, we trivially get 1.\\
For $n=2$, we get $1 + 1 = 2$.\\
For $n=3$, we get $2 + 3 + 1 = 6$.\\
For $n=4$, we get $6 + 11 + 6 + 1 = 24$.\\
For $n=5$, we get $24 + 50 + 35 + 10 + 1 = 120$.

We know these numbers: these are the factorials of $n = n!$.
Since the factorial counts the number of all possible
permutations of a set, it is just natural that the 
Stirling numbers of the first kind, 
which count the possible permutations of a set
with a given number of orbits, add up to 
the number of all possible permutations, \ie\ the factorial
of the size of the set.
The triangle itself hints to that.
The outer left diagonal, actually, shows the factorials!
It shows the factorials of $n - 1$ though (with $n$ indicating the row).
If you think of how we create permutations of orbits
-- \viz\ as permutations of the tail of the orbit,
without touching the head --
it becomes immediately clear why the Stirling number $\stirlingOne{n}{1}$,
the one with only one partition, equals $(n-1)!$.

There is still a lot to say about Stirling numbers.
But that may involve concepts we have not yet discussed.
So, we will have to come back to this topic later.

