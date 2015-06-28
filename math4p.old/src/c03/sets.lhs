\ignore{
\begin{code}
module Set
where
  import           Natural
  import qualified Data.Vector as V
  import           Data.Vector ((!))
\end{code}
}

Many real-world problems can be modelled
in terms of \term{sets}.
We have already used sets informally
and, indeed, they are of tremendous importance
in the whole field of mathematics --
set theory is even believed to provide
a sound fundamentation for most areas of mathematics.
This, however, is strongly contested since more than
hundred years now and today 
there are other candidates for this role
besides set theory. Today
many, if not even most mathematicians, 
after long battles
over the foundations of math
mainly during the first half of the $20^{th}$, 
are tired of discussing these issues.

Anyway, what is a set in the first place?
Georg Cantor, one of the main inventors of set theory,
provided several definitions, 
for instance: 
``a set is a Many that allows being thought of as a One''
or: ``a collection of distinct objects''.
Both definitions are quite abstract,
but, in this respect, 
they express a major aspect of set theory quite well.

The second definition, ``collection of distinct objects'',
serves our purposes well enough. 
A set can consist of any kind of objects,
as long as these objects can be clearly distinguished.
Examples are: 
The set of all green things,
the set of all people,
The set of Peter, Paul and Mary,
the set of all animals that belong to the emperor,
the set of the natural numbers from 1 to 9
the set of all natural numbers
and so on.

There are different ways to define sets.
We can first give a definition:
the set of the natural numbers from 1 to 9,
the set of all people, \etc
But we can also name the members explicitly:
Homer, Marge, Bart, Lisa and Maggie or
$1,2,3,4,5,6,7,8,9$.

The first way to define a set is called 
by \term{intension}. The intension of a set is
what it implies, without referring explicitly
to its members.
The second way is called by \term{extension}.
The extension of a set consists of all its members.

This distinction is used in different kinds of
defining lists in Haskell.
One can define a list by extension:
\haskell{[1,2,3,4,5]}
or by intension:
\haskell{[1..5]}, \haskell{[1..]}.
A powerful tool to define lists by intension 
in Haskell is list comprehension, for instance:
\haskell{[x || x <- [1..100], even x, x `mod` 3 /= 0]},
which would contain all even numbers
between 1 and 100 that are not multiples of 3.

Defining sets by intension is 
very powerful. 
The overhead of constructing a set by extension,
\ie\ by naming all its members,
is quite heavy.
If we had to mention all numbers we wanted to use
in a program beforehand, the code would become incredibly large
and we would need to work on it literally an eternity.
Instead, we just define the kind of objects we want to work with.
However, intension bears the risk
of introducing some mind-boggling complications,
one of which is infinite sets.
For the time being, 
we will steer clear of any of these complications.
We have sufficient work with the kind of math
that comes without fierce creatures like infinity.

Sets, as you have already seen, are written in braces
like, for instance:
$\lbrace 1,2,3\rbrace$.
The members of a set, here the numbers 1, 2 and 3,
are called elements of this set,
it holds true, for example, that
$1 \in \lbrace 1,2,3\rbrace$ and 
$0 \not\in \lbrace 1,2,3\rbrace$.

A similar relation is \term{subset}.
A set $A$ is subset of another set $B$, iff
all elements of $A$ are also in $B$:
$\lbrace 1\rbrace \subseteq \lbrace 1,2,3\rbrace$,
$\lbrace 1,3\rbrace \subseteq \lbrace 1,2,3\rbrace$
and also
$\lbrace 1,2,3\rbrace \subseteq \lbrace 1,2,3\rbrace$.
The last case is interesting,
because it asserts that every set is subset of itself.
To exclude this case and only talk about subsets
that are smaller than the set in question,
we refer to the \term{proper} or \term{strict subset},
denoted as $A \subset B$.
An important detail of the subset relation is
that there is one special set that is subset of any set,
\viz\ the \term{empty set} $\lbrace\rbrace$, 
often denoted as $\emptyset$,
that does not contain any elements.

As you can see in the example $\lbrace 1,2,3\rbrace$,
a set may have many subsets.
The set of all possible subsets of a set
is called the \term{powerset} of this set,
often written $P(S)$ for a set $S$.
The powerset of $\lbrace 1,2,3\rbrace$, for example, is:
$\lbrace
\emptyset,
\lbrace 1\rbrace,
\lbrace 2\rbrace,
\lbrace 3\rbrace,
\lbrace 1,2\rbrace,
\lbrace 1,3\rbrace,
\lbrace 2,3\rbrace,
\lbrace 1,2,3\rbrace\rbrace$.

Does this remind you of something?
Perhaps not yet.
What if I was to ask: 
how many elements are there 
in the powerset of a set with $n$ elements?
Well, there is the empty set,
the set itself,
then sets with one element,
sets with two elements
and so on.
How many sets with $k$ elements
are there in the powerset of a set with $n$ elements?
The answer is: 
there are as many sets of $k$ elements
as there are ways to select $k$ items out of $n$.
In other words, 
the size of the powerset equals 
the sum of all binomial coefficients 
$\binom{n}{k}$ for one $n$,
\ie\ for one row of Pascal's Triangle.
For $n=0$, we have: $\binom{0}{0} = 1$,
since the only subset of $\emptyset$ is $\emptyset$.
For $n=1$, we have: $\binom{1}{0} + \binom{1}{1} = 2$.
For $n=2$, we have:
$\binom{2}{0} + \binom{2}{1} + \binom{2}{2}$,
which is $1 + 2 + 1 = 4$.
For $n=3$, we have:
$\binom{3}{0} + \binom{3}{1} + \binom{3}{2} + \binom{3}{3}$,
which is $1 + 3 + 3 + 1 = 8$,
for $n=4$, we have:
$\binom{4}{0} + \binom{4}{1} + \binom{4}{2} + \binom{4}{3} + \binom{4}{4}$,
which is $1 + 4 + 6 + 4 + 1 = 16$.

Probably, you already see  the pattern.
For 5 elements, there are 32 possible subsets;
for 6 elements, there are 64 subsets,
for 7, there are 128 and for 8 there are 256 subsets.
In general,
for a set with $n$ elements,
there are $2^n$ subsets and,
as you may confirm in the Triangle in the previous section,
the sum of all binomial coefficients in one row of the Triangle
is also $2^n$.
This, in its turn, implies that the sum
of the coefficients in an expression of the form 
$a^n + \binom{n}{1}a^{n-1}b + \dots + \binom{n}{n-1}ab^{n-1} + b^n$,
as well, is $2^n$.

Is there a good algorithm 
to construct the powerset of a given set?
There are in fact many ways to build the powerset,
some more efficient or more elegant than others,
but really \emph{good} in the sense
that it efficiently creates powersets
of arbitrarily large sets
is none of them.
The size of the powerset 
increases exponentially in the size
of the input set, which basically means
that it is not feasible at all to 
create the powerset in most cases.
The powerset of a set of 10 elements,
for instance, has \num{1024} elements.
That of a set of 15 elements
has already \num{32768}
and a set of 20 elements has more than a million.

Here is a Haskell implementation
of a quite elegant and simple algorithm:

\begin{code}
  ps :: (Eq a) => [a] -> [[a]]
  ps [] = [[]]
  ps (x:xs) = ps xs ++ map (x:) $ ps xs
\end{code}

Note that we use lists instead of sets.
There is a set module in Haskell,
but since we will not work too much
with sets, we stick to lists
with the convention
that there should be no duplicates
in lists that represent sets.

Let us see
how the $ps$ function works for the 
input $\lbrace 1,2,3\rbrace$.
We start with \haskell{ps (1:[2,3])}
and imediately continue with \haskell{ps (2:[3])}
and, in the next round, with \haskell{ps (3:[])},
which then leads to the base case 
\haskell{ps [] = [[]]}.
On the way back,
we then have \haskell{[[]] ++ map (3:) [[]]},
which leads to the result \haskell{[[],[3]]}.
This, one step further back,
leads to 
\haskell{[[],[3]] ++ map (2:) [[],[3]]},
which results in
\haskell{[[], [3], [2], [2,3]]}.
In the previous step:
\haskell{[[], [3], [2], [2,3]] ++ map (1:) [[], [3], [2], [2,3]]},
resulting in
\haskell{[[], [3], [2], [2,3], [1], [1,3], [1,2], [1,2,3]]}
and we are done.

A completely different approach
uses binary numbers to represent powersets.
This approach is based on the observation
that there are $2^n$ possible subsets
of a set with $n$ elements
and this happens to be the number
of values one can represent
with a binary number of length $n$.
Binary numbers, which we will discuss in more detail later,
use only the digits 0 and 1
instead of the digits $0\dots 9$
as we do with decimal numbers.
In binary numbers we would count like

\begin{center}
\begin{tabular}{r||r}
binary & decimal \\\hline\hline
0 & 0 \\
1 & 1 \\\hline
10 & 2 \\
11 & 3 \\\hline
100 & 4 \\
101 & 5 \\
110 & 6 \\
111 & 7 \\\hline
1000 & 8 \\
1001 & 9 \\
1010 & 10 \\
1011 & 11 \\
1100 & 12 \\
1101 & 13 \\
1110 & 14 \\
1111 & 15
\end{tabular} 
\end{center}

Indeed, there are 10 kinds of persons in the world:
those who understand binary numbers
and those who do not.

To construct the powerset of a set of $n$ elements,
we can use binary numbers with $n$ digits.
We would loop over this numbers starting from 0
and move up to the greatest number representable
with $n$ digits. For $n = 3$, we would loop through:
$000$, $001$, $010$, $011$, $100$, $101$, $110$, $111$.
Each of these numbers describes one subset of the input set,
such that the $k^{th}$ digit of each number would tell us,
whether the $k^{th}$ element of the input set
is part of the current subset.
The number $000$ would indicate the empty set.
The number $001$ would indicate 
that the first element is in the set: $\lbrace 1\rbrace$.
The number $010$ would indicate
that the second element is in the set: $\lbrace 2\rbrace$.
The number $011$ would indicate
that the first and the second element 
are in the set: $\lbrace 1,2\rbrace$
and so on.

An important issue to gain any speed advantage
by this scheme is how to map the binary numbers
to elements in the input set.
We could na\"ively use an underlying representation
of binary numbers as lists -- 
like that of our natural numbers --
iterate through these lists and,
every time we find a 1,
add the corresponding element of the input set
to the current subset.
But this would mean that we had to loop
through $2^n$ lists of length $n$.
That does not sound very efficient.

The key is to realise
that we are talking about numbers.
We do not need to represent binary numbers
as lists at all.
Instead, we can just use decimal numbers
and extract the positions
where, in the binary representation of each number,
there is a 1. 

To illustrate this,
remember that the value of a decimal number
is computed as a sum of powers of 10:
$1024 = 1 \times 10^3 + 0 \times 10^2 + 2 \times 10^1 + 4 \times 10^0$.
The representation of \num{1024} as powers of two
is of course much simpler: 
$1024 = 1 \times 2^{10} + 
        0 \times 2^9 + 0 \times 2^8 + \dots + 0 \times 2^0$
or, for short:
$1024 = 2^{10}$.
Let us look at a number
with a simple decimal representation like \num{1000},
which, in powers of 10, is simply:
$10^3$. Represented as powers of two, however:
$2^9 + 2^8 + 2^7 + 2^6 + 2^5 + 2^3$,
which is 
$512 + 256 + 128 + 64 + 32 + 8 = 1000$.

The point is that the exponents
of \num{1000} represented as powers of two
indicate where the binary representation of \num{1000}
has a 1. \num{1000} in the binary system, indeed, is:
$1111101000$, whereas \num{1024} is
$10000000000$.
Let us index these numbers, first \num{1000}:

\begin{tabular}{ r r r r r r r r r r r}
10 & 9 & 8 & 7 & 6 & 5 & 4 & 3 & 2 & 1 & 0\\\hline
 0 & 1 & 1 & 1 & 1 & 1 & 0 & 1 & 0 & 0 & 0
\end{tabular}

and \num{1024}:

\begin{tabular}{ r r r r r r r r r r r}
10 & 9 & 8 & 7 & 6 & 5 & 4 & 3 & 2 & 1 & 0\\\hline
 1 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0 & 0
\end{tabular}

You see that the indexes in the first row
that have a 1 in the second row
correspond to the exponents of the powers
of two that sum up to the respective number,
\ie\ 9, 8, 7, 6, 5 and 3 for \num{1000}
and 10 for \num{1024}.
We can, hence, interpret the exponents of 
the powers of two as indexes into the set
for which we want to construct the powerset.
Think of the input set as an array
in a language like $C$,
where we can refer to an element of the set
directly by addressing the memory cell
where is resides. 
\haskell{x = set[0];} for instance,
would give us the first element of the set.

When we look at how a number, say $d$, is computed
as sum of powers of two,
we can derive the following algorithm:
compute the greatest power of two
that is smaller than or equal to $d$ 
and than do the same with the difference
between $d$ and this power of two
until the difference,
which in analogy to division,
we may call the remainder,
is zero.
The greatest power of two $\le d$ 
is just the log base 2
of this number rounded down
to the next natural number.
A function implementing this in Haskell
would be:

\begin{code}
  natLog :: Natural -> Natural -> (Natural,Natural)
  natLog b n =  let e = floor $ logBase  (fromIntegral b)
                                         (fromIntegral n)
                 in (e, n - 2^e)
\end{code}

This function takes a natural number $b$,
the base, and a natural number $n$.
The result consists of 
two numbers $(e,r)$ that
shall fulfil the condition $n = b^e + r$.

We can use this function 
to obtain all exponents of the powers of two
that sum up to a given number:

\begin{code}
  binExp :: Natural -> [Natural]
  binExp 0  = []
  binExp 1  = [0]
  binExp n  = let (e,r) = natLog 2 n in e : binExp r
\end{code}

That is, for input 0 and 1,
we explicitly define the results \haskell{[]}
and \haskell{[0]}.
Here, \haskell{[]} means that
there is no 1 in the binary representation
and \haskell{[0]} means that
there is 1 at the first position (indexed by 0).
For any other number $n$,
we calculate the exponent $e$ 
of the greatest power of two $\le n$
and the remainder $r$ and add $e$
to the list that will result from
applying $binExp$ to $r$.
Let us look at the example \num{1000}.
We start with \haskell{natLog 2 1000}:

\haskell{binExp 1000} = $(9, 1000 - 2^9 = 1000 - 512 = 488)$\\
\haskell{9 : binExp 488} = $(8, 488 - 2^8 = 488 - 256 = 232)$\\
\haskell{9 : 8 : binExp 232} = $(7, 232 - 2^7 = 232 - 128 = 104)$\\
\haskell{9 : 8 : 7 : binExp 104} $ = (6, 104 - 2^6 = 104 - 64 = 40)$\\
\haskell{9 : 8 : 7 : 6 : binExp 40} $ = (5, 40 - 2^5 = 40 - 32 = 8)$\\
\haskell{9 : 8 : 7 : 6 : 5 : binExp 8} $ = (3, 8 - 2^3 = 8 - 8 = 0)$\\
\haskell{9 : 8 : 7 : 6 : 5 : 3 binExp 0 = []}\\
\haskell{9 : 8 : 7 : 6 : 5 : 3 : []},

which, indeed, is the list of the exponents
of the powers of two that add up to \num{1000}.

Now we need a function
that loops through all numbers $0 \dots 2^n$,
calculates the exponents of the powers of two for each number
and then retrieves the elements in the input set
that corresponds to the exponents:

\begin{code}
  ps2 :: (Eq a) => [a] -> [[a]]
  ps2 []  = [[]]
  ps2 xs  = go (2^(length xs)-1) 0
    where  go n i  |  i == n     =  [xs]
                   |  otherwise  =  let s = map exp2idx $ binExp i
                                    in  s:go n (i+1)
           exp2idx x = xs!!(fromIntegral x)
\end{code}

The function $ps2$ returns just a set that contains the empty set
when called with the empty set.
Otherwise, it enters a loop with two parameters:
$2^{(length~xs)}-1$, which is the greatest number 
that can be represented with a binary number with $n$ digits,
when we start to count from 0.
For each number $i$:
if we have reached the last number, we just know
the corresponding subset is the input set itself.
Otherwise, we map a mapping function $exponent \rightarrow index$
to the result of $binExp$ applied to the current number $i$.
The mapping function, $exp2idx$, 
uses the list index operator \haskell{!!} 
to get the element of the input list $xs$
at the position $x$, which is just an exponent.
(Note that we have to convert x from $Natural$ to $Int$,
since \haskell{!!} expects an $Int$ value.)

This algorithm exploits a fascinating
\term{isomorphism} -- an analogous structure --
between binary numbers and powersets.
With an appropriate data structure
to represent sets, like arrays,
and, of course, a more efficient number representation
than our humble natural numbers,
the algorithm definitely beats the one
we implemented as $ps$.
Unfortunately, lists show very bad performance
with random access such as indexing.
Therefore, $ps2$ is slower than $ps$.
But using Haskell vectors (implemented in module $Data.Vector$)
and Integers instead of our $Natural$,
$ps2$ is indeed faster.
The changes, by the way, are minimal.
Just compare the implementation of $ps2$ and $psv$:

\begin{code}
  psv :: (Eq a) => [a] -> [[a]]
  psv []  = [[]]
  psv xs  =  let v = V.fromList xs 
             in go v (2^(length xs)-1) 0
    where  go v n i  |  i == n     =  [xs]
                     |  otherwise  =  let s = map exp2idx $ binExp i
                                      in  s:go v n (i+1)
           exp2idx x = v!(fromIntegral x)
\end{code}

The changes to the code of $ps2$ relate to the introduction
of $v$, a vector created from $xs$ 
by using the $fromList$ function from the vector module,
which is qualified as $V$.
In practical terms, however,
the performance of the powerset function
does not matter too much,
since, as already said, it is not feasible
to compute the powerset of huge sets anyway.
Nevertheless, problems related to subsets
are quite common.
An infamous example is the \term{set cover} problem.

The challenge in the set cover problem
is to combine given subsets of a set $A$
so that the combined subsets together 
equal $A$. This involves an operation
on sets we have not yet discussed.
Combining sets is formally called \term{union}:
$A \cup B$.
The union of two sets, $A$ and $B$,
contains all elements that are in $A$ or $B$
(or both),
for example:
$\lbrace 1,2,3\rbrace \cup \lbrace 3,4,5\rbrace = 
 \lbrace 1,2,3,4,5\rbrace$.

Two other important set operations
are intersection and difference.
The intersection of two sets $A$ and $B$,
$A \cap B$, contains all elements $x$,
such that $x \in A$ and $x \in B$.
To continue with the example used above:
$\lbrace 1,2,3\rbrace \cap \lbrace 3,4,5\rbrace
= \lbrace 3\rbrace$.
The intersection of the union of two sets
with one of these sets is just that set,
$(A \cup B) \cap A = A$:
$(\lbrace 1,2,3\rbrace \cup \lbrace 3,4,5\rbrace)
 \cap \lbrace 1,2,3\rbrace = 
 \lbrace 1,2,3,4,5\rbrace \cap \lbrace 1,2,3\rbrace =
 \lbrace 1,2,3\rbrace$.

The difference of two sets $A$ and $B$, $A \setminus B$,
contains all elements in $A$ that are not in $B$,
for example:
$\lbrace 1,2,3\rbrace \setminus \lbrace 3,4,5\rbrace = 
\lbrace 1,2\rbrace$.
If $B$ is a subset of $A$,
then the different $A \setminus B$ is called
the \term{complement} of $B$ in $A$.

Now let us model the three set operations
union, intersection and difference with Haskell lists.
The simplest case is difference,
since, assuming that we always use lists
without duplicates,
we can just use the predefined list operator 
\textbackslash\textbackslash.
Union is not too difficult either
using the function $nub$,
which removes duplicates from a list:

\begin{code}
  union :: (Eq a) => [a] -> [a] -> [a]
  union a b = nub (a ++ b)
\end{code}

Using $nub$ is necessary,
since merging the two list
will introduce duplicates for any 
$x \in a \wedge x \in b$.

Intersect is slightly more difficult.
The intersect of two sets $a$ and $b$ 
is the set that contains
all elements that are in both sets, $a$ and $b$.
We could implement this by means
of $nub$ as well, since we used $nub$ in $union$
to remove the duplicates of exactly those elements
that we want to have in intersect.
The intersect, hence, could be implemented as
\haskell{a ++ b \textbackslash\textbackslash\ nub (a ++ b)}.
This would define the intersect as the difference
of the concatenation of two lists
and the union of these two lists.
Have a look at the example
$A=\lbrace 1,2,3\rbrace$ and
$B=\lbrace 3,4,5\rbrace$:

\haskell{A ++ B \textbackslash\textbackslash\ nub (A ++ B) =}\\
\haskell{[1,2,3] ++ [3,4,5] \textbackslash\textbackslash\ 
         nub ([1,2,3] ++ [3,4,5]) = }\\
\haskell{[1,2,3,3,4,5] \textbackslash\textbackslash\ nub ([1,2,3,3,4,5]) = }\\
\haskell{[1,2,3,3,4,5] \textbackslash\textbackslash\ [1,2,3,4,5]) = }\\
\haskell{[3]}.
         
This implementation, however,
is not very efficient.
Preferable is the following one:

\begin{code}
  intersect :: (Eq a) => [a] -> [a] -> [a]
  intersect [] _  =  []
  intersect _ []  =  []
  intersect (a:as) bs  |  a `elem` bs  = a :  intersect as bs
                       |  otherwise    =      intersect as bs
\end{code}

We first define the intersection of the empty set
with any other set as the empty set.
(Note the similarity of the $\emptyset$ 
 in union and intersection with $0$ in 
 addition and multiplication!)
For other cases, we start with the first element of the first list, $a$,
and check if it is also in $bs$;
if so, we add $a$ to the result set,
whose remainder results from the application
of $intersect$ on the tail of the first list;
otherwise, we only construct the remainder
of the result set without adding anything
in this round. 

We now can state the set cover problem more formally:
We have a set $U$, called the \term{universe},
and a set $S = \lbrace s_1,s_2, \dots, s_n\rbrace$ 
of subsets of $U$, 
$s_1 \subseteq U, s_2 \subseteq U, \dots, s_n \subseteq U$,
such that the union of all the sets in $S$ equals $U$,
$s_1 \cup s_2 \cup \dots \cup s_n = U$.
What is the least expensive union of a subset of $S$
that yields $U$?

Least expensive may be interpreted in different ways.
In the pure mathematical sense,
it usually means the smallest number of sets,
but in real world problems,
least expensive may refer to lowest cost,
shortest time, fewest people involved, \etc\
The problem is in fact very common.
It comes up in scheduling problems
where the members of $S$
represent sets of threads assigned
to groups of processors;
very typical are problems of
team building where the sets in $S$
represent teams of people with complementing skills;
but there are also problems 
similar to the \term{travelling salesman}
problem where the sets in $S$ 
represent locations that must be visited 
during a round trip.

So, how many steps do we need to solve this problem?
To find the optimal solution,
we basically have to try out
all combinations of subsets in $S$.
For $S = \lbrace a,b,c\rbrace$,
$\lbrace a\rbrace$ may be the best solution,
$\lbrace b\rbrace$ may be,
$\lbrace c\rbrace$,
$\lbrace a,b\rbrace$,
$\lbrace a,c\rbrace$, 
$\lbrace b,c\rbrace$ and, of course,
$\lbrace a,b,c\rbrace$.
As you should see,
that are $2^n$ possibilities,
\ie\ the sum of all binomial coefficients $\binom{n}{k}$
where $n$ is the size of $S$.
That, as we know, is not feasible to compute
with large $S$'s.
There are, however, solutions for specific problems
using heuristics.

Heuristics are helpers in otherwise exponential
search problems.
In practice, heuristics may be derived from the concrete
problem domain.
With respect to the examples mentioned above,
it is often obvious that we do 
not want to combine threads on one processor
that better work in parallel;
concerning problems with teams,
we could exclude combinations of people
who do not like each other
or we may want to construct gender balanced teams. 
Such restrictions and insights
can be used to drastically reduce 
the number of possible solutions
and, thus, making computation feasible.
But think, for instance,
of a general purpose operating system
that does not have any previous knowledge
about the user tasks it runs.
No real-world heuristics are available
for the kernel to find an optimal balance.

There are, however, also purely mathematical
heuristics. For the set cover problem,
a known heuristic that reduces 
computational complexity significantly,
is to search for local optimums
instead of the global optimum.
That is, we do not try to find the 
solution that is the best compared
with all other solution,
but, instead, we make optimal
decisions in each round.
For example, if we had the universe
$U = \lbrace 1,2,3,4,5,6\rbrace$
and $S = \lbrace 
\lbrace 1,2,3\rbrace,
\lbrace 1,2,4\rbrace,
\lbrace 1,4\rbrace,
\lbrace 3,5\rbrace,
\lbrace 1,6\rbrace\rbrace$,
the optimal solution would be
$\lbrace
\lbrace 1, 2, 4\rbrace, 
\lbrace 3, 5\rbrace,\\ 
\lbrace 1, 6\rbrace\rbrace$.
The key to find this solution
is to realise that 
the second set in $S$,
$\lbrace 1,2,4\rbrace$,
is the better choice compared to the first set
$\lbrace 1,2,3\rbrace$.
But to actually realise that,
we have to try all possible combinations of sets,
which are $2^n$ and, hence, too many.
An algorithm
that does not go for the global optimum,
but for local optimums,
would just take the first set,
because, in the moment of the decision,
it is one of two equally good options
and there is nothing that would hint to the fact
that, with the second set, 
the overall outcome would be better.
This \term{greedy} algorithm will
consequently find only a suboptimal solution,
\ie\:
$\lbrace 1,2,3\rbrace$,
$\lbrace 1,4\rbrace$ or even $\lbrace 1,2,4\rbrace$,
$\lbrace 3,5\rbrace$ and
$\lbrace 1,6\rbrace$.
It, hence, needs one set more
than the global optimum.

In many cases,
local optimums are sufficient
and feasible to compute.
This should be motivation enough
to try to implement a greedy solution
for the set cover problem.
The algorithm will in each step
take the set that brings the greatest
reduction in the distance between the current
state and the universe.
We, first, need some way to express this distance
and an obvious notion for distance
is just the length of the difference
between the universe and another set:

\begin{code}
  dist :: (Eq a) => [a] -> [a] -> Int
  dist a b = length (a \\ b)
\end{code}

Now, we need a function, say, $best$
that uses $dist$ to find the set in $S$
with the least distance to the universe
and another function that 
repeatedly finds the local minimum
using $best$, until either
all sets in $S$ have been used
or no set in $S$ is able to reduce
the distance to the universe anymore.
Here are these functions:

\begin{code}
  greedySetCover :: (Eq a) => [a] -> [[a]] -> [[a]]
  greedySetCover u s = loop (length u) [] s
    where  loop _ _  []  = []
           loop m rs xs  =  let (m',p) = best m rs [] xs
                            in if m' < m 
                                 then p : loop m' (p `union` rs) (delete p xs)
                                 else []
           best m r p []      =  (m, p)
           best m r p (x:xs)  =  let m' = dist u (x `union` r)
                                 in if m' < m  then  best m'  r x xs
                                               else  best m   r p xs
\end{code}

The measure for the current optimum is 
the variable $m$ used in $loop$ and $best$.
The whole algorithm starts with $m = length(u)$,
which is the worst possible distance,
\viz\ the distance between $\emptyset$ and the universe.

The second parameter passed to $loop$, $rs$,
is the union of all partial results.
It is initially empty.
The third parameter is the set of subsets
we are working on
starting with $S$.
With an empty $S$, $loop$
is just $\emptyset$.
Otherwise, it uses $best$
to get the local optimum,
which is the tuple $(m',p)$,
where $p$ is the best choice for the local optimum
and $m'$ the distance of this set to the universe.
If $m' < m$,
then we actually have found a solution 
that improves on the current state
and we continue adding $p$ to the result set,
which results from the recursion of $loop$
with $m'$ as current optimum,
the union of $p$ and the partial result $rs$
and the current instance of $S$ without $p$.
Otherwise, the result is just the empty set.

The function $best$ simply goes through
all elements of the current instance of $S$.
If $best$ arrives at the end of the list,
it just returns the previously identified optimum $(m,p)$.
Otherwise, for each element of the current set of subsets,
it computes the distance and,
should the current distance improve on the result,
continues with this current optimum,
if it does not, it continues with the old parameters.

The fact that we do not go back
in the $loop$ function to test other options,
but always stick with a solution once it was found
makes this algorithm \term{greedy}:
It takes the money and runs.
What is the speed-up
we obtain with this apparently ugly strategy?
One call of $best$ passes through the whole list,
which, initially, is $S$.
$loop$, if $best$ has found an optimum
that improves on the old result,
removes the corresponding element from the list
and repeates the process.
This time, $best$ will go through a list of $n-1$ elements,
where $n$ is the size of $S$.
If it finds a new mimimum again,
the corresponding element is removed,
and we get a list of $n-2$ elements.
The process repeats, until $best$ does not find
a new optimum anymore.
In the worst case, this is only after all elements
in the list have been consumed.
The maximum number of steps
that must be processed, hence,
is $n + n - 1 + n - 2 + \dots + 1$
or simply the series $\sum_{k=1}^{n}{k}$,
which, as we already know,
is $\frac{n^2 + n}{2}$.
For a set $S$ with 100 elements,
we would need to consider $2^{100}$ possible cases
to compute the global optimum, 
which is \num{1267650600228229401496703205376}.
With the local optimum,
we can reduce this number 
to $\frac{100 \times 101}{2} = 5050$ steps.
For some cases,
the local minimum is therefore the preferred solution.
