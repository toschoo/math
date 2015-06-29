\ignore{
\begin{code}
module Cantor1
where
  import Natural
  import Quoz
\end{code}
}

How many negative numbers are there?
That is a strange question! 
How do you want me to answer?
I can tell you how many numbers
of a specific kind are there only
in relation to another kind of numbers.
The words ``how many'' clearly indicate
that the answer is again a number.

Let us try to state the question more pecisely:
are there more or fewer negative numbers
than natural numbers or are
there exactly as many negative numbers
as natural numbers?

To answer the question, 
I would like to suggest
a way to compare two sets.
To compare the size of two sets, $A$ and $B$, 
we create a third set,
which consists of pairs $(a,b)$,
such that $a\in A$ and $b \in B$.
The sets are equal, if and only if
every $a \in A$ appears exactly once
in the new set and every $b \in B$
appears exactly once in the new set.
If there is an $a \in A$
that does not appear in the new set,
but all $b \in B$ appear exactly once,
then $B$ is greater than $A$.
If there is a $b \in B$
that does not appear, but all $a \in A$
appear, then $A$ is greater than $B$.

Furthermore, I suggest a way of counting a set $A$.
We count a set by creating a new set
that consists of pairs $(a,n)$,
such that $a \in A$ and $n \in \mathbb{N}$.
For $n$, we start with 0 and, for each element
in $A$, we increase $n$ by 1 before we put it
into the new set, like this:

\begin{minipage}{\textwidth}
\begin{code}
  count :: [a] -> [(a,Natural)]
  count = go 0
    where  go _ []     = []
           go n (x:xs) = (x,n+1) go (n+1) xs
\end{code}
\end{minipage}

The greatest number $n$, we find in the pairs
of this set is the number of elements in $A$.
Let us see if we can count the negative numbers
in this manner.
We count them by creating the set 
$\lbrace (-1,1), (-2,2), (-3,3), \dots\rbrace$.
Do we ever run out of negative or natural numbers?
I don't think so. 
Should we ever feel that we run out of negative
numbers, then we just take the current natural number
and put a minus sign before it.
Should we ever feel that we run out of natural numbers,
then we simply take the current negative number
and remove the negative sign.
This proves, I guess, that there is a way
to assign each negative number to exactly one
natural number and vice versa.
There are hence as many negative numbers
as natural numbers.

Well, how many numbers are that?
That are $||\mathbb{N}||$ numbers.
If you want a word for that, call it 
\term{aleph-zero} and write it like this: $\aleph_0$.
A set with this cardinality is infinite,
but countable. Calling |count| on it,
we will never get a final answer.
But we will have a partial result at any given step.

What about fractions?
On the first sight, fractions look very different.
There are infinitely many of them between
any two natural numbers as we have seen 
with Zeno's paradox.
But now comes Cantor and his first
diagonal argument to show that fractions
are countable and, therefore, that the set of fractions
has cardinality $\aleph_0$.

Cantor's proof, his first diagonal argument,
goes as follows. He arranged the fractions
in a table, such that the first column contained
the integers starting from 1 in the first row
and counting up advancing row by row.
The integers correspond to fractions with 1
as denominator. So, we could say,
the first column of this table is dedicated
to denominator 1. The second column, correspondingly,
is dedicated to denominator 2; the third
to denominator 3 and so on.
Then, the rows are dedicated likewise to numerators.
The first row contains numerator 1, the second
contains numerator 2, the third numerator 3
and so on.
Like this:

\begin{center}
\begingroup
\renewcommand{\arraystretch}{1.5}
\begin{tabular}{||c||c||c||c||c||c||c}\hline
$\frac{1}{1}$ & $\frac{1}{2}$ & $\frac{1}{3}$ & $\frac{1}{4}$ & $\frac{1}{5}$ & $\frac{1}{6}$ & $\dots$\\\hline
$\frac{2}{1}$ & $\frac{2}{2}$ & $\frac{2}{3}$ & $\frac{2}{4}$ & $\frac{2}{5}$ & $\frac{2}{6}$ & $\dots$\\\hline
$\frac{3}{1}$ & $\frac{3}{2}$ & $\frac{3}{3}$ & $\frac{3}{4}$ & $\frac{3}{5}$ & $\frac{3}{6}$ & $\dots$\\\hline
$\frac{4}{1}$ & $\frac{4}{2}$ & $\frac{4}{3}$ & $\frac{4}{4}$ & $\frac{4}{5}$ & $\frac{4}{6}$ & $\dots$\\\hline
$\frac{5}{1}$ & $\frac{5}{2}$ & $\frac{5}{3}$ & $\frac{5}{4}$ & $\frac{5}{5}$ & $\frac{5}{6}$ & $\dots$\\\hline
$\frac{6}{1}$ & $\frac{6}{2}$ & $\frac{6}{3}$ & $\frac{6}{4}$ & $\frac{6}{5}$ & $\frac{6}{6}$ & $\dots$\\\hline
$\dots$       & $\dots$       & $\dots$       & $\dots$       & $\dots$       & $\dots$       & $\dots$
\end{tabular}
\endgroup
\end{center}

This table is a brute-force approach
to list all fractions in two dimensions.
Obviously, this table contains all possible fractions,
since, for any possible pair of numbers, it contains
a fraction, which, however, is not necessarily
in canonical form, so the table contains duplicates.

Then, using this table, he created a sequence.
He started with the first cell containing $\frac{1}{1}$.
From there he went to the next row $\frac{2}{1}$.
Then he applied the rule |up|,
that is he went up following a diagonal line to the first row,
so he would eventually reach $\frac{1}{2}$.
He went one to the right and then applied rule |down|,
that is he went down following a diagonal line
to the first column, so he would eventually reach $\frac{3}{1}$.
Now he continued to the next row and repeated 
the process of going |up| and |down| in
diagonal lines infitely adding the number of each
cell he crossed to the sequence.

The sequence evolves as follows:
Cantor starts with $\frac{1}{1}$, adds $\frac{2}{1}$,
applies rule |up| and adds $\frac{1}{2}$,
goes to the right, adds $\frac{1}{3}$ and
applies rule |down| adding $\frac{2}{2}$;
then he goes to the next row adding $\frac{4}{1}$
and goes |up| again adding $\frac{3}{2}$,
$\frac{2}{3}$ and $\frac{1}{4}$ and so he
goes on forever.

We can reformulate this rule in Haskell,
which will make the process clearer:

\begin{minipage}{\textwidth}
\begin{code}
  cantor1 :: [Ratio]
  cantor1 = (1%1):go 2 1
    where  go    n 1   = up    n 1  ++ go 1 (n+1) 
           go    1 d   = down  1 d  ++ go (d+1) 1
           down  n 1   = [n%1]
           down  n d   = (n%d) : down  (n+1) (d-1)
           up    1 d   = [1%d]
           up    n d   = (n%d) : up    (n-1) (d+1)
\end{code}
\end{minipage}

When we look at rule |up|, starting at the bottom
of the code, we see the base case where the numerator
is 1. In this case, we just yield |[1%d]|.
Otherwise, we call |up| again with the numerator |n|
decremented by 1 and the denominator incremented by 1.
|up 4 1|, thus, is processed as follows:

\begin{minipage}{\textwidth}
|up 4 1 = (4%1): up 3 2 |\\
|up 3 2 = (3%2): up 2 3 |\\
|up 2 3 = (2%3): up 1 4 |\\
|up 1 4 = [1%4]|
\end{minipage}

yielding the sequence
$\frac{4}{1}, 
 \frac{3}{2}, 
 \frac{2}{3}, 
 \frac{1}{4}$. 
|go|, after calling |up|, proceeds with 
|go 1 (n+1)|.
We, hence, would continue with 
|go 1 5|, which calls |down|.
The base case of |down| is the case
where the denominator is 1.
Otherwise, we increment the numerator by 1
and decrement the denominator by 1.
|down 1 5|, hence, is processed as follows:

\begin{minipage}{\textwidth}
|down 1 5 = (1%5): down 2 4 |\\
|down 2 4 = (2%4): down 3 3 |\\
|down 3 3 = (3%3): down 4 2 |\\
|down 4 2 = (4%2): down 5 1 |\\
|down 5 1 = [5%1]|
\end{minipage}

yielding the sequence
$\frac{1}{5}, 
 \frac{2}{4}, 
 \frac{3}{3}, 
 \frac{4}{2},
 \frac{5}{1}$. 
When we put the sequence together,
including the first steps,
we see

\[
 \frac{1}{1},
 \frac{2}{1}, 
 \frac{1}{2}, 
 \frac{1}{3}, 
 \frac{2}{2}, 
 \frac{3}{1}, 
 \frac{4}{1}, 
 \frac{3}{2}, 
 \frac{2}{3}, 
 \frac{1}{4}, 
 \frac{1}{5}, 
 \frac{2}{4}, 
 \frac{3}{3}, 
 \frac{4}{2},
 \frac{5}{1},
 \dots
\] 

When we reduce all fractions to canonical form,
we see a lot of repetitions:

\[
 \frac{1}{1},
 \frac{2}{1}, 
 \frac{1}{2}, 
 \frac{1}{3}, 
 \frac{1}{1}, 
 \frac{3}{1}, 
 \frac{4}{1}, 
 \frac{3}{2}, 
 \frac{2}{3}, 
 \frac{1}{4}, 
 \frac{1}{5}, 
 \frac{1}{2}, 
 \frac{1}{1}, 
 \frac{2}{1},
 \frac{5}{1},
 \dots
\] 

We see the numbers 
$\frac{1}{1} = 1$,
$\frac{2}{1} = 2$ and
$\frac{1}{2}$ repeated several times.
They will continue to appear over and over again and,
even worse, other numbers will start to reappear too.
That is, before we can use this sequence to count
fractions, we need to filter duplicates out
leading to the sequence

\[
 \frac{1}{1},
 \frac{2}{1}, 
 \frac{1}{2}, 
 \frac{1}{3}, 
 \frac{3}{1}, 
 \frac{4}{1}, 
 \frac{3}{2}, 
 \frac{2}{3}, 
 \frac{1}{4}, 
 \frac{1}{5}, 
 \frac{5}{1},
 \dots
\]

But now we see that we can enumerate, that is count
the fractions creating the sequence

\[
 \left(1,\frac{1}{1}\right),
 \left(2,\frac{2}{1}\right), 
 \left(3,\frac{1}{2}\right), 
 \left(4,\frac{1}{3}\right), 
 \left(5,\frac{3}{1}\right), 
 \left(6,\frac{4}{1}\right), 
 \left(7,\frac{3}{2}\right), 
 \left(8,\frac{2}{3}\right), 
 \left(9,\frac{1}{4}\right), 
 \dots
\]

This clearly shows that the cardinality 
of the set of fractions is $\aleph_0$.\qed

This result may feel a bit odd on the first sight.
We clearly have the feeling that there must
be more fractions than integers, because,
between any two integers, there are infinitely many
fractions. 
When we think of a visualisation with a pair of balances,
with the fractions being in one balance and the integers
in the other, then, what we would see at any given instance, 
clearly indicates that there must be more fractions than integers.
However, our feeling betrays us, when it comes
to infinity. Indeed, our feeling was not made for infinity.
Therefore, at least if we accept the notions
of comparison and counting outlined above,
then we have to accept the result of Cantor's
argument.
Even further, I would say that the fact that this argument
shows things in a way that contradicts our spontaneous
way to see these things, underlines
the extraordinary quality of this argument.
Cantor lets us see things that are usually
hidden from our perception.
This makes Cantor, who was seen by his
contemporary opponents as a kind of sorcerer,
a true magus.

It is, by the way, quite simple
to extend the argument to negative fractions.
We just have to insert behind each number
its additive inverse, resulting in
the sequence:

\[
 \left(1,\frac{1}{1}\right),
 \left(2,-\frac{1}{1}\right),
 \left(3,\frac{2}{1}\right), 
 \left(4,-\frac{2}{1}\right), 
 \left(5,\frac{1}{2}\right), 
 \left(6,-\frac{1}{2}\right), 
 \left(7,\frac{1}{3}\right), 
 \left(8,-\frac{1}{3}\right), 
 \dots
\]

Indeed, there appears to be a lot of room
for new numbers, once we are dealing with
infinity.
This led the great David Hilbert to the
analogy of the hotel, which is today 
named after him \term{Hilbert's Hotel}.
In this analogy, there is a hotel with the
uncommon property of having infinitely
many rooms. This hotel will never run
out of rooms for new guests.
If a new guest arrives and there are
already infinitely many guests,
the manager just asks the guests
to move one room up, \ie\ the guest
currently in room 1 moves to room 2,
the guest in room 2 moves to room 3
and so on. 
At the end of the process,
room 1 is free for the new arriving guest.
Since there are infinitely
many rooms, there is no guest,
even though there are infinitely many guests already,
who would not find a room with a room number
one greater than his previous room number.

This approach works for any number of finitely many
guests arriving.
But even when infinitely many new guests arrive,
the manager, still, has resources.
In this case, he could ask the guests to move to
a room with a number twice the number of his
current room, \eg\ the guest in room 1
would move to room 2, the guest in room 2
would move to room 4, the guest in room 3
would move to room 6 and so on
leaving infinitely many rooms
with odd room numbers unoccupied
and making room for infinitely many new guests.

But let us go back to more technical stuff.
In spite of its ingenuity, Cantor's argument
is not perfect.
In particular, the sequence and how it is
created is quite uggly,
but, apparently, nobody, for more than hundred years,
cared too much about that. 
Then, in \num{2000}, Neil Calkin and Herbert Wilf
published a paper with a new sequence with 
a bunch of interesting properties that make
this sequence for enumerating the fractions
much more attractive than Cantor's original
sequence. The beginning of the sequence is

\[
 \frac{1}{1},
 \frac{1}{2}, 
 \frac{2}{1}, 
 \frac{1}{3}, 
 \frac{3}{2}, 
 \frac{2}{3}, 
 \frac{3}{1}, 
 \frac{1}{4}, 
 \frac{4}{3}, 
 \frac{3}{5}, 
 \frac{5}{2},
 \frac{2}{5},
 \frac{5}{3},
 \frac{3}{4},
 \frac{4}{1},
 \dots
\]

The sequence, as we will show in a minute,
corresponds to a \term{binary tree} of the form

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
\node (A5) at ( 6,-2 ) {$\frac{3}{2}$};

% kids of A3
\node (A6) at ( 8,-2 ) {$\frac{2}{3}$};
\node (A7) at (10,-2 ) {$\frac{3}{1}$};

% kids of A4
\node (A8) at ( 3  ,-3 ) {$\frac{1}{4}$};
\node (A9) at ( 4.2,-3 ) {$\frac{4}{3}$};

% kids of A5
\node (A10) at ( 5.2,-3 ) {$\frac{3}{5}$};
\node (A11) at ( 6.5,-3 ) {$\frac{5}{2}$};

% kids of A6
\node (A12) at ( 7.5,-3 ) {$\frac{2}{5}$};
\node (A13) at ( 8.5,-3 ) {$\frac{5}{3}$};

% kids of A7
\node (A14) at ( 9.5,-3 ) {$\frac{3}{4}$};
\node (A15) at (11  ,-3 ) {$\frac{4}{1}$};

% kids of A8
\node (A16) at ( 2  ,-4 ) {$\frac{1}{5}$};
\node (A17) at ( 2.6,-4 ) {$\frac{5}{4}$};

% kids of A9
\node (A18) at ( 3.3,-4 ) {$\frac{4}{7}$};
\node (A19) at ( 3.9,-4 ) {$\frac{7}{3}$};

% kids of A10
\node (A20) at ( 4.7,-4 ) {$\frac{3}{8}$};
\node (A21) at ( 5.3,-4 ) {$\frac{8}{5}$};

% kids of A11
\node (A22) at ( 6.1,-4 ) {$\frac{5}{7}$};
\node (A23) at ( 6.7,-4 ) {$\frac{7}{2}$};

% kids of A12
\node (A24) at ( 7.3,-4 ) {$\frac{2}{7}$};
\node (A25) at ( 7.9,-4 ) {$\frac{7}{5}$};

% kids of A13
\node (A26) at ( 8.7,-4 ) {$\frac{5}{8}$};
\node (A27) at ( 9.3,-4 ) {$\frac{8}{3}$};

% kids of A14
\node (A28) at (10.1,-4 ) {$\frac{3}{7}$};
\node (A29) at (10.6,-4 ) {$\frac{7}{4}$};

% kids of A15
\node (A30) at (11.4,-4 ) {$\frac{4}{5}$};
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
\ignore{$}

When you have a closer look at the tree,
you see that the kids of each node
are created by a simple formula.
If the current node has the form
$\frac{n}{d}$, then the left kid
corresponds to $\frac{n}{n+d}$ and
the right kid corresponds to $\frac{n+d}{d}$.
For instance, the kids of $\frac{1}{1}$
are $\frac{1}{1+1}$ and $\frac{1+1}{1}$.
The kids of $\frac{3}{2}$ are
$\frac{3}{3+2}$ and $\frac{3+2}{2}$.

We can easily create this tree in Haskell.
First, we need a data type to represent
the tree:

\begin{minipage}{\textwidth}
\begin{code}
  data Tree a = Node a [Tree a]
    deriving (Eq, Show)

  type CalWiTree = Tree Ratio
\end{code}
\end{minipage}

The |Tree| data type is in fact not a
binary tree, but a generic tree with
an arbitrary number of nodes.
But it is simple to implement and
serves our purpose.
The data type is parametrised,
so we define a specialised data type
|Tree Ratio| and the type synonym
|CalWiTree| referring to this type.
Now we create the tree with:

\begin{minipage}{\textwidth}
\begin{code}
  calWiTree :: Zahl -> Ratio -> CalWiTree
  calWiTree 1 r          = Node r []
  calWiTree i r@(Q n d)  = Node r [  calWiTree (i-1) (n % (n+d)),
                                     calWiTree (i-1) ((n+d) % d)]
\end{code}
\end{minipage}

The function takes two arguments,
a |Zahl| and a |Ratio|.
The |Ratio| is the starting point
and the |Zahl| is the number of generations
we want to create.
Often we do not want the function to create
the entire sequence -- for that a lot of patience
and memory resources would be necessary --
but only a tiny part of it.
In this case, we set the |Zahl| to $i\ge 1$.
If $i$ reaches 1, we create the current node
without kids.
If we want the function to create the entire
infinite tree, we just assign a value $i < 1$.
If $i\neq 1$,
we create a node with |r| as data and 
the kids resulting from calling |calWiTree| on
$\frac{n}{n+d}$ and $\frac{n+d}{d}$
with $i$ reduced by 1.

This shows that there is a very simple algorithm
to generate the tree.
We will now show that 
Calkin-Wilf tree and Calkin-Wilf sequence 
are equivalent.
We do so by creating an algorithm that 
converts the tree to the sequence.

We may be tempted to do this with a typical
recursive function that converts the current node
into a member of the sequence and adds it to
the partial sequence that results from recursively
calling the function on the left and the right kid.
This approach, however, is \term{depth-first}.
The resulting sequences would follow the branches
of the tree. It would create partial sequences
like, for instance, 
$\frac{1}{1},
 \frac{1}{2},
 \frac{1}{3},
 \frac{1}{4},
 \dots$
But what we need is partial sequences
that cover generation by generation, \ie\
$\frac{1}{1},
 \frac{1}{2},
 \frac{2}{1},
 \frac{1}{3},
 \frac{3}{2},
 \frac{2}{3},
 \frac{3}{1}$ and so on.
In other words, we need a \term{breadth-first} approach.

Since this is a generic problem,
we can define a function on the level
of the generic |Tree| data type
that creates a sequence composed
of subsequences corresponding to tree generations:

\begin{minipage}{\textwidth}
\begin{code}
  getKids :: Natural -> Tree a -> [a]
  getKids 1  (Node r  _)       =  [r]
  getKids n  (Node r  [])      =  []
  getKids n  (Node r  (x:xs))  =  getKids (n-1) x ++ 
                                  getKids n (Node r xs) 
\end{code}
\end{minipage}

The function receives two arguments,
a |Natural|, which determines the generation we want to obtain,
and the tree on which we are operating.
If the generation is 1, we just give back the data
of the current node.
Otherwise, we distinguish two cases:
If the current node has no kids, then the result 
is the empty list. This indicates that we have
exhausted the current (finite) tree.
Otherwise, we advance recursively on the head and tail
of the list of kids.
Decisive is that we do not add anything 
to the resulting sequence, before we have reached
the intended depth $n$.
This way, the function produces a sequence
containing all kids on level $n$.
We now just apply |getKids| to all
generations in the tree:

\begin{minipage}{\textwidth}
\begin{code}
  calWiTree2Seq :: CalwiTree -> [Rational]
  calWiTree2Seq t = go 1
    where go n =  case getKids n t of
                  []  -> []
                  sq  -> sq ++ go (n+1)
\end{code}
\end{minipage}

We have shown that there is a simple algorithm
to generate the tree and 
that there is a simple algorithm to convert
the tree into the sequence.
The latter aspect is quite useful,
since it means that tree and sequence are equivalent.
This allows us to prove some crucial properties
of the sequence using the tree,
which is much simpler than proving them
on the sequence directly.

Already a quick glance at the tree
reveales some interesting properties,
for instance, that, in all cases,
the left kid is smaller and the right kid
is greater than the parent node.
This, of course, is just a consquence
of the generating algorithm.
We also see that the integers are all
in the right-most branch, which equals
the first first column in Cantor's table.
The left-most branch equals the first row
in Cantor's table: it contains all fractions
with 1 in the numerator.
We also see that all fractions are in
canonical form, different from Cantor's table.
Also, no fraction repeats and, as far as we can tell,
the fractions appear to be complete.

The crucial properties, those that we need
to show that the Calkin-Wilf sequence contains
exactly all fractions and, thus, can be used
for Cantor's argument in place of the old sequence,
are:

\begin{enumerate}
\item All fractions in the tree are in canonical form;
\item Every possible fraction (in canoncial form) 
      appears in the tree;
\item Every fraction appears exactly once.
\end{enumerate}

The first property is simple to prove.
We observe that
the first fractions appearing in the tree 
that do not have
1 in numerator or denominator have
there either 2 or 3. 2 and 3 a coprime to each other.
Summing two coprimes, $a$ and $b$, will lead to a number
that again is coprime to both $a$ and $b$.
Therefore, if we have at a node $n$
a fraction whose numerator and denominator
are coprime to each other, the whole subtree
below $n$ will contain fractions whose
numerator and denominator are coprime to each other.
Therefore, the subtrees below $\frac{3}{2}$ and
$\frac{2}{3}$ will only contain fractions in canonical form.

The fractions that actually have 1 in 
numerator or denominator, however,
will necessarily lead to a fraction
whose numerator and denominator are coprime
to each other, since no number $n$, except $n=1$,
shares any divisor with $n+1$.
Since we start the tree with 
$\frac{1}{1}$, the whole tree can only contain
fractions in canonical form.\qed 

We prove the second property by contradiction.
Let us assume that there are fractions 
that do not appear in the tree. 
Then, there are fractions with 
the smallest denominator and, among those,
there is one with the smallest denominator.
Let $\frac{n}{d}$ be this fraction.
If $n>d$, then $\frac{n-d}{d}$ cannot appear either, since,
$\frac{n}{d}$ would be its right kid.
But that is a contradiction to our assumption that
$\frac{n}{d}$ was the nonappearing fraction
with the smallest numerator among those fractions
with denominator $d$, but, obviously, $n-d$
is an even smaller numerator.
If $n<d$, then $\frac{n}{d-n}$ cannot appear either, since,
$\frac{n}{d}$ would be its left kid.
But that again is a contradiction, since the denominator
is smaller than the denominator of one of the fractions 
we assumed to be those with the smallest denominator.
The only way to solve this dilemma is
to assume that $n$ and $d$ are equal.
Then, indeed, $\frac{n-d}{d} = 0$ and
$\frac{n}{n-d} = \bot$ would not be in the tree.
But such a fraction with $n=d$ is irrelevant,
since it reduces to $\frac{1}{1}$, 
which already is in the tree.\qed

To prove the third property, we first observe
that 1 is the root of the tree. 
Since any fraction below 1 is either
$\frac{n}{n+d}$ or $\frac{n+d}{d}$,
there cannot be a fraction with $n=d$.
With this out of the way, we can argue 
as we did for the second property:
we assume there are fractions that appear
more than once.
From all these fractions, there is a group 
that shares the smallest denominator and,
among this group, one with the smallest numerator.
But this fraction is then either the left kid
of two fractions of the form $\frac{n}{d-n}$,
making the denominator of these fractions even smaller,
or the right kid of two fractions of the form
$\frac{n-d}{d}$, making the numerator of these fractions
even smaller. In both cases with arrive at a contradiction.\qed

We can also prove directly -- but the argument 
may be more subtle or, which is the same,
almost self-evident.
We know that all nodes are of either of the forms
$\frac{n}{n+d}$ or $\frac{n+d}{d}$.
Since, except for the root node, we never have
$n=d$, no node derived from one of those fractions
can ever equal one derived from the other.
To say that two fractions
derived from such nodes are equal, would mean that
we could have two numbers $n$ and $d$, such that
$n=n+d$ and $d=n+d$. That would only work if
$n=0$ and $d=0$. But that case cannot occur.\qed

That taken all together shows that the Calkin-Wilf sequence
contains all fractions exactly once.
Since we can enumerate this sequence, we can 
enumerate all fractions and, hence, $||\mathbb{Q}|| = \aleph_0$.\qed

There is still another advantage of this sequence
over Cantor's original one.
There is a simple, yet quite exciting way
to compute which is the $n^{th}$ fraction.
The key is to realise that we are dealing
with a structure, namely a binary tree, 
that stores sequences of binary decisions.
At any given node in the tree,
we can go either right or left.
We could, therefore, describe 
the $n^{th}$ position as a trajectory
through the tree, where at each node,
we take a binary decision:
going right or going left.
An efficient way to encode a sequence
of binary decisions is a binary number,
and, indeed, the position in the sequence,
represented as a binary number
leads to the fraction at that position.
Here is a function that,
given a natural number $n$,
returns the fraction in the Calkin-Wilf sequence
at position $n$:

\begin{minipage}{\textwidth}
\begin{code}
  calwiR :: Natural -> Rational
  calwiR = go (0 % 1) . toBinary 
    where  go r []              = r
           go r@(Q n d) (0:xs)  = go (n % (n+d)) xs
           go r@(Q n d) (1:xs)  = go ((n+d) % d) xs
\end{code}
\end{minipage}

We start by converting $n$ to binary format.
Then we call |go| starting with 0, since,
for the first number 1, we want position 1.
If we have exhausted the binary number,
the result is just $r$, the rational number we pass
to |go|. Otherwise, we distinguish two cases:
the head of the binary number being 0 or 1.
If it is zero, we follow the left branch,
which has the fraction $\frac{n}{n+d}$
at its top; if it is 1, we follow 
the right branch with the fraction $\frac{n+d}{d}$.

Consider $n=25$ as an example.
25 in binary format is $11001$.
We go through the steps:

|go (Q 0 1) [1,1,0,0,1]  = go (1%1) [1,0,0,1]|\\
|go (Q 1 1) [1,0,0,1]    = go (2%1) [0,0,1]|\\
|go (Q 2 1) [0,0,1]      = go (2%3) [0,1]|\\
|go (Q 2 3) [0,1]        = go (2%5) [1]|\\
|go (Q 2 5) [1]          = go (7%5) []|\\
|go (Q 7 5) []           = Q 7 5|

Let us check if this is true;
|take 1 $ drop 24 $ calWiTree2Seq (calWiTree 5 (1%1))| gives

|[Q 7 5]|,

which corresponds to the correct result $\frac{7}{5}$.
With this we can create the sequence much simpler
without deviating through the tree:

\begin{minipage}{\textwidth}
\begin{code}
  calwis :: [Ratio]
  calwis = map calwiR [1..]
\end{code}
\end{minipage}

We can also do the opposite:
compute the position of any given fraction.
For this, we just have to turn the logic
described above around:

\begin{minipage}{\textwidth}
\begin{code}
  calwiP :: Ratio -> Natural
  calwiP = fromBinary . reverse . go 
    where  go (Q 0 _)  = []
           go (Q n d)  | n >= d     = 1 : go ((n - d) % d)
                       | otherwise  = 0 : go (n % (d-n))
\end{code}
\end{minipage}

Here we do exactly the opposite.
We look at the fraction and, if $n\ge d$,
then we add 1 to the digits of the 
resulting binary number, otherwise,
we add 0.
Note that there is only one case where $n = d$
(as we have proven above), namely the root node
of the tree. In all other cases, we either have
$n>d$ or $n<d$.
In the first case, we know that the fraction
is a right kid and in the second case,
we know it is a left kid.
When we call this function on $\frac{7}{5}$,
we see the steps:

|go (Q 7 5) = 1 : go (2 % 5)|\\
|go (Q 2 5) = 0 : go (2 % 3)|\\
|go (Q 2 3) = 0 : go (2 % 1)|\\
|go (Q 2 1) = 1 : go (1 % 1)|\\
|go (Q 1 1) = 1 : go (0 % 1)|\\
|go (Q 0 1) = []|,

which leads to the list |1:0:0:1:1:[]|.
This evaluated and reversed is |1,1,0,0,1|,
which, converted to decimal representation, is 25.

Surprisingly or not, the Calkin-Wilf sequence
is not completely new. A part of it was already studied
in the $19^{th}$ century by 
German mathematician Moritz Stern (1807 -- 1894),
successor of Gauss at the university of Göttingen
and professor of Bernhard Riemann.
The numerators of the Calkin-Wilf sequence correspond to 
\term{Stern's diatomic sequence}.
Using the Calkin-Wilf sequence,
we can produce Stern's sequence with the function

\begin{minipage}{\textwidth}
\begin{code}
  stern :: [Natural]
  stern = map numerator calwis
    where numerator (Q n _) = n
\end{code}
\end{minipage}

|take 32 stern| shows:

\[
  1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1,
\]

which is equal to the output produced by 
mapping the following function onto |[1..32]|:

\begin{minipage}{\textwidth}
\begin{code}
  fusc :: Natural -> Natural
  fusc 0  = 0
  fusc 1  = 1
  fusc n  | even n     =  fusc (n `div` 2)
          | otherwise  =  let k = (n-1) `div` 2 
                          in fusc k + fusc (k+1)
\end{code}
\end{minipage}

From the definition of the |fusc| function,
we can read some of the many properties
of Stern's sequence.
First, an even number has the same value
as half of that number, for instance
|fusc 3| is 2 and so is |fusc 6|,
|fusc 12|, |fusc 24| and so on.
We immediately see that for any power of 2,
|fusc| equals |fusc 1|, which is 1;
|map fusc [2,4,8,16,32,64,128,256]|, hence,
gives |[1,1,1,1,1,1,1,1]|.

Of course, |fusc n| is the numerator
of the $n^{th}$ fraction in the Calwin-Wilf sequence.
The fraction at position 25, 
as we have seen using |calwiR|, is $\frac{7}{5}$.
|fusc 25| (as well as |fusc 50|, |fusc 100|,
|fusc 200|, \etc) is 7.

An amazing property of |fusc|,
found by Edsgar Dijkstra, is related
to the binary representation
of its argument, namely
that two numbers, whose binary representations
are reverses of each other,
have the same |fusc| result.
25, for instance, is 11001.
When we reverse this sequence, we get 10011,
which, in decimal representation, is 19 and
we have |fusc 19 = fusc 25 = 7|.
Dijkstra's argument is closely related
to his algorithm, which is different
from ours:

\begin{minipage}{\textwidth}
\begin{code}
  fuscd :: Natural -> Natural
  fuscd n = ewd n 1 0
    where  ewd 0 _ b = b
           ewd m a b  | even m     =  ewd (m `div` 2) (a+b) b
                      | otherwise  =  ewd ((m-1) `div` 2) a (b+a)
\end{code}
\end{minipage}

The final result is stored in |b|,
which is returned when |n|, by subsequently
halving, if the intermediate result is even,
and reducing by one and halving, if it is odd,
reaches 0. |b|, initially 0,
carries the value of sums of |a| and |b|.
If we never enter the |odd| branch,
the result would be 0.
If |n| is a power of 2, then we would enter
the |odd| branch only once, namely,
when we reach $m=1$.
The result then is 1.
Every time, we enter the |odd| branch,
$b$ is incremented by the current value of $a$.
The only chance of $a$ to increase beyond 1, however, is 
for the evaluation to enter the |odd| branch at least once.

Dijkstra proposes to represent the two variables
|a| and |b| by a single variable |k|, initialised to 0.
We would represent the sum $a+b$ for the |even| branch as $2k$;
note that, without having entered 
the |odd| branch at least once, this would
not change the value of $k=0$. 
In the |odd| branch, we would represent the sum as $2k+1$,
leading, at its first occurence, to 1.
Afterwards, the value starts to change also
in the |even| branch.
 
We, hence, have two variables,
one running from $n$ down to zero and
the other running from zero upwards
until $n$ reaches zero.
Here is a piece of code to show this behaviour:

\begin{minipage}{\textwidth}
\begin{code}
  fuscd2 :: Natural -> Natural
  fuscd2 n = ewd n 0
    where  ewd 0 k = k
           ewd m k  | even m     =  ewd (m `div` 2) (2*k)
                    | otherwise  =  ewd ((m-1) `div` 2) (2*k+1)
\end{code}
\end{minipage}

When we reach the base case |ewd 0 k|, $k$ is the binary reverse
of $n$, \eg\ |fuscd2 25| is 19 and |fuscd2 19| is 25.

For the Calwin-Wilf tree this means that,
when we have two trajectories through the tree,
where each step after the root node
is the opposite of the simultaneous step
of the other one, we arrive at two fractions
with the same numerator.
The trajectory defined by the binary sequence
$1,1,0,0,1$ leads, first, to the root node $\frac{1}{1}$,
then through 
$\frac{2}{1}$,
$\frac{2}{3}$ and
$\frac{2}{5}$ to
$\frac{7}{5}$.
The reverse of this sequence, $1,0,0,1,1$ leads,
first, to the root node $\frac{1}{1}$ and then through
$\frac{1}{2}$,
$\frac{1}{3}$ and
$\frac{4}{3}$ to
$\frac{7}{3}$.

At this occasion, as we are already looking at the tree,
we should inspect the generation containing 
$\frac{7}{3}$ and 
$\frac{7}{5}$ a bit further.
There are two more fractions with numerator 7,
namely 
$\frac{7}{2}$ and
$\frac{7}{4}$.
What is their position in the sequence?
|calwiP (Q 7 2)| is 23 and |calwiP (Q 7 4)| is 29.
In binary representation 23 is |1,0,1,1,1| and 29
is its reverse |1,1,1,0,1|.
Compare the binary representations of 23 and 25:

\begin{center}
\begin{tabular}{||c||c||c||c||c||c||}
25 & 1 & 1 & 0 & 0 & 1\\\hline
23 & 1 & 0 & 1 & 1 & 1
\end{tabular}
\end{center}

We see that 23 is 25 with all digits, but the
most and least significant ones,
inverted. The same is true for 19 and 29:

\begin{center}
\begin{tabular}{||c||c||c||c||c||c||}
19 & 1 & 0 & 0 & 1 & 1\\\hline
29 & 1 & 1 & 1 & 0 & 1
\end{tabular}
\end{center}

Therefore, we have 
|fusc 19 = fusc 23 = fusc 25 = fusc 29 = 7|.
Interestingly, Dijkstra arrives at the same result
describing the |fuscd| algorithm as a state automaton
and the binary number as a string of the language
it is processing.
We could then say that the Calkin-Wilf tree
is a model that gives meaning to this language.

We see, even further, that there are 4 fractions
in that generation where 7 appears in the denominator,
and, for each denominator of the fractions with 7 in the numerator,
there is also a fraction with this number in the numerator.
That is, for each of these four fraction,
the generation also contains its multiplicative inverse.
When we examine the generation closer,
we see that this holds in fact for all fractions
in that generation. 
When we pair up the fractions with their inverses, we get:

\[
\left(\frac{1}{5}, \frac{5}{1}\right),
\left(\frac{5}{4}, \frac{4}{5}\right),
\left(\frac{4}{7}, \frac{7}{4}\right),
\left(\frac{7}{3}, \frac{3}{7}\right),
\left(\frac{3}{8}, \frac{8}{3}\right),
\left(\frac{8}{5}, \frac{5}{8}\right),
\left(\frac{5}{7}, \frac{7}{5}\right),
\left(\frac{7}{2}, \frac{2}{7}\right).
\]

Indeed, the product of the fractions 
in each generation in the tree
is 1. You can try this out with
the simple function 

\begin{minipage}{\textwidth}
\begin{code}
  genprod :: Natural -> CalwiTree -> Rational
  genprod n t = product (getKids n t)
\end{code}
\end{minipage}

A sequence with so many nice properties,
one might feel to say with some poetic fervour,
cannot be meaningless. 
Isn't there anything in the (more or less) real world
that these numbers would actually count?
It turns out there is.
There are in fact many things the Stern sequence actually does count.
Just to mention two things:
It counts odd binomial coefficients of the form
$\binom{n-r}{r}, 0 \le 2r \le n$.
That is the odd numbers in the first half of the lines $n-r$
in Pascal's triangle.
This would translate to a function 
creating a sequence of numbers of the form

\begin{minipage}{\textwidth}
\begin{code}
  oddCos :: Natural -> [Natural]
  oddCos n = filter odd [choose (n-r) r | r <- [0..(n `div` 2)]]
\end{code}
\end{minipage}

|fusc (n+1)| is exactly the size of that sequence.
For instance:
|oddCos 5| is |[1,3]| and |fusc 6| is 2;
|oddCos 6| is |[1,5,1]| and |fusc 7| is 3;
|oddCos 7| is |[1]| and |fusc 8|, which is a power of 2, is 1;
|oddCos 8| is |[1,7,15,1]| and |fusc 9| is 1;
|oddCos 9| is |[1,21,5]| and |fusc 10| is 3.

Moritz Stern arrived at his sequence,
when studying ways to represent numbers as
powers of 2. Any number can be written
as such a sum and most number even in various ways.
For instance, $2 = 2^0 + 2^0$,
$3 = 2^0 + 2^1 = 2^0 + 2^0 + 2^0$,
$4 = 2^1 + 2^1 = 2^0 + 2^0 + 2^1$,
$5 = 2^0 + 2^2 = 2^0 + 2^1 + 2^1$
and so on.
Stern focussed on so called \term{hyperbinary} systems,
that is sums of powers of 2, where any power appears
at most twice.
For instance, $3 = 2^0 + 2^1$ is such a system,
but $3 = 2^0 + 2^0 + 2^0$ is not.
Stern's sequence counts the number of ways
this is possible for any number $n-1$. In other words, 
|fusc (n+1)| is exactly the number of hyperbinary systems
for $n$.
For 3, as an example, there is only one way
and |fusc 4| is 1;
for 4, there are 3 such systems:
$2^0 + 2^0 + 2^1$, $2^1 + 2^1$ and $2^2$ and
|fusc 5| is 3;
for 5, there are only 2 such systems:
$2^0 + 2^1 + 2^1$ and $2^0 + 2^2$ and
|fusc 6| is 2.

Finding all hyperbinary systems for a number $n$
is quite an interesting problem in its own right.
A brute-force and, hence, inefficient algorithm
could apply the following logic.
We first find all powers of 2 less than or equal to $n$:

\begin{minipage}{\textwidth}
\begin{code}
  pows2 :: Natural -> [Natural]
  pows2 n = takeWhile (<= n) [2^x | x <- [0..]]
\end{code}
\end{minipage}

We then create all permutations of this set
and try to build sums with these permutations 
that equal $n$:

\begin{minipage}{\textwidth}
\begin{code}
  hyperbin :: Natural -> [[Natural]]
  hyperbin n = nub (go $ perms $ pows2 n)
    where  go pss = filter  (\k -> sum k == n) 
                            [sort (sums 0 ps ps) | ps <- pss]
           sums _ [] []      = [] 
           sums s [] ds      = sums s ds []
           sums s (p:ps) ds  | s + p > n   = sums s ds ps
                             | s + p == n  = [p]
                             | otherwise   = p : sums (s+p) ps ds
                                           
\end{code}
\end{minipage}

Note that we pass the available pool of powers of 2 $\le n$
twice to |sums|. When the first instance is exhausted or
$s + p > n$, we start to use the second instance of the pool.
This reflects the fact that we are allowed to use every number
twice.
If the sum $s+p$ equals $n$, we have found a valid
hyperbinary system.
Otherwise, if $s+p < n$, we continue adding the current power
to the result set.
In |go|, we try |sums| on all permutations of the powers
filtering the resulting sets for which |sum| equals $n$.
We sort each list to be able to recognise duplicates and
to remove them with |nub|.
