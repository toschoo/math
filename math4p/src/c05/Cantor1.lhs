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

What about all the integers?
There should be twice as much as natural numbers, right?
Let us see. We first create a set to count the 
natural numbers:

\[
\lbrace(1,1),(2,2),(3,3),\dots\rbrace
\]

Then we insert a negative number before or behind
each positive number:

\[
\lbrace(1,1),(-1,2),(2,3),(-2,4),(3,5),(-3,6),\dots\rbrace
\]

Again, it appears that we do not run out of natural numbers
to count all the integers. The set of integers,
hence, has cardinality $\aleph_0$ too.

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

But now we see that we can enumerate, that is count,
the fractions creating the sequence

\[
 \left(\frac{1}{1},1\right),
 \left(\frac{2}{1},2\right), 
 \left(\frac{1}{2},3\right), 
 \left(\frac{1}{3},4\right), 
 \left(\frac{3}{1},5\right), 
 \left(\frac{4}{1},6\right), 
 \left(\frac{3}{2},7\right), 
 \left(\frac{2}{3},8\right), 
 \left(\frac{1}{4},9\right), 
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
 \left(\frac{1}{1},1\right),
 \left(-\frac{1}{1},2\right),
 \left(\frac{2}{1},3\right), 
 \left(-\frac{2}{1},4\right), 
 \left(\frac{1}{2},5\right), 
 \left(-\frac{1}{2},6\right), 
 \left(\frac{1}{3},7\right), 
 \left(-\frac{1}{3},8\right), 
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
even though there are infinitely many of them already,
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
with $i$ decremented by 1.

This shows that there is a very simple algorithm
to generate the tree.
We will now show that 
Calkin-Wilf tree and Calkin-Wilf sequence 
are equivalent.
We do so by creating an algorithm that 
converts the tree to the sequence.

We may be tempted to do this with a typical
recursive function that does something with the current node
and then adds the result of the operation to
the partial sequences that result from recursively
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
The latter is quite useful,
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
the first column in Cantor's table.
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
Then, there is a number of fractions that have
the smallest denominator and, among those,
there is one with the smallest numerator.
Let $\frac{n}{d}$ be this fraction.
If $n>d$, then $\frac{n-d}{d}$ cannot appear either, since
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
even smaller. In both cases we arrive at a contradiction.\qed

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

We start by converting $n$ to binary format
(\ie\ a list of 0s and 1s).
Then we call |go| starting with 0, since,
for the first number 1, we want position 1.
If we have exhausted the binary number,
the result is just $r$, the rational number we pass
to |go|. Otherwise, we distinguish two cases:
the head of the binary number being 0 or 1.
If it is 0, we follow the left branch,
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

\begin{minipage}{\textwidth}
|go (Q 7 5) = 1 : go (2 % 5)|\\
|go (Q 2 5) = 0 : go (2 % 3)|\\
|go (Q 2 3) = 0 : go (2 % 1)|\\
|go (Q 2 1) = 1 : go (1 % 1)|\\
|go (Q 1 1) = 1 : go (0 % 1)|\\
|go (Q 0 1) = []|,
\end{minipage}

which leads to the list |1:0:0:1:1:[]|.
This evaluated and reversed is |1,1,0,0,1|,
which, converted to decimal representation, is 25.

Surprisingly or not, the Calkin-Wilf sequence
is not completely new. A part of it was already studied
in the $19^{th}$ century by 
German mathematician Moritz Stern (1807 -- 1894),
successor of the early deceased sucessor of Gauss
at the University of Göttingen, Lejeune-Dirichlet, 
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
  1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1.
\]

Mapping |denominator| defined as |denominator (Q _ d) = d|
on the Calkin-Wilf sequence would give a very similar result:
the Stern sequence one ahead, \ie:

\[
  1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3,8,5,7,2,7,5,8,3,7,4,5,1,6.
\]

Edsgar Dijkstra, the great pioneer of the art of computer programming,
studied this sequence not knowing that it had already been studied before. 
He called it the \term{fusc} sequence and generated it with
the function

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
of Stern's sequence (and the Calkin-Wilf tree).
First, an even number has the same value
as half of that number, for instance
|fusc 3| is 2 and so is |fusc 6|,
|fusc 12|, |fusc 24| and so on.
Even numbers in binary format
end on zeros. For instance,
3 in binary notation is 11.
$2 \times 3$ is 110,
$2 \times 6$ is 1100,
$2 \times 12$ is 11000 and so on.
The binary format clearly indicates that, after having
reached the number before the trail of zeros
at the end, we go down in a straight line
following the left branch of that node
in the Calkin-Wilf tree.
Since, in the left path, the numerator
never changes, the result of 
$fusc(n)$ equals the result of $fusc(2n)$.

We also see that for any power of 2,
|fusc| equals |fusc 1|, which is 1;
|map fusc| |[2,4,8,16,32,| |64,128,256]|, hence,
gives |[1,1,1,1,1,1,1,1]|.
Note that, looking at the Calkin-Wilf tree,
this is immediate obvious, since powers of 2
in binary representation are numbers of the form
$1,10,100,1000,\dots$
Those numbers indicate that 
we navigate through the tree in a straight line
following the left branch of the root node $\frac{1}{1}$.

The |fusc| results of powers of two minus one ($1,11,111,1111,\dots$)
equal the number of digits of this number in binary form.
This is the right outer branch
of the tree with the integers.

The |fusc| results of powers of two plus one ($1,11,101,1001,\dots$)
also equal the number of digits
in the binary representation of that number.
These numerators appear in the immediate neighbours
of the powers of two in the left outer branch of the tree,
for instance 
$\frac{3}{2},
 \frac{4}{3},
 \frac{5}{4},
 \frac{6}{5}, \dots$

What about numbers with an alternating sequence
of 1s and 0s, like 101010101?
Those numbers are not in the outer branches
and not even close to them. Indeed, they 
tend to the horizontal centre of the tree.
The first 1 leads to node $\frac{1}{1}$.
We now go left, that is,
we add the numerator to the denominator leading
to $\frac{1}{2}$; we then add the denominator
to the numerator leading to $\frac{3}{2}$;
then we add the numerator to the denominator again
leading to $\frac{3}{5}$ 
and so we go on and obtain the fractions
$\frac{8}{5},
 \frac{8}{13},
 \frac{21}{13},
 \frac{21}{34},
 \frac{55}{34},\dots$
Do you see the point?
All the numerators and denominators are Fibonacci numbers!
Well, what we did above,
adding the two numbers we obtained before
starting with the pair $(1,1)$,
is just the recipe
to create the Fibonacci numbers.

An amazing property of |fusc|,
found by Dijkstra, is the fact that
two numbers whose binary representations
are the reverse of each other
have the same |fusc| result. 
25, for instance, is 11001.
The reverse, 10011, is 19,
and |fusc 19 = fusc 25 = 7|.

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

A similar is true for two numbers,
whose binary sequences can be transformed
into one another by inverting the inner bits.
For instance, 11001, 25, can be transformed into
10111, inverting all bits, but the first and the last one.
10111 is 23 and |fusc 19 = fusc 23 = fusc 25|.
What about the bit inverse of 19?
That is 11101, the reverse of 10111 and 29 in decimal notation.
Therefore |fusc 19 = fusc 23 = fusc 25 = fusc 29|.

We can reformulate this result in terms of group theory.
We have three basic transformations $i$, the identity,
$\rho$, the reverse, and $\beta$, the bit inverse.
We add one more transformation, the composition of
$\rho$ and $\beta$ and call it $\sigma = \rho \cdot \beta$.
The operation defined over this set is composition.
We see that the identity is part of the set;
for each transformation, its inverse is in the set, too,
because $\rho \cdot \rho = i$, $\beta \cdot \beta = i$ and
$\sigma \cdot \sigma = i$.
To illustrate the logic of this group with the numbers above,
we define it on the base string 19, which is 10011:

$i = 10011$\\
$\rho = 11001$\\
$\beta = 11101$\\
$\sigma = 10111$.

Now we can play around and see that we will
never generate a string that is not already 
in the group:

$\rho  \cdot i     = 11001$\\
$\rho  \cdot \rho  = i = 10011$\\
$\beta \cdot \beta = i = 10011$\\
$\sigma \cdot \sigma = i = 10011$\\
$\rho  \cdot \beta = \sigma = 10111$\\
$\beta \cdot \rho  = \sigma = 10111$\\
$\sigma \cdot \rho  = \beta = 11101$\\
$\sigma \cdot \beta = \rho  = 11001$\\
$\dots$

All elements of one group are in the same generation
of the Calkin-Wilf tree,
since they all have the same number of digits.
Numbers with a symmetric binary representation,
such that $\rho = i$, lead to groups with only 
two distinguishable members, for instance
$fusc(2^n+1) = fusc(2^{n+1}-1)$.
The same is true for numbers with a binary representation
such that $\rho = \beta$, for instance, 
101011 (43) = 110101 (53) and $fusc(43) = fusc(53) = 13$.

There are infinitely many numbers
with the same |fusc| result.
Most of these numbers have trailing zeros
and, as such, are in the long shadow thrown 
by one of the original odd numbers with the same result.
One example of such a shadow is the outer left branch,
which maintains the numerator of the root node $\frac{1}{1}$
and also maintains the leading 1 in the binary representation
of its positions, merely adding more and more 0s to it.

How many ``original'' numbers in this sense are there
for a given |fusc| result $n$?
The answer is simple if we consider two facts:
1. The fractions in the Calkin-Wilf tree are in
canonical form, \ie\ numerator and denominator
do not share divisors, and
2. Any position number whose binary representation
ends with 1, points to a right kid and, for all fractions
$\frac{n}{d}$ that are right kids:
$n > d$. Binary numbers, however, that end with 0,
point to a left kid and, therefore, $n < d$.
In other words, the number of original numbers
for a given numerator $n$ is $\varphi(n)$, the totient number of $n$.
The denominators of the original fractions are 
the coprimes of $n$.

The numerator 7, for instance, appears in six positions:
19, 23, 25, 29, 65 and 127.
The denominators of the fractions at those positions are
3, 2, 5, 4, 6 and 1.
For numerator 8, there are only four such numbers:
21, 27, 129 and 255.
The denominators at those positions are 5, 3, 7 and 1.
Note that 21 in binary format is 10101, 
which is its own reverse,
and 27 is the bit inverse of 21, namely 11011,
which also is its own reverse.

Furthermore, those numbers appear in groups
with 2 or 4 members, depending on the properties 
of the binary representation. The number of such groups,
hence, is $\frac{\varphi(n)}{k}$, 
where $k$ is some integer that divides $\varphi(n)$.
For 7: $k=3$, since there are two groups,
one containing 4 elements, the other containing 2.
For 8: $k=2$, since there are two groups, 
both containing 2 elements.

\ignore{
The $k$ values for the numbers 1-20 are

1,1,2,2,2,2,3,2,3,2,5,2,4,6,4,4,5,6,4,4

which, up to my knowledge, is not a known integer sequence.
}

The last group is the one consisting of
binary numbers with $n$ bits, \ie\ $2^{n-1}+1$ and $2^n-1$.
The other groups appear in generations of the Calkin-Wilf tree
before the generation with that final group.
For 7, the generation of the group
with four members is the fifth generation and
the generation with the final group is of course the seventh generation.
In other cases,
the groups can be many generations apart.
The numerator 55, for instance, appears for the first time
in generation 10, namely in the fraction $\frac{55}{34}$
(both Fibonacci numbers).
This is far off from generation 55 
with the group consisting of $\frac{55}{1}$ and $\frac{55}{54}$.

Interestingly, Dijkstra was not aware of the relation
of the |fusc| algorithm to the Stern sequence, and
the Calkin-Wilf tree was not even around at that time.
Dijkstra describes |fusc| as a state automaton that
parses strings consisting of 1s and 0s.
The parsing result would be a number, 
namely the result of |fusc|.
We could now say that the Calkin-Wilf tree
is a model that gives meaning to the strings
in terms of trajectories through the tree.

A final remark relates to the product of one generation in the tree.
Each generation consists of fractions 
whose numerators and denominators were created
by adding the numerators and denominators 
of the fractions of the previous generation.
We start with the fraction $\frac{1}{1}$.
In consequence, in any generation, 
there is for any fraction $\frac{n}{d}$ 
a fraction $\frac{d}{n}$.
The fractions in the fifth generation for example,
the one containing the fractions 
at positions 19, 23, 25 and 29 in the Calkin-Wilf sequence,
can be paired up in the following way:

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

The product of each pair is 1.
The product of all fractions 
in one generation is therefore 1 as well.
You can try this out with
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
There are in fact many things the Stern sequence actually counts.
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
|oddCos 8| is |[1,7,15,1]| and |fusc 9| is 4;
|oddCos 9| is |[1,21,5]| and |fusc 10| is 3.

Moritz Stern arrived at his sequence,
when studying ways to represent numbers as
powers of 2. Any number can be written
as such a sum and most numbers even in various ways.
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
and try to build sums 
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
We sort each list to make lists with equal elements equal 
and to, thus, be able to recognise duplicates and
to remove them with |nub|.
